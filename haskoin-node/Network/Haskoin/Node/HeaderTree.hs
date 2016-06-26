{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Network.Haskoin.Node.HeaderTree
( BlockChainAction(..)
, BlockHeight
, NodeBlock
, Timestamp
, initHeaderTree
, migrateHeaderTree
, getBestBlock
, getHeads
, getBlockByHash
, getParentBlock
, getBlockWindow
, getBlockAfterTime
, getChildBlocks
, getBlockByHeight
, getBlocksByHeight
, getBlocksFromHeight
, getBlocksAtHeight
, putBlock
, putBlocks
, genesisBlock
, splitBlock
, splitChains
, nodeBlock
, nodeBlockHeight
, nodeHash
, nodeHeader
, nodePrev
, nodeTimestamp
, isBestChain
, isChainReorg
, isSideChain
, isKnownChain
, connectHeader
, connectHeaders
, blockLocator
, pruneChain
) where

import           Control.Monad                         (foldM, forM, unless,
                                                        when, (<=<))
import           Control.Monad.State                   (evalStateT, get, put)
import           Control.Monad.Trans                   (MonadIO, lift)
import           Control.Monad.Trans.Either            (EitherT, left,
                                                        runEitherT)
import           Data.Bits                             (shiftL)
import qualified Data.ByteString                       as BS (reverse, take)
import           Data.Function                         (on)
import           Data.List                             (find, maximumBy, sort)
import           Data.Maybe                            (fromMaybe, isNothing,
                                                        listToMaybe, mapMaybe)
import           Data.Serialize                        (decode, encode)
import           Data.String.Conversions               (cs)
import           Data.Word                             (Word32)
import           Database.Esqueleto                    (Esqueleto, Value, asc,
                                                        delete, from, groupBy,
                                                        in_, insertMany_, limit,
                                                        max_, not_, orderBy,
                                                        select, set, unValue,
                                                        update, val, valList,
                                                        where_, (!=.), (&&.),
                                                        (<=.), (=.), (==.),
                                                        (>.), (>=.), (^.),
                                                        (||.))
import           Database.Persist                      (Entity (..), insert_)
import           Database.Persist.Sql                  (SqlPersistT)
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.Checkpoints
import           Network.Haskoin.Node.HeaderTree.Model
import           Network.Haskoin.Node.HeaderTree.Types
import           Network.Haskoin.Util

data BlockChainAction
    = BestChain  { actionNodes :: ![NodeBlock] }
    | ChainReorg { actionSplitNode :: !NodeBlock
                 , actionOldNodes  :: ![NodeBlock]
                 , actionNodes     :: ![NodeBlock]
                 }
    | SideChain  { actionNodes :: ![NodeBlock] }
    | KnownChain { actionNodes :: ![NodeBlock] }
    deriving (Show, Eq)

type MinWork = Word32

shortHash :: BlockHash -> ShortHash
shortHash = fromRight . decode . BS.take 8 . getHash256 . getBlockHash

nodeHeader :: NodeBlock -> BlockHeader
nodeHeader = getNodeHeader . nodeBlockHeader

nodeHash :: NodeBlock -> BlockHash
nodeHash = headerHash . nodeHeader

nodePrev :: NodeBlock -> BlockHash
nodePrev = prevBlock . nodeHeader

nodeTimestamp :: NodeBlock -> Timestamp
nodeTimestamp = blockTimestamp . nodeHeader

-- | Number of blocks on average between difficulty cycles (2016 blocks).
diffInterval :: Word32
diffInterval = targetTimespan `div` targetSpacing

-- | Genesis block.
genesisBlock :: NodeBlock
genesisBlock = NodeBlock
    { nodeBlockHash          = shortHash $ headerHash genesisHeader
    , nodeBlockHeader        = NodeHeader genesisHeader
    , nodeBlockWork          = 1.0
    , nodeBlockHeight        = 0
    , nodeBlockChain         = 0
    }

-- | Initialize the block header chain by inserting the genesis block if it
-- doesn't already exist.
initHeaderTree :: MonadIO m => SqlPersistT m ()
initHeaderTree = do
    nodeM <- getBlockByHash $ headerHash genesisHeader
    when (isNothing nodeM) $ putBlock genesisBlock

getVerifyParams
    :: MonadIO m
    => BlockHeader
    -> EitherT String (SqlPersistT m)
               (NodeBlock, [Timestamp], Timestamp, Word32, Maybe Word32)
getVerifyParams bh = do
    parentM <- lift $ getBlockByHash $ prevBlock bh
    parent <- maybe (left "Could not get parent node") return parentM
    checkPointM <- fmap nodeBlockHeight <$> lift lastSeenCheckpoint
    diffBlockM <- lift $ getBlockByHeight parent $
        nodeBlockHeight parent `div` diffInterval * diffInterval
    diffTime <- maybe (left "Could not get difficulty change block")
        (return . nodeTimestamp)
        diffBlockM
    medianBlocks <- lift $ map nodeTimestamp <$>
        getBlocksFromHeight parent 11 (min 0 $ nodeBlockHeight parent - 10)
    minWork <- lift $ findMinWork parent
    return (parent, medianBlocks, diffTime, minWork, checkPointM)

findMinWork :: MonadIO m => NodeBlock -> SqlPersistT m MinWork
findMinWork bn
    | isMinWork bn = return $ blockBits $ nodeHeader bn
    | otherwise = getParentBlock bn >>=
        maybe (return $ blockBits $ nodeHeader bn) findMinWork

isMinWork :: NodeBlock -> Bool
isMinWork bn
    | not allowMinDifficultyBlocks = True
    | nodeBlockHeight bn `mod` diffInterval == 0 = True
    | blockBits (nodeHeader bn) /= encodeCompact powLimit = True
    | otherwise = False

splitKnown :: MonadIO m
           => [BlockHeader]
           -> SqlPersistT m ([NodeBlock], [BlockHeader])
splitKnown hs = do
    (kno, unk) <- foldM f ([], []) hs
    return (reverse kno, reverse unk)
  where
    f (kno, []) n = do
        bnM <- getBlockByHash (headerHash n)
        case bnM of
            Nothing -> return (kno, [n])
            Just bn -> return (bn:kno, [])
    f (kno, unk) n = return (kno, n:unk)

-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectHeader :: MonadIO m
              => NodeBlock
              -> BlockHeader
              -> Timestamp
              -> SqlPersistT m (Either String BlockChainAction)
connectHeader best bh ts = runEitherT $ do
    (kno, _) <- lift $ splitKnown [bh]
    case kno of
        [] -> do
            (parent, medians, diffTime, minWork, cpM) <- getVerifyParams bh
            chain <- lift $ getChain parent
            let bn = nodeBlock parent chain bh
            liftEither $
                verifyBlockHeader parent medians diffTime cpM minWork ts bh
            lift $ putBlock bn
            lift $ evalNewChain best [bn]
        _ -> return $ KnownChain kno

-- | A more efficient way of connecting a list of block headers than connecting
-- them individually. The list of block headers have must form a valid chain.
connectHeaders :: MonadIO m
               => NodeBlock
               -> [BlockHeader]
               -> Timestamp
               -> SqlPersistT m (Either String BlockChainAction)
connectHeaders _ [] _ = runEitherT $ left "Nothing to connect"
connectHeaders best bhs ts = runEitherT $ do
    unless (validChain bhs) $ left "Block headers do not form a valid chain"
    (kno, unk) <- lift $ splitKnown bhs
    case unk of
        [] -> return $ KnownChain kno
        (bh:_) -> do
            (parent, medians, diffTime, minWork, cpM) <- getVerifyParams bh
            chain <- lift $ getChain parent
            nodes <- (`evalStateT` (parent, diffTime, medians, minWork)) $
                forM unk $ \b -> do
                    (p, d, ms, mw) <- get
                    lift $ liftEither $ verifyBlockHeader p ms d cpM mw ts b
                    let bn = nodeBlock p chain b
                        d' = if nodeBlockHeight bn `mod` diffInterval == 0
                             then blockTimestamp b
                             else d
                        ms' = blockTimestamp b : if length ms == 11
                                                 then tail ms
                                                 else ms
                        mw' = if isMinWork bn then blockBits b else mw
                    put (bn, d', ms', mw')
                    return bn
            lift $ putBlocks nodes
            lift $ evalNewChain best nodes
  where
    validChain (a:b:xs) = prevBlock b == headerHash a && validChain (b:xs)
    validChain [_] = True
    validChain _ = False

-- | Returns True if the action is a best chain.
isBestChain :: BlockChainAction -> Bool
isBestChain (BestChain _) = True
isBestChain _             = False

-- | Returns True if the action is a chain reorg.
isChainReorg :: BlockChainAction -> Bool
isChainReorg ChainReorg{} = True
isChainReorg _            = False

-- | Returns True if the action is a side chain.
isSideChain :: BlockChainAction -> Bool
isSideChain (SideChain _) = True
isSideChain _             = False

-- | Returns True if the action is a known chain.
isKnownChain :: BlockChainAction -> Bool
isKnownChain (KnownChain _) = True
isKnownChain _              = False

-- | Returns a BlockLocator object for a given block hash.
blockLocator :: MonadIO m => NodeBlock -> SqlPersistT m BlockLocator
blockLocator node = do
    nodes <- getBlocksByHeight node bs
    return $ map nodeHash nodes
  where
    h = nodeBlockHeight node
    f x s = (fst x - s, fst x > s)
    bs = (++ [0]) $ map fst $ takeWhile snd $
        [(h - x, x < h) | x <- [0..9]] ++
        scanl f (h - 10, h > 10) [2 ^ (x :: Word32) | x <- [1..]]

-- | Verify block header conforms to protocol.
verifyBlockHeader :: NodeBlock        -- ^ Parent block header
                  -> [Timestamp]      -- ^ Timestamps of previous 11 blocks
                  -> Timestamp        -- ^ Previous difficulty change
                  -> Maybe Word32     -- ^ Height of most recent checkpoint
                  -> MinWork          -- ^ Last MinWork (e.g. Testnet3)
                  -> Timestamp        -- ^ Current time
                  -> BlockHeader      -- ^ Block header to validate
                  -> Either String ()
-- TODO: Add DOS return values
verifyBlockHeader par mts dt cp mw ts bh = do
    unless (isValidPOW bh) $
        Left "Invalid proof of work"

    unless (blockTimestamp bh <= ts + 2 * 60 * 60) $
        Left "Invalid header timestamp"

    let nextWork = nextWorkRequired par dt mw bh
    unless (blockBits bh == nextWork) $
        Left "Incorrect work transition (bits)"

    let sortedMedians = sort mts
        medianTime    = sortedMedians !! (length sortedMedians `div` 2)
    when (blockTimestamp bh <= medianTime) $
        Left "Block timestamp is too early"

    let newHeight = nodeBlockHeight par + 1
    unless (maybe True (fromIntegral newHeight >) cp) $
        Left "Rewriting pre-checkpoint chain"

    unless (verifyCheckpoint (fromIntegral newHeight) (headerHash bh)) $
        Left "Rejected by checkpoint lock-in"

    -- All block of height 227836 or more use version 2 in prodnet
    -- TODO: Find out the value here for testnet
    when (networkName == "prodnet"
          && blockVersion bh == 1
          && nodeBlockHeight par + 1 >= 227836) $
        Left "Rejected version 1 block"

-- | Create a block node data structure from a block header.
nodeBlock :: NodeBlock    -- ^ Parent block node
          -> Word32       -- ^ Chain number for new node
          -> BlockHeader
          -> NodeBlock
nodeBlock parent chain bh = NodeBlock
    { nodeBlockHash              = shortHash $ headerHash bh
    , nodeBlockHeader            = NodeHeader bh
    , nodeBlockWork              = newWork
    , nodeBlockHeight            = height
    , nodeBlockChain             = chain
    }
  where
    newWork = nodeBlockWork parent + fromIntegral
        (headerWork bh `div` headerWork genesisHeader)
    height = nodeBlockHeight parent + 1

-- | Return blockchain action to connect given block with best block. Count will
-- limit the amount of blocks building up from split point towards the best
-- block.
getBlockWindow :: MonadIO m
               => NodeBlock  -- ^ Best block
               -> NodeBlock  -- ^ Start of window
               -> Word32     -- ^ Window count
               -> SqlPersistT m BlockChainAction
getBlockWindow best node cnt = do
    (_, old, new) <- splitChains (node, 0) (best, cnt)
    return $ if null old then BestChain new else ChainReorg node old new

-- | Find the split point between two nodes. It also returns the two partial
-- chains leading from the split point to the respective nodes. Tuples must
-- contain a block node and the count of nodes that should be returned from the
-- split towards that block. 0 means all.
splitChains :: MonadIO m
            => (NodeBlock, Word32)
            -> (NodeBlock, Word32)
            -> SqlPersistT m (NodeBlock, [NodeBlock], [NodeBlock])
splitChains (l, ln) (r, rn) = do
    sn <- splitBlock l r
    (split:ls) <- getBlocksFromHeight l ln (nodeBlockHeight sn)
    rs         <- getBlocksFromHeight r rn (nodeBlockHeight sn + 1)
    return (split, ls, rs)

-- | Finds the parent of a block.
getParentBlock :: MonadIO m
               => NodeBlock
               -> SqlPersistT m (Maybe NodeBlock)
getParentBlock node
    | nodeBlockHeight node == 0 = return Nothing
    | otherwise = getBlockByHash p
  where
    p = nodePrev node

-- | Get all children for a block
getChildBlocks :: MonadIO m
              => BlockHash
              -> SqlPersistT m [NodeBlock]
getChildBlocks h = do
    ch <- (+1) . nodeBlockHeight . fromMaybe e <$> getBlockByHash h
    filter ((==h) . nodePrev) <$> getBlocksAtHeight ch
  where
    e = error $ "Cannot find block hash " ++ cs (blockHashToHex h)


-- | Get the last checkpoint that we have seen.
lastSeenCheckpoint :: MonadIO m
                   => SqlPersistT m (Maybe NodeBlock)
lastSeenCheckpoint =
    fmap listToMaybe $ getBlocksByHash $ map snd $ reverse checkpointList

-- | Returns the work required for a block header given the previous block. This
-- coresponds to bitcoind function GetNextWorkRequired in main.cpp.
nextWorkRequired :: NodeBlock
                 -> Timestamp
                 -> MinWork
                 -> BlockHeader
                 -> Word32
nextWorkRequired par ts mw bh
    -- Genesis block
    | nodeBlockHeight par == 0 = encodeCompact powLimit
    -- Only change the difficulty once per interval
    | (nodeBlockHeight par + 1) `mod` diffInterval /= 0 =
        if allowMinDifficultyBlocks
            then minPOW
            else blockBits $ nodeHeader par
    | otherwise = workFromInterval ts (nodeHeader par)
  where
    delta = targetSpacing * 2
    minPOW
        | blockTimestamp bh > nodeTimestamp par + delta = encodeCompact powLimit
        | otherwise = mw

-- | Computes the work required for the next block given a timestamp and the
-- current block. The timestamp should come from the block that matched the
-- last jump in difficulty (spaced out by 2016 blocks in prodnet).
workFromInterval :: Timestamp -> BlockHeader -> Word32
workFromInterval ts lastB
    | newDiff > powLimit = encodeCompact powLimit
    | otherwise          = encodeCompact newDiff
  where
    t = fromIntegral $ blockTimestamp lastB - ts
    actualTime
        | t < targetTimespan `div` 4 = targetTimespan `div` 4
        | t > targetTimespan * 4     = targetTimespan * 4
        | otherwise                  = t
    lastDiff = decodeCompact $ blockBits lastB
    newDiff = lastDiff * toInteger actualTime `div` toInteger targetTimespan

-- | Returns True if the difficulty target (bits) of the header is valid and the
-- proof of work of the header matches the advertised difficulty target. This
-- function corresponds to the function CheckProofOfWork from bitcoind in
-- main.cpp.
isValidPOW :: BlockHeader -> Bool
isValidPOW bh
    | target <= 0 || target > powLimit = False
    | otherwise = headerPOW bh <= fromIntegral target
  where
    target = decodeCompact $ blockBits bh

-- | Returns the proof of work of a block header as an Integer number.
headerPOW :: BlockHeader -> Integer
headerPOW =  bsToInteger . BS.reverse . encode . headerHash

-- | Returns the work represented by this block. Work is defined as the number
-- of tries needed to solve a block in the average case with respect to the
-- target.
headerWork :: BlockHeader -> Integer
headerWork bh =
    fromIntegral $ largestHash `div` (target + 1)
  where
    target      = decodeCompact (blockBits bh)
    largestHash = 1 `shiftL` 256

{- Persistent backend -}

chainPathQuery :: forall (expr :: * -> *) (query :: * -> *) backend.
                  Esqueleto query expr backend
               => expr (Entity NodeBlock)
               -> [NodeBlock]
               -> expr (Value Bool)
chainPathQuery _ [] = error "Monsters, monsters everywhere"

chainPathQuery t [NodeBlock{..}] =
    t ^. NodeBlockHeight <=. val nodeBlockHeight &&.
    t ^. NodeBlockChain ==. val nodeBlockChain

chainPathQuery t (n1:bs@(n2:_)) = chainPathQuery t bs ||.
    (   t ^. NodeBlockHeight <=. val (nodeBlockHeight n1)
    &&. t ^. NodeBlockHeight >.  val (nodeBlockHeight n2)
    &&. t ^. NodeBlockChain  ==. val (nodeBlockChain  n1)
    )

getHeads :: MonadIO m => SqlPersistT m [NodeBlock]
getHeads = fmap (map (entityVal . snd)) $ select $ from $ \t -> do
    groupBy $ t ^. NodeBlockChain
    return (max_ (t ^. NodeBlockHeight), t)

-- | Chain for new block building on a parent node
getChain :: MonadIO m
         => NodeBlock    -- ^ Parent node
         -> SqlPersistT m Word32
getChain parent = do
    maxHeightM <- fmap (unValue <=< listToMaybe) $ select $ from $ \t -> do
        where_ $ t ^. NodeBlockChain ==. val (nodeBlockChain parent)
        return $ max_ $ t ^. NodeBlockHeight
    let maxHeight = fromMaybe (error "That chain does not exist") maxHeightM
    if maxHeight == nodeBlockHeight parent
        then return $ nodeBlockChain parent
        else do
            maxChainM <- fmap (unValue <=< listToMaybe) $ select $ from $ \t ->
                return $ max_ $ t ^. NodeBlockChain
            let maxChain = fromMaybe (error "Ran out of chains") maxChainM
            return $ maxChain + 1

getPivots :: MonadIO m => NodeBlock -> SqlPersistT m [NodeBlock]
getPivots = go []
  where
    go acc b
        | nodeBlockChain b == 0 = return $ genesisBlock : b : acc
        | otherwise = do
            l <- fromMaybe (error "Houston, we have a problem") <$>
                getChainLowest b
            c <- fromMaybe (error "Ground Control to Major Tom") <$>
                getParentBlock l
            go (b:acc) c

getChainLowest :: MonadIO m => NodeBlock -> SqlPersistT m (Maybe NodeBlock)
getChainLowest nb = fmap (listToMaybe . map entityVal) $
    select $ from $ \t -> do
        where_ $ t ^. NodeBlockChain ==. val (nodeBlockChain nb)
        orderBy [ asc $ t ^. NodeBlockHeight ]
        limit 1
        return t

-- | Get node height and chain common to both given.
splitBlock :: MonadIO m
           => NodeBlock
           -> NodeBlock
           -> SqlPersistT m NodeBlock
splitBlock l r = if nodeBlockChain l == nodeBlockChain r
    then if nodeBlockHeight l < nodeBlockHeight r
        then return l
        else return r
    else do
        pivotsL <- getPivots l
        pivotsR <- getPivots r
        let ns = zip pivotsL pivotsR
            f (x,y) = nodeBlockChain x == nodeBlockChain y
            (one, two) = last $ takeWhile f ns
        if nodeBlockHeight one < nodeBlockHeight two
            then return one
            else return two

-- | Put single block in database.
putBlock :: MonadIO m => NodeBlock -> SqlPersistT m ()
putBlock = insert_

-- | Put multiple blocks in database.
putBlocks :: MonadIO m => [NodeBlock] -> SqlPersistT m ()
putBlocks = mapM_ insertMany_ . f
  where
    f [] = []
    f xs = let (xs',xxs) = splitAt 50 xs in xs' : f xxs

getBestBlock :: MonadIO m => SqlPersistT m NodeBlock
getBestBlock =
    maximumBy (compare `on` nodeBlockWork) <$> getHeads

getBlockByHash :: MonadIO m => BlockHash -> SqlPersistT m (Maybe NodeBlock)
getBlockByHash h =
    fmap (listToMaybe . map entityVal) $ select $ from $ \t -> do
        where_ $ t ^. NodeBlockHash ==. val (shortHash h)
        return t

-- | Get multiple blocks corresponding to given hashes
getBlocksByHash :: MonadIO m
                => [BlockHash]
                -> SqlPersistT m [NodeBlock]
getBlocksByHash hashes = do
    nodes <- fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ t ^. NodeBlockHash `in_` valList (map shortHash hashes)
        return t
    return $ mapMaybe
        (\h -> find ((== shortHash h) . nodeBlockHash) nodes)
        hashes

-- | Get ancestor of specified block at given height.
getBlockByHeight :: MonadIO m
                  => NodeBlock     -- ^ Best block
                  -> BlockHeight
                  -> SqlPersistT m (Maybe NodeBlock)
getBlockByHeight block height = do
    forks <- reverse <$> getPivots block
    fmap (listToMaybe . map entityVal) $ select $ from $ \t -> do
        where_ $ chainPathQuery t forks &&.
            t ^. NodeBlockHeight ==. val height
        return t

-- | Get ancestors for specified block at given heights.
getBlocksByHeight :: MonadIO m
                  => NodeBlock       -- ^ Best block
                  -> [BlockHeight]
                  -> SqlPersistT m [NodeBlock]
getBlocksByHeight best heights = do
    forks <- reverse <$> getPivots best
    nodes <- fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ chainPathQuery t forks &&.
            t ^. NodeBlockHeight `in_` valList heights
        return t
    return $ mapMaybe (\h -> find ((==h) . nodeBlockHeight) nodes) heights

-- | Get a range of block headers building up to specified block. If
-- specified height is too large, an empty list will be returned.
getBlocksFromHeight :: MonadIO m
                    => NodeBlock     -- ^ Best block
                    -> Word32        -- ^ Count (0 for all)
                    -> BlockHeight   -- ^ Height from (including)
                    -> SqlPersistT m [NodeBlock]
getBlocksFromHeight block cnt height = do
    forks <- reverse <$> getPivots block
    fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ chainPathQuery t forks &&.
            t ^. NodeBlockHeight >=. val height
        when (cnt > 0) $ limit $ fromIntegral cnt
        return t

-- | Get node immediately at or after timestamp in main chain.
getBlockAfterTime :: MonadIO m => Timestamp -> SqlPersistT m (Maybe NodeBlock)
getBlockAfterTime ts = do
    n@NodeBlock{..} <- getBestBlock
    f genesisBlock n
  where
    f l r | nodeTimestamp r < ts =
              return Nothing
          | nodeTimestamp l >= ts =
              return $ Just l
          | (nodeBlockHeight r - nodeBlockHeight l) `div` 2 == 0 =
              return $ Just r
          | otherwise = do
              let rh = nodeBlockHeight r
                  lh = nodeBlockHeight l
                  mh = rh - (rh - lh) `div` 2
              m <- fromMaybe (error "My God, it’s full of stars!") <$>
                  getBlockByHeight r mh
              if nodeTimestamp m > ts then f l m else f m r

-- | Get blocks at specified height in all chains.
getBlocksAtHeight :: MonadIO m => BlockHeight -> SqlPersistT m [NodeBlock]
getBlocksAtHeight height = fmap (map entityVal) $ select $ from $ \t -> do
      where_ $ t ^. NodeBlockHeight ==. val height
      return t

-- | Evaluate block action for provided best block and chain of new blocks.
evalNewChain :: MonadIO m
             => NodeBlock
             -> [NodeBlock]
             -> SqlPersistT m BlockChainAction
evalNewChain _ [] = error "You find yourself in the dungeon of missing blocks"
evalNewChain best newNodes
    | buildsOnBest = do
        return $ BestChain newNodes
    | nodeBlockWork (last newNodes) > nodeBlockWork best = do
        (split, old, new) <- splitChains (best, 0) (head newNodes, 0)
        return $ ChainReorg split old (new ++ tail newNodes)
    | otherwise = do
        (split, _, new) <- splitChains (best, 0) (head newNodes, 0)
        case new of
            [] -> return $ KnownChain newNodes
            _  -> return $ SideChain $ split : new ++ tail newNodes
  where
    buildsOnBest = nodePrev (head newNodes) == nodeHash best

-- | Remove all other chains from database and return updated best block node.
pruneChain :: MonadIO m
           => NodeBlock
           -> SqlPersistT m NodeBlock
pruneChain best = if (nodeBlockChain best == 0) then return best else do
    forks <- reverse <$> getPivots best
    delete $ from $ \t -> where_ $ not_ (chainPathQuery t forks)
    update $ \t -> do
        set t [ NodeBlockChain =. val 0 ]
        where_ $ t ^. NodeBlockHeight <=. val (nodeBlockHeight best)
              &&. t ^. NodeBlockChain  !=. val 0
    return best{ nodeBlockChain = 0 }
