{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Network.Haskoin.Node.HeaderTree
( BlockChainAction(..)
, genesisBlock
, initHeaderTree
, connectHeader
, connectHeaders
, isBestChain
, isChainReorg
, isSideChain
, isKnownChain
, blockLocator
, verifyBlockHeader
, nodeBlock
, getBlockWindow
, splitChains
, getParentBlock
, getChildBlocks
, lastSeenCheckpoint
, nextWorkRequired
, workFromInterval
, isValidPOW
, headerPOW
, headerWork
, nodeBlockHeader
, getForks
, chainPathQuery
, getHeads
, getChain
, splitBlock
, putBlock
, putBlocks
, getBestBlock
, getBlockByHash
, getBlocksByHash
, getBlockByHeight
, getBlocksByHeight
, getBlocksFromHeight
, getBlockAfterTime
, getBlocksAtHeight
, evalNewChain
, module Network.Haskoin.Node.HeaderTree.Model
, module Network.Haskoin.Node.HeaderTree.Types
) where

import           Control.Monad                         (forM, unless, when,
                                                        (<=<))
import           Control.Monad.State                   (evalStateT, get, put)
import           Control.Monad.Trans                   (MonadIO, lift)
import           Control.Monad.Trans.Either            (left, runEitherT)
import           Data.Bits                             (shiftL)
import qualified Data.ByteString                       as BS (reverse)
import           Data.Function                         (on)
import           Data.LargeWord                        (Word256)
import           Data.List                             (find, maximumBy,
                                                        minimumBy, sort)
import           Data.Maybe                            (fromMaybe, isNothing,
                                                        listToMaybe, mapMaybe)
import           Data.Word                             (Word32)
import           Database.Esqueleto                    (Esqueleto, Value, asc,
                                                        from, groupBy, in_,
                                                        insertMany_, limit,
                                                        max_, orderBy, select,
                                                        unValue, val, valList,
                                                        where_, (&&.), (<=.),
                                                        (==.), (>.), (>=.),
                                                        (^.), (||.))
import           Database.Persist                      (Entity (..), insert_)
import           Database.Persist.Sql                  (SqlPersistT)
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Node.Checkpoints
import           Network.Haskoin.Node.HeaderTree.Model
import           Network.Haskoin.Node.HeaderTree.Types
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util

z :: BlockHash
z = "0000000000000000000000000000000000000000000000000000000000000000"

data BlockChainAction
    = BestChain  { actionNodes :: ![NodeBlock] }
    | ChainReorg { actionSplitNode :: !NodeBlock
                 , actionOldNodes  :: ![NodeBlock]
                 , actionNodes     :: ![NodeBlock]
                 }
    | SideChain  { actionNodes :: ![NodeBlock] }
    | KnownChain { actionNodes :: ![NodeBlock] }
    deriving (Show, Eq)

-- | Number of blocks on average between difficulty cycles (2016 blocks).
diffInterval :: Word32
diffInterval = targetTimespan `div` targetSpacing

-- | Genesis block.
genesisBlock :: NodeBlock
genesisBlock = NodeBlock
    { nodeBlockHash          = NodeHash $ headerHash genesisHeader
    , nodeBlockVersion       = blockVersion genesisHeader
    , nodeBlockPrev          = NodeHash $ prevBlock genesisHeader
    , nodeBlockMerkleRoot    = MerkleHash $ TxHash $ merkleRoot genesisHeader
    , nodeBlockTime          = blockTimestamp genesisHeader
    , nodeBlockBits          = blockBits genesisHeader
    , nodeBlockNonce         = bhNonce genesisHeader
    , nodeBlockHeight        = 0
    , nodeBlockWork          = Work $ headerWork genesisHeader
    , nodeBlockMedianTimes   = [blockTimestamp genesisHeader]
    , nodeBlockMinWork       = blockBits genesisHeader
    , nodeBlockChain         = 0
    , nodeBlockPivots        = []
    , nodeBlockPivotChains   = []
    }

-- | Initialize the block header chain by inserting the genesis block if it
-- doesn't already exist.
initHeaderTree :: MonadIO m => SqlPersistT m ()
initHeaderTree = do
    nodeM <- getBlockByHash $ headerHash genesisHeader
    when (isNothing nodeM) $ putBlock genesisBlock

-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectHeader :: MonadIO m
              => NodeBlock
              -> BlockHeader
              -> Timestamp
              -> SqlPersistT m (Either String BlockChainAction)
connectHeader best bh ts = runEitherT $ do
    parentM <- lift $ getBlockByHash $ prevBlock bh
    parent <- maybe (left "Could not get parent node") return parentM
    checkPointM <- fmap nodeBlockHeight <$> lift lastSeenCheckpoint
    chain <- lift $ getChain parent
    let bn = nodeBlock parent chain bh
    diffBlockM <- lift $ getBlockByHeight parent $
        nodeBlockHeight parent `div` diffInterval * diffInterval
    diffTime <- maybe (left "Could not get difficulty change block")
        (return . blockTimestamp . nodeBlockHeader)
        diffBlockM
    liftEither $ verifyBlockHeader parent diffTime checkPointM ts bh
    lift $ putBlock bn
    lift $ evalNewChain best [bn]

-- | A more efficient way of connecting a list of block headers than connecting
-- them individually. The list of block headers have must form a valid chain.
connectHeaders :: MonadIO m
               => [BlockHeader]
               -> Timestamp
               -> SqlPersistT m (Either String BlockChainAction)
connectHeaders [] _ = runEitherT $ left "Nothing to connect"
connectHeaders bhs@(bh:_) ts = runEitherT $ do
    unless (validChain bhs) $
        left "Block headers do not form a valid chain"
    best <- lift getBestBlock
    checkPointM <- fmap nodeBlockHeight <$> lift lastSeenCheckpoint
    parentM <- lift $ getBlockByHash $ prevBlock bh
    parent <- maybe (left "Could not get parent node") return parentM
    chain <- lift $ getChain parent
    diffBlockM <- lift $ getBlockByHeight parent $
        nodeBlockHeight parent `div` diffInterval * diffInterval
    diffTime <- maybe (left "Could not get difficulty change block")
        (return . blockTimestamp . nodeBlockHeader)
        diffBlockM
    nodes <- (`evalStateT` (parent, diffTime)) $ forM bhs $ \b -> do
        (p, d) <- get
        lift $ liftEither $ verifyBlockHeader p d checkPointM ts b
        let bn = nodeBlock p chain b
            d' = if nodeBlockHeight bn `mod` diffInterval == 0
                 then blockTimestamp b
                 else d
        put (bn, d')
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
    return $ map (getNodeHash . nodeBlockHash) nodes
  where
    h = nodeBlockHeight node
    f x s = (fst x - s, fst x > s)
    bs = (++ [0]) $ map fst $ takeWhile snd $
        [(h - x, x < h) | x <- [0..9]] ++
        scanl f (h - 10, h > 10) [2 ^ (x :: Word32) | x <- [1..]]

-- | Verify block header conforms to protocol.
verifyBlockHeader :: NodeBlock        -- ^ Parent block header
                  -> Timestamp        -- ^ Previous difficulty change
                  -> Maybe Word32     -- ^ Height of most recent checkpoint
                  -> Timestamp        -- ^ Current time
                  -> BlockHeader      -- ^ Block header to validate
                  -> Either String ()
-- TODO: Add DOS return values
verifyBlockHeader parent prevDiffTime checkPointM timestamp bh = do
    unless (isValidPOW bh) $
        Left "Invalid proof of work"

    unless (blockTimestamp bh <= timestamp + 2 * 60 * 60) $
        Left "Invalid header timestamp"

    let nextWork = nextWorkRequired parent prevDiffTime bh
    unless (blockBits bh == nextWork) $
        Left "Incorrect work transition (bits)"

    let sortedMedians = sort $ nodeBlockMedianTimes parent
        medianTime    = sortedMedians !! (length sortedMedians `div` 2)
    when (blockTimestamp bh <= medianTime) $
        Left "Block timestamp is too early"

    let newHeight = nodeBlockHeight parent + 1
    unless (maybe True (fromIntegral newHeight >) checkPointM) $
        Left "Rewriting pre-checkpoint chain"

    unless (verifyCheckpoint (fromIntegral newHeight) (headerHash bh)) $
        Left "Rejected by checkpoint lock-in"

    -- All block of height 227836 or more use version 2 in prodnet
    -- TODO: Find out the value here for testnet
    when (networkName == "prodnet"
          && blockVersion bh == 1
          && nodeBlockHeight parent + 1 >= 227836) $
        Left "Rejected version 1 block"

-- | Create a block node data structure from a block header.
nodeBlock :: NodeBlock    -- ^ Parent block node
          -> Int          -- ^ Chain number for new node
          -> BlockHeader
          -> NodeBlock
nodeBlock parent chain bh = NodeBlock
    { nodeBlockHash              = NodeHash $ headerHash bh
    , nodeBlockVersion           = blockVersion bh
    , nodeBlockPrev              = NodeHash $ prevBlock bh
    , nodeBlockMerkleRoot        = MerkleHash $ TxHash $ merkleRoot bh
    , nodeBlockTime              = blockTimestamp bh
    , nodeBlockBits              = blockBits bh
    , nodeBlockNonce             = bhNonce bh
    , nodeBlockHeight            = height
    , nodeBlockWork              = newWork
    , nodeBlockMedianTimes       = medianTimes
    , nodeBlockMinWork           = minWork
    , nodeBlockChain             = chain
    , nodeBlockPivots            = pivots
    , nodeBlockPivotChains       = chains
    }
  where
    newWork = Work $ getWork (nodeBlockWork parent) + headerWork bh
    medianTimes = blockTimestamp bh : take 10 (nodeBlockMedianTimes parent)
    height = nodeBlockHeight parent + 1
    isDiffChange = height `mod` diffInterval == 0
    isNotLimit   = blockBits bh /= encodeCompact powLimit
    minWork | not allowMinDifficultyBlocks = 0
            | isDiffChange || isNotLimit   = blockBits bh
            | otherwise                    = nodeBlockMinWork parent
    pivots | chain == nodeBlockChain parent = nodeBlockPivots parent
           | otherwise = nodeBlockHeight parent : nodeBlockPivots parent
    chains | chain == nodeBlockChain parent = nodeBlockPivotChains parent
           | otherwise = nodeBlockChain parent : nodeBlockPivotChains parent

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
    let (height, _) = splitBlock l r
    (split:ls) <- getBlocksFromHeight l ln height
    rs         <- getBlocksFromHeight r rn (height + 1)
    return (split, ls, rs)

-- | Finds the parent of a block.
getParentBlock :: MonadIO m
               => NodeBlock
               -> SqlPersistT m (Maybe NodeBlock)
getParentBlock node
    | p == z    = return Nothing
    | otherwise = getBlockByHash p
  where
    p = getNodeHash $ nodeBlockPrev node

-- | Get all children for a block
getChildBlocks :: MonadIO m
              => BlockHash
              -> SqlPersistT m [NodeBlock]
getChildBlocks h = fmap (map entityVal) $ select $ from $ \t -> do
    where_ $ t ^. NodeBlockPrev ==. val (NodeHash h)
    return t


-- | Get the last checkpoint that we have seen.
lastSeenCheckpoint :: MonadIO m
                   => SqlPersistT m (Maybe NodeBlock)
lastSeenCheckpoint =
    fmap listToMaybe $ getBlocksByHash $ map snd $ reverse checkpointList

-- | Returns the work required for a block header given the previous block. This
-- coresponds to bitcoind function GetNextWorkRequired in main.cpp.
nextWorkRequired :: NodeBlock
                 -> Timestamp
                 -> BlockHeader
                 -> Word32
nextWorkRequired parent ts bh
    -- Genesis block
    | nodeBlockPrev parent == NodeHash z = encodeCompact powLimit
    -- Only change the difficulty once per interval
    | (nodeBlockHeight parent + 1) `mod` diffInterval /= 0 =
        if allowMinDifficultyBlocks
            then minPOW
            else nodeBlockBits parent
    | otherwise = workFromInterval ts (nodeBlockHeader parent)
  where
    delta = targetSpacing * 2
    minPOW
        | blockTimestamp bh > nodeBlockTime parent + delta =
            encodeCompact powLimit
        | otherwise = nodeBlockMinWork parent

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
headerPOW =  bsToInteger . BS.reverse . encode' . headerHash

-- | Returns the work represented by this block. Work is defined as the number
-- of tries needed to solve a block in the average case with respect to the
-- target.
headerWork :: BlockHeader -> Word256
headerWork bh =
    fromIntegral $ largestHash `div` (target + 1)
  where
    target      = decodeCompact (blockBits bh)
    largestHash = 1 `shiftL` 256

{- Persistent backend -}

nodeBlockHeader :: NodeBlock -> BlockHeader
nodeBlockHeader node = BlockHeader
    { blockVersion    = nodeBlockVersion node
    , prevBlock       = getNodeHash $ nodeBlockPrev node
    , merkleRoot      = getTxHash $ getMerkleHash $ nodeBlockMerkleRoot node
    , blockTimestamp  = nodeBlockTime node
    , blockBits       = nodeBlockBits node
    , bhNonce         = nodeBlockNonce node
    }

getForks :: NodeBlock -> [(BlockHeight, Int)]
getForks NodeBlock{..} = zip nodeBlockPivots nodeBlockPivotChains

chainPathQuery :: forall (expr :: * -> *) (query :: * -> *) backend.
                  Esqueleto query expr backend
               => expr (Entity NodeBlock)
               -> [(BlockHeight, Int)]
               -> expr (Value Bool)
chainPathQuery _ [] = error "Monsters, monsters everywhere"

chainPathQuery t [(h,c)] =
    t ^. NodeBlockHeight <=. val h &&. t ^. NodeBlockChain ==. val c

chainPathQuery t ((h1,c):bs@((h2,_):_)) = chainPathQuery t bs ||.
    (   t ^. NodeBlockHeight <=. val h1
    &&. t ^. NodeBlockHeight >.  val h2
    &&. t ^. NodeBlockChain  ==. val c
    )

getHeads :: MonadIO m => SqlPersistT m [NodeBlock]
getHeads = fmap (map (entityVal . snd)) $ select $ from $ \t -> do
    groupBy $ t ^. NodeBlockChain
    return (max_ (t ^. NodeBlockHeight), t)

-- | Chain for new block building on a parent node
getChain :: MonadIO m
         => NodeBlock    -- ^ Parent node
         -> SqlPersistT m Int
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

-- | Get node height and chain common to both given.
splitBlock :: NodeBlock -> NodeBlock -> (BlockHeight, Int)
splitBlock l r =
    if nodeBlockChain l == nodeBlockChain r
      then
        let nb = minimumBy (compare `on` nodeBlockHeight) [l,r]
        in (nodeBlockHeight nb, nodeBlockChain nb)
      else
        let f (x,y) = snd x == snd y
            g x = (0, 0) :
                reverse ((nodeBlockHeight x, nodeBlockChain x) : getForks x)
            ns = zip (g l) (g r)
            xs = reverse $ takeWhile f ns
        in minimumBy (compare `on` fst) [fst (head xs), snd (head xs)]

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
getBestBlock = maximumBy (compare `on` nodeBlockWork) <$> getHeads

getBlockByHash :: MonadIO m => BlockHash -> SqlPersistT m (Maybe NodeBlock)
getBlockByHash h =
    fmap (listToMaybe . map entityVal) $ select $ from $ \t -> do
        where_ $ t ^. NodeBlockHash ==. val (NodeHash h)
        return t

-- | Get multiple blocks corresponding to given hashes
getBlocksByHash :: MonadIO m
                => [BlockHash]
                -> SqlPersistT m [NodeBlock]
getBlocksByHash hashes = do
    nodes <- fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ t ^. NodeBlockHash `in_` valList (map NodeHash hashes)
        return t
    return $ mapMaybe
        (\h -> find ((==h) . getNodeHash . nodeBlockHash) nodes)
        hashes

-- | Get ancestor of specified block at given height.
getBlockByHeight :: MonadIO m
                  => NodeBlock     -- ^ Best block
                  -> BlockHeight
                  -> SqlPersistT m (Maybe NodeBlock)
getBlockByHeight best height = do
    let forks = (nodeBlockHeight best, nodeBlockChain best) : getForks best
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
    let forks = (nodeBlockHeight best, nodeBlockChain best) : getForks best
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
getBlocksFromHeight best cnt height = do
    let forks = (nodeBlockHeight best, nodeBlockChain best) : getForks best
    fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ chainPathQuery t forks &&.
            t ^. NodeBlockHeight >=. val height
        when (cnt > 0) $ limit $ fromIntegral cnt
        return t

-- | Get node immediately at or after timestamp in main chain.
getBlockAfterTime :: MonadIO m => Timestamp -> SqlPersistT m (Maybe NodeBlock)
getBlockAfterTime ts = do
    n <- getBestBlock
    let ns = (nodeBlockHeight n, nodeBlockChain n) : getForks n
    fmap (listToMaybe . map entityVal) $ select $ from $ \t -> do
        where_ $ chainPathQuery t ns &&. t ^. NodeBlockTime >=. val ts
        orderBy [asc $ t ^. NodeBlockHeight]
        limit 1
        return t

-- | Get nodes at height.
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
    | buildsOnBest = return $ BestChain newNodes
    | nodeBlockWork (last newNodes) > nodeBlockWork best = do
        (split, old, new) <- splitChains (best, 0) (head newNodes, 0)
        return $ ChainReorg split old (new ++ tail newNodes)
    | otherwise = do
        (split, _, new) <- splitChains (best, 0) (head newNodes, 0)
        case new of
            [] -> return $ KnownChain newNodes
            _  -> return $ SideChain $ split : new ++ tail newNodes
  where
    buildsOnBest = nodeBlockPrev (head newNodes) == nodeBlockHash best
