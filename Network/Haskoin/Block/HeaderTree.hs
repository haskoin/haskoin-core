{-# LANGUAGE UndecidableInstances #-}
module Network.Haskoin.Block.HeaderTree where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, when, unless, liftM, (<=<), forM)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Either (EitherT, left, runEitherT)
import Control.Monad.State (MonadState, StateT, evalStateT, get)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans.Free (FreeF(..), Free, FreeT(..), liftF, runFreeT)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Trans.Control
    ( MonadTransControl(..)
    , MonadBaseControl(..)
    , defaultLiftWith
    , defaultLiftBaseWith
    )

import Data.Word (Word32)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (sort, nub)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Default (def)
import qualified Data.Binary as B (Binary, get, put)
import qualified Data.Conduit as C (Source, awaitForever, yield, ($=), ($$))
import qualified Data.Conduit.List as CL (consume)
import qualified Data.ByteString as BS (ByteString, reverse, append)

import qualified Database.LevelDB.Base as L
    ( DB
    , ReadOptions(..)
    , get
    , put
    , iterFirst
    , iterNext
    , iterValue
    , createIter
    , releaseIter
    , createSnapshot
    , releaseSnapshot
    )

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Checkpoints
import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

data HeaderTreeF a
    = GetBlockHeaderNode BlockHash ( Maybe BlockHeaderNode -> a )
    | PutBlockHeaderNode BlockHeaderNode a
    | GetBestBlockHeader ( BlockHeaderNode -> a )
    | SetBestBlockHeader BlockHeaderNode a
    deriving Functor

-- | HeaderTree Free monad transformer
type HeaderTreeT m = FreeT HeaderTreeF m

-- | HeaderTree Free monad
type HeaderTree = Free HeaderTreeF

-- MonadLogger instance for HeaderTreeT
instance MonadLogger m => MonadLogger (HeaderTreeT m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

instance MonadBase b m => MonadBase b (HeaderTreeT m) where
    liftBase = liftBaseDefault

-- MonadBaseControl instance for HeaderTreeT
instance MonadBaseControl b m => MonadBaseControl b (HeaderTreeT m) where
    newtype StM (HeaderTreeT m) a = 
        StMHeaderTreeT (StM m (FreeF HeaderTreeF a (HeaderTreeT m a)))

    liftBaseWith f = 
        FreeT $ liftM Pure $
            liftBaseWith $ \runInBase -> 
                f $ \ht -> liftM StMHeaderTreeT $ runInBase $ runFreeT ht

    restoreM (StMHeaderTreeT m) = FreeT . restoreM $ m
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

getBlockHeaderNode :: Monad m 
                   => BlockHash -> HeaderTreeT m (Maybe BlockHeaderNode)
getBlockHeaderNode bh = liftF $ GetBlockHeaderNode bh id

putBlockHeaderNode :: Monad m => BlockHeaderNode -> HeaderTreeT m ()
putBlockHeaderNode bhn = liftF $ PutBlockHeaderNode bhn ()

getBestBlockHeader :: Monad m => HeaderTreeT m BlockHeaderNode
getBestBlockHeader = liftF $ GetBestBlockHeader id

setBestBlockHeader :: Monad m => BlockHeaderNode -> HeaderTreeT m ()
setBestBlockHeader bhn = liftF $ SetBestBlockHeader bhn ()

-- | Data type representing a BlockHeader node in the header chain. It
-- contains additional data such as the chain work and chain height for this
-- node.
data BlockHeaderNode = BlockHeaderNode 
    { nodeBlockHash    :: !BlockHash
    , nodeHeader       :: !BlockHeader
    , nodeHeaderHeight :: !BlockHeight
    , nodeChainWork    :: !Integer
    , nodeChild        :: !(Maybe BlockHash)
    , nodeMedianTimes  :: ![Timestamp]
    , nodeMinWork      :: !Word32 -- Only used for testnet
    } deriving (Show, Read, Eq)

instance B.Binary BlockHeaderNode where

    get = do
        nodeBlockHash    <- B.get
        nodeHeader       <- B.get
        nodeHeaderHeight <- getWord32le
        nodeChainWork    <- B.get
        nodeChild        <- B.get
        nodeMedianTimes  <- B.get
        nodeMinWork      <- B.get
        return BlockHeaderNode{..}

    put BlockHeaderNode{..} = do
        B.put       nodeBlockHash
        B.put       nodeHeader
        putWord32le nodeHeaderHeight
        B.put       nodeChainWork
        B.put       nodeChild
        B.put       nodeMedianTimes
        B.put       nodeMinWork

data BlockChainAction
    = BestChain  { actionBestChain :: ![BlockHeaderNode] }
    | ChainReorg { reorgSplitPoint :: !BlockHeaderNode
                 , reorgOldBlocks  :: ![BlockHeaderNode]
                 , reorgNewBlocks  :: ![BlockHeaderNode]
                 }
    | SideChain  { actionSideBlock :: ![BlockHeaderNode] }
    deriving (Read, Show, Eq)

actionNewNodes :: BlockChainAction -> [BlockHeaderNode]
actionNewNodes action = case action of
    BestChain ns -> ns
    ChainReorg _ _ ns -> ns
    SideChain ns -> ns

-- | Number of blocks on average between difficulty cycles (2016 blocks)
diffInterval :: Word32
diffInterval = targetTimespan `div` targetSpacing

-- | Genesis BlockHeaderNode
genesisNode :: BlockHeaderNode
genesisNode = BlockHeaderNode
    { nodeBlockHash    = headerHash genesisHeader
    , nodeHeader       = genesisHeader
    , nodeHeaderHeight = 0
    , nodeChainWork    = headerWork genesisHeader
    , nodeChild        = Nothing
    , nodeMedianTimes  = [blockTimestamp genesisHeader]
    , nodeMinWork      = blockBits genesisHeader
    }

-- | Initialize the block header chain by inserting the genesis block if
-- it doesn't already exist.
initHeaderTree :: Monad m => HeaderTreeT m ()
initHeaderTree = do
    genM <- getBlockHeaderNode $ nodeBlockHash genesisNode
    when (isNothing genM) $ do
        putBlockHeaderNode genesisNode
        setBestBlockHeader genesisNode

-- A more efficient way of connecting a list of BlockHeaders than connecting
-- them individually. The work check will only be done once for the whole
-- chain. The list of BlockHeaders have to form a valid chain, linked by their
-- parents.
connectHeaders :: Monad m
               => [BlockHeader]
               -> Timestamp
               -> Bool
               -> HeaderTreeT m (Either String BlockChainAction)
connectHeaders bhs adjustedTime commit
    | null bhs = return $ Left "Invalid empty BlockHeaders in connectHeaders"
    | validChain bhs = runEitherT $ do
        newNode <- liftM last $ forM bhs $ \bh -> do
            parNode <- verifyBlockHeader bh adjustedTime
            lift $ storeBlockHeader bh parNode
        -- Best header will only be updated if we have no errors
        lift $ evalNewChain commit newNode 
    | otherwise = return $ Left "BlockHeaders do not form a valid chain."
  where
    validChain (a:b:xs) =  prevBlock b == headerHash a && validChain (b:xs)
    validChain (a:[]) = True
    validChain _ = False

-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectHeader :: Monad m 
              => BlockHeader
              -> Timestamp
              -> Bool
              -> HeaderTreeT m (Either String BlockChainAction)
connectHeader bh adjustedTime commit = runEitherT $ do
    parNode <- verifyBlockHeader bh adjustedTime
    lift $ evalNewChain commit =<< storeBlockHeader bh parNode

evalNewChain :: Monad m 
             => Bool -> BlockHeaderNode -> HeaderTreeT m BlockChainAction
evalNewChain commit newNode = do
    currentHead <- getBestBlockHeader
    action <- go =<< findSplitNode currentHead newNode
    when commit $ commitAction action
    return action
  where
    go (split, old, new)
        | length old == 0 && length new >= 1 = return $ BestChain new
        | nodeChainWork (last new) > nodeChainWork (last old) = 
            return $ ChainReorg split old new
        | otherwise = return $ SideChain new

-- | Update the best block header of the action in the header tree
commitAction :: Monad m => BlockChainAction -> HeaderTreeT m ()
commitAction action = do
    currentHead <- getBestBlockHeader
    let newNodes = actionNewNodes action
        newNode  = last newNodes
    when (nodeChainWork newNode > nodeChainWork currentHead) $ do
        -- Update the child links starting from the current head
        updateChilds (currentHead:newNodes)
        -- Update the best block header
        setBestBlockHeader newNode
  where
    updateChilds (a:b:xs) = do
        putBlockHeaderNode a{ nodeChild = Just $ nodeBlockHash b }
        updateChilds (b:xs)
    updateChilds _ = return ()
    
-- TODO: Add DOS return values
verifyBlockHeader :: Monad m 
                  => BlockHeader 
                  -> Timestamp
                  -> EitherT String (HeaderTreeT m) BlockHeaderNode
verifyBlockHeader bh adjustedTime  = do
    unless (isValidPOW bh) $ left "Invalid proof of work"

    unless (blockTimestamp bh <= adjustedTime + 2 * 60 * 60) $
        left "Invalid header timestamp"

    parNodeM <- lift $ getBlockHeaderNode $ prevBlock bh
    let parNode = fromJust parNodeM
    when (isNothing parNodeM) $ left "Parent block not found"

    nextWork <- lift $ nextWorkRequired parNode bh
    unless (blockBits bh == nextWork) $ left "Incorrect work transition (bits)"

    let sortedMedians = sort $ nodeMedianTimes parNode
        medianTime    = sortedMedians !! (length sortedMedians `div` 2)
    when (blockTimestamp bh <= medianTime) $ left "Block timestamp is too early"

    chkPointM <- lift lastSeenCheckpoint
    let chkPoint  = fromJust chkPointM
        newHeight = nodeHeaderHeight parNode + 1
    unless (isNothing chkPointM || (fromIntegral newHeight) > fst chkPoint) $
        left "Rewriting pre-checkpoint chain"

    unless (verifyCheckpoint (fromIntegral newHeight) bid) $
        left "Rejected by checkpoint lock-in"

    -- All block of height 227836 or more use version 2 in prodnet
    -- TODO: Find out the value here for testnet
    when (  networkName == "prodnet" 
         && blockVersion bh == 1 
         && nodeHeaderHeight parNode + 1 >= 227836) $
        left "Rejected version=1 block"

    return parNode
  where
    bid = headerHash bh

-- Build a new block header and store it
storeBlockHeader :: Monad m 
                 => BlockHeader 
                 -> BlockHeaderNode
                 -> HeaderTreeT m BlockHeaderNode
storeBlockHeader bh parNode = do
    let nodeBlockHash    = bid
        nodeHeader       = bh
        nodeHeaderHeight = newHeight
        nodeChainWork    = newWork
        nodeChild        = Nothing
        nodeMedianTimes  = newMedian
        nodeMinWork      = minWork
        newNode          = BlockHeaderNode{..}
    prevM <- getBlockHeaderNode bid
    case prevM of
        Just prev -> return prev
        Nothing   -> putBlockHeaderNode newNode >> return newNode
  where
    bid       = headerHash bh
    newHeight = nodeHeaderHeight parNode + 1
    newWork   = nodeChainWork parNode + headerWork bh
    newMedian = blockTimestamp bh : (take 10 $ nodeMedianTimes parNode)
    isDiffChange = newHeight `mod` diffInterval == 0
    isNotLimit   = blockBits bh /= encodeCompact powLimit
    minWork | not allowMinDifficultyBlocks = 0
            | isDiffChange || isNotLimit   = blockBits bh
            | otherwise                    = nodeMinWork parNode

-- | Return the window of nodes starting from the child of the given node.
-- Child links are followed to build the window. If the window ends in an
-- orphaned chain, we backtrack and return the window in the main chain.
-- The result is returned in a BlockChainAction to know if we had to 
-- backtrack into the main chain or not.
getNodeWindow :: Monad m 
              => BlockHash -> Int -> HeaderTreeT m (Maybe BlockChainAction)
getNodeWindow bh cnt = getBlockHeaderNode bh >>= \nodeM -> case nodeM of
    Just node -> go [] cnt node
    Nothing -> return Nothing
  where
    go [] 0 _  = return Nothing
    go acc 0 _ = return $ Just $ BestChain $ reverse acc
    go acc i node = getChildNode node >>= \childM -> case childM of
        Just child -> go (child:acc) (i-1) child
        Nothing -> do
            -- We are at the end of our chain. Check if there is a better chain.
            currentHead <- getBestBlockHeader
            if nodeChainWork currentHead > nodeChainWork node
                -- We got stuck in an orphan chain. We need to backtrack.
                then findMainChain currentHead
                -- We are at the end of the main chain
                else return $ if null acc 
                    then Nothing 
                    else Just $ BestChain $ reverse acc
    findMainChain currentHead = do
        -- Compute the split point from the original input node so that the old
        -- chain doesn't contain blocks beyond the original node.
        node <- liftM fromJust $ getBlockHeaderNode bh
        (split, old, new) <- findSplitNode node currentHead
        return $ Just $ ChainReorg split old $ take cnt new

-- | Find the split point between two nodes. It also returns the two partial
-- chains leading from the split point to the respective nodes.
findSplitNode :: Monad m
              => BlockHeaderNode 
              -> BlockHeaderNode
              -> HeaderTreeT m ( BlockHeaderNode
                               , [BlockHeaderNode]
                               , [BlockHeaderNode]
                               )
findSplitNode n1 n2 = 
    go [] [] n1 n2
  where
    go xs ys x y
        | nodeBlockHash x == nodeBlockHash y = return (x, xs, ys)
        | nodeHeaderHeight x > nodeHeaderHeight y = do
            par <- fromJust <$> getParentNode x
            go (x:xs) ys par y
        | otherwise = do
            par <- fromJust <$> getParentNode y
            go xs (y:ys) x par

-- | Finds the parent of a BlockHeaderNode
getParentNode :: Monad m 
              => BlockHeaderNode -> HeaderTreeT m (Maybe BlockHeaderNode)
getParentNode node
    | p == 0    = return Nothing
    | otherwise = getBlockHeaderNode p
  where
    p = prevBlock $ nodeHeader node

-- | Finds the child of a BlockHeaderNode if it exists. If a node has
-- multiple childs, this function will always return the child on the
-- main branch.
getChildNode :: Monad m 
             => BlockHeaderNode -> HeaderTreeT m (Maybe BlockHeaderNode)
getChildNode node = case nodeChild node of
    Just child -> getBlockHeaderNode child
    Nothing    -> return Nothing

-- | Get the last checkpoint that we have seen
lastSeenCheckpoint :: Monad m => HeaderTreeT m (Maybe (Int, BlockHash))
lastSeenCheckpoint = 
    go $ reverse checkpointList
  where
    go ((i, chk):xs) = do
        existsChk <- isJust <$> getBlockHeaderNode chk
        if existsChk then return $ Just (i, chk) else go xs
    go [] = return Nothing

-- | Returns the work required for a BlockHeader given the previous
-- BlockHeaderNode. This function coresponds to bitcoind function
-- GetNextWorkRequired in main.cpp.
nextWorkRequired :: Monad m 
                 => BlockHeaderNode -> BlockHeader -> HeaderTreeT m Word32
nextWorkRequired lastNode bh
    -- Genesis block
    | prevBlock (nodeHeader lastNode) == 0 = return $ encodeCompact powLimit
    -- Only change the difficulty once per interval
    | (nodeHeaderHeight lastNode + 1) `mod` diffInterval /= 0 = return $
        if allowMinDifficultyBlocks 
            then minPOW 
            else blockBits $ nodeHeader lastNode
    | otherwise = do
        -- TODO: Can this break if there are not enough blocks in the chain?
        firstNode <- foldM (flip ($)) lastNode fs
        let lastTs = blockTimestamp $ nodeHeader firstNode
        return $ workFromInterval lastTs (nodeHeader lastNode)
  where
    len   = fromIntegral diffInterval - 1
    fs    = replicate len (fmap fromJust . getParentNode)
    delta = targetSpacing * 2
    minPOW
        | blockTimestamp bh > (blockTimestamp $ nodeHeader lastNode) + delta =
            encodeCompact powLimit
        | otherwise = nodeMinWork lastNode

-- | Computes the work required for the next block given a timestamp and the
-- current block. The timestamp should come from the block that matched the
-- last jump in difficulty (spaced out by 2016 blocks in prodnet).
workFromInterval :: Timestamp -> BlockHeader -> Word32
workFromInterval ts lastB
    | newDiff > powLimit = encodeCompact powLimit
    | otherwise          = encodeCompact newDiff
  where
    t = fromIntegral $ (blockTimestamp lastB) - ts
    actualTime 
        | t < targetTimespan `div` 4 = targetTimespan `div` 4
        | t > targetTimespan * 4     = targetTimespan * 4
        | otherwise                  = t
    lastDiff = decodeCompact $ blockBits lastB
    newDiff = lastDiff * (toInteger actualTime) `div` (toInteger targetTimespan)

-- | Returns a BlockLocator object.
blockLocator :: Monad m => HeaderTreeT m BlockLocator
blockLocator = do
    h  <- getBestBlockHeader
    let xs = [go ((2 :: Int)^x) | x <- ([0..] :: [Int])]
    ns <- f [h] $ replicate 10 (go (1 :: Int)) ++ xs
    return $ reverse $ nub $ genid : map nodeBlockHash ns
  where
    genid = headerHash genesisHeader
    f acc gs = (head gs) (head acc) >>= \resM -> case resM of
        Just res -> f (res:acc) (tail gs)
        Nothing  -> return acc
    go step n = getParentNode n >>= \parM -> case parM of
        Just par -> if step == 0 then return (Just n) else go (step - 1) par
        Nothing  -> return Nothing

bestBlockHeaderHeight :: Monad m => HeaderTreeT m BlockHeight
bestBlockHeaderHeight = liftM nodeHeaderHeight getBestBlockHeader

getBlockHeaderHeight :: Monad m 
                     => BlockHash -> HeaderTreeT m (Maybe BlockHeight)
getBlockHeaderHeight = return . fmap nodeHeaderHeight <=< getBlockHeaderNode

-- | Returns True if the difficulty target (bits) of the header is valid
-- and the proof of work of the header matches the advertised difficulty target.
-- This function corresponds to the function CheckProofOfWork from bitcoind
-- in main.cpp
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
headerWork :: BlockHeader -> Integer
headerWork bh = 
    largestHash `div` (target + 1)
  where
    target      = decodeCompact (blockBits bh)
    largestHash = 1 `shiftL` 256

{- Default LevelDB implementation -}

indexKey :: BlockHash -> BS.ByteString
indexKey bid = "index_" `BS.append` encode' bid

getLevelDBNode :: (MonadIO m, MonadState L.DB m) 
               => BlockHash -> m (Maybe BlockHeaderNode)
getLevelDBNode bid = do
    db <- get
    resM <- L.get db def $ indexKey bid
    return $ decodeToMaybe =<< resM

putLevelDBNode :: (MonadIO m, MonadState L.DB m) => BlockHeaderNode -> m ()
putLevelDBNode node = do
    db <- get
    L.put db def (indexKey $ nodeBlockHash node) $ encode' node

-- TODO: Finish this
runHeaderTreeLevelDB :: (MonadIO m, MonadState L.DB m) => HeaderTreeT m a -> m a
runHeaderTreeLevelDB ht = runFreeT ht >>= \f -> case f of
    Free (GetBlockHeaderNode bid next) -> do
        nodeM <- getLevelDBNode bid
        runHeaderTreeLevelDB $ next nodeM
    Free (PutBlockHeaderNode node next) -> do
        putLevelDBNode node
        runHeaderTreeLevelDB next
    Free (GetBestBlockHeader next) -> do
        db <- get
        bidM <- L.get db def "bestblockheader"
        case decodeToMaybe =<< bidM of
            Just bid -> do
                node <- liftM fromJust $ getLevelDBNode bid
                runHeaderTreeLevelDB $ next node
            Nothing  -> error 
                "GetBestBlockHeader: Best block header does not exist"
    Free (SetBestBlockHeader node next) -> do
        db <- get
        L.put db def "bestblockheader" $ encode' $ nodeBlockHash node
        runHeaderTreeLevelDB next

