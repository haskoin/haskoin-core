module Network.Haskoin.Block.HeaderChain 
( BlockHeaderNode(..)
, BlockHeaderStore(..)
, BlockHeaderAction(..)
, genesisBlockHeaderNode
, initHeaderChain
, rescanHeaderChain
, connectBlockHeader
, blockLocator
, bestBlockHeaderHeight
, getBlockHeaderHeight
, lastSeenCheckpoint
, findSplitNode
, getParentNode
, nextWorkRequired
, workFromInterval
, isValidPOW
, headerPOW
, headerWork
, BlockChainAction(..)
, getActionNode
, connectBlock
, blocksToDownload
, LevelDBChain
, runLevelDBChain
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, when, unless, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (left, runEitherT)
import qualified Control.Monad.State as S (StateT, evalStateT, get)

import Data.Word (Word32)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (sort, nub)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Default (def)
import qualified Data.Conduit as C (Source, awaitForever, yield, ($=), ($$))
import qualified Data.Conduit.List as CL (consume)
import qualified Data.ByteString as BS (ByteString, reverse, append)

import qualified Database.LevelDB.Base as DB 
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

-- | Data type representing a BlockHeader node in the header chain. It
-- contains additional data such as the chain work and chain height for this
-- node.
data BlockHeaderNode = BlockHeaderNode 
    { nodeBlockHash    :: !BlockHash
    , nodeHeader       :: !BlockHeader
    , nodeHeaderHeight :: !BlockHeight
    , nodeChainWork    :: !Integer
    , nodeMedianTimes  :: ![Timestamp]
    , nodeMinWork      :: !Word32 -- Only used for testnet
    , nodeHaveBlock    :: !Bool -- Did we receive the full block/merkle block?
    } deriving (Show, Read, Eq)

instance Binary BlockHeaderNode where

    get = BlockHeaderNode <$> get 
                          <*> get
                          <*> getWord32le 
                          <*> get
                          <*> get
                          <*> get
                          <*> get

    put (BlockHeaderNode i b h w t m f) = do
        put i 
        put b 
        putWord32le h 
        put w 
        put t 
        put m
        put f

-- Return value of linking a new block header in the chain
data BlockHeaderAction
    = RejectHeader !String
    | HeaderAlreadyExists !BlockHeaderNode
    | AcceptHeader !BlockHeaderNode
    deriving (Show, Read, Eq)

class Monad m => BlockHeaderStore m where
    getBlockHeaderNode    :: BlockHash -> m BlockHeaderNode
    putBlockHeaderNode    :: BlockHeaderNode -> m ()
    existsBlockHeaderNode :: BlockHash -> m Bool
    sourceBlockHeader     :: C.Source m BlockHeaderNode
    getBestBlockHeader    :: m BlockHeaderNode
    setBestBlockHeader    :: BlockHeaderNode -> m ()
    getBestBlock          :: m BlockHeaderNode
    setBestBlock          :: BlockHeaderNode -> m ()
    setFastCatchup        :: Timestamp -> m ()
    getFastCatchup        :: m Timestamp
    setFastCatchupHeight  :: Maybe BlockHeight -> m ()
    getFastCatchupHeight  :: m (Maybe BlockHeight)

-- | Number of blocks on average between difficulty cycles (2016 blocks)
diffInterval :: Word32
diffInterval = targetTimespan `div` targetSpacing

-- | Genesis BlockHeaderNode
genesisBlockHeaderNode :: BlockHeaderNode
genesisBlockHeaderNode = BlockHeaderNode
    { nodeBlockHash    = headerHash genesisHeader
    , nodeHeader       = genesisHeader
    , nodeHeaderHeight = 0
    , nodeChainWork    = headerWork genesisHeader
    , nodeMedianTimes  = [blockTimestamp genesisHeader]
    , nodeMinWork      = blockBits genesisHeader
    , nodeHaveBlock    = True -- We always have the genesis block
    }

-- TODO: If dwnStart is not equal to the one in the database, issue a warning
-- or an error.
-- | Initialize the block header chain by inserting the genesis block if
-- it doesn't already exist.
initHeaderChain :: BlockHeaderStore m => Timestamp -> m ()
initHeaderChain ts = do
    existsGen <- existsBlockHeaderNode $ headerHash genesisHeader
    unless existsGen $ do
        putBlockHeaderNode genesisBlockHeaderNode
        setBestBlockHeader genesisBlockHeaderNode
        setBestBlock genesisBlockHeaderNode
        -- Adjust time backwards by a week to handle clock drifts.
        let fastCatchupI = max 0 ((toInteger ts) - 86400 * 7)
            fc           = fromInteger fastCatchupI 
        setFastCatchup fc
        setFastCatchupHeight $ if fc == 0 then Just 0 else Nothing

rescanHeaderChain :: BlockHeaderStore m 
                  => Timestamp -> m [(BlockHeight, BlockHash)]
rescanHeaderChain ts = do
    setFastCatchup fc
    -- Find and set the fast catchup height
    fcBlockM <- findFCBlock =<< getBestBlockHeader
    let fcHeightM = nodeHeaderHeight <$> fcBlockM
    setFastCatchupHeight fcHeightM
    -- Update the haveBlock flags
    toDwn <- sourceBlockHeader C.$= (updateHaveBlock fcHeightM) C.$$ CL.consume
    return $ map f toDwn
  where
    -- Adjust time backwards by a week to handle clock drifts.
    fastCatchupI = max 0 ((toInteger ts) - 86400 * 7)
    fc = fromInteger fastCatchupI 
    f n = (nodeHeaderHeight n, nodeBlockHash n)
    updateHaveBlock hM = C.awaitForever $ \n -> do
        let haveBlock = case hM of
                Just h  -> nodeHeaderHeight n < h
                Nothing -> True
        lift $ putBlockHeaderNode n{ nodeHaveBlock = haveBlock }
        unless haveBlock $ C.yield n
    -- We need to find the first block >= fc but the parent < fc
    findFCBlock n 
        | isGenesis n = setBestBlock n >> return (Just n)
        | blockTimestamp (nodeHeader n) >= fc = getParentNode n >>= \p -> do
            if blockTimestamp (nodeHeader p) < fc
                then setBestBlock p >> return (Just n)
                else findFCBlock p
        | otherwise = setBestBlock n >> return Nothing
    isGenesis n = nodeBlockHash n == nodeBlockHash genesisBlockHeaderNode

-- TODO: Add DOS return values
-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectBlockHeader :: BlockHeaderStore m 
                   => BlockHeader 
                   -> Timestamp
                   -> m BlockHeaderAction
connectBlockHeader bh adjustedTime = ((liftM f) . runEitherT) $ do
    unless (isValidPOW bh) $ 
        left $ RejectHeader "Invalid proof of work"
    unless (blockTimestamp bh <= adjustedTime + 2 * 60 * 60) $
        left $ RejectHeader "Invalid header timestamp"
    blockExists <- lift $ existsBlockHeaderNode bid
    when blockExists $ do
        prev <- lift $ getBlockHeaderNode bid
        left $ HeaderAlreadyExists prev
    prevExists <- lift $ existsBlockHeaderNode $ prevBlock bh
    unless prevExists $ left $ RejectHeader "Previous block not found"
    prevNode <- lift $ getBlockHeaderNode $ prevBlock bh
    nextWork <- lift $ nextWorkRequired prevNode bh
    unless (blockBits bh == nextWork) $
        left $ RejectHeader "Incorrect work transition (bits)"
    let sortedMedians = sort $ nodeMedianTimes prevNode
        medianTime    = sortedMedians !! (length sortedMedians `div` 2)
    when (blockTimestamp bh <= medianTime) $
        left $ RejectHeader "Block timestamp is too early"
    chkPointM <- lift lastSeenCheckpoint
    let chkPoint  = fromJust chkPointM
        newHeight = nodeHeaderHeight prevNode + 1
    unless (isNothing chkPointM || (fromIntegral newHeight) > fst chkPoint) $
        left $ RejectHeader "Rewriting pre-checkpoint chain"
    unless (verifyCheckpoint (fromIntegral newHeight) bid) $
        left $ RejectHeader "Rejected by checkpoint lock-in"
    -- All block of height 227836 or more use version 2 in prodnet
    -- TODO: Find out the value here for testnet
    when (  networkName == "prodnet" 
         && blockVersion bh == 1 
         && nodeHeaderHeight prevNode + 1 >= 227836) $
        left $ RejectHeader "Rejected version=1 block"
    lift $ storeBlockHeader prevNode bh 
  where
    f (Right x) = x
    f (Left  x) = x
    bid = headerHash bh

storeBlockHeader :: BlockHeaderStore m 
                 => BlockHeaderNode 
                 -> BlockHeader 
                 -> m BlockHeaderAction
storeBlockHeader prevNode bh = do
    -- Compute the fast catchup height if possible
    fc <- getFastCatchup
    fcHeightM <- getFastCatchupHeight
    haveBlock <- if isJust fcHeightM
        then return $ newHeight < (fromJust fcHeightM)
        else if blockTimestamp bh < fc
            then return True
            else do
                -- From this point on, we use newHeight as the fast catchup
                setFastCatchupHeight $ Just newHeight
                return False

    -- Store the new node
    let newNode = buildNewNode haveBlock
    putBlockHeaderNode newNode

    -- Update the best block header and best block
    currentHead <- getBestBlockHeader
    when (newWork > nodeChainWork currentHead) $ do
        setBestBlockHeader newNode
        when (nodeHaveBlock newNode) $ setBestBlock newNode

    return $ AcceptHeader newNode
  where
    bid       = headerHash bh
    newHeight = nodeHeaderHeight prevNode + 1
    newWork   = nodeChainWork prevNode + headerWork bh
    newMedian 
        | length (nodeMedianTimes prevNode) == 11 =
            tail (nodeMedianTimes prevNode) ++ [blockTimestamp bh]
        | otherwise = (nodeMedianTimes prevNode) ++ [blockTimestamp bh]
    isDiffChange = newHeight `mod` diffInterval == 0
    isNotLimit   = blockBits bh /= encodeCompact powLimit
    minWork | not allowMinDifficultyBlocks = 0
            | isDiffChange || isNotLimit   = blockBits bh
            | otherwise                    = nodeMinWork prevNode
    buildNewNode haveBlock = BlockHeaderNode 
        { nodeBlockHash    = bid
        , nodeHeader       = bh
        , nodeHeaderHeight = newHeight
        , nodeChainWork    = newWork
        , nodeMedianTimes  = newMedian
        , nodeMinWork      = minWork
        , nodeHaveBlock    = haveBlock
        }

-- | Get the last checkpoint that we have seen
lastSeenCheckpoint :: BlockHeaderStore m => m (Maybe (Int, BlockHash))
lastSeenCheckpoint = 
    foldM f Nothing $ reverse checkpointList
  where
    f res@(Just _) _  = return res
    f Nothing (i,chk) = do
        existsChk <- existsBlockHeaderNode chk
        return $ if existsChk then Just (i,chk) else Nothing

-- | Finds the parent of a BlockHeaderNode. Returns an error if a parent
-- node doesn't exist (for example, the genesis block).
getParentNode :: BlockHeaderStore m => BlockHeaderNode -> m BlockHeaderNode
getParentNode node 
    | p == 0    = error "Genesis block has no parent"
    | otherwise = getBlockHeaderNode p
  where
    p = prevBlock $ nodeHeader node

-- | Returns the work required for a BlockHeader given the previous
-- BlockHeaderNode. This function coresponds to bitcoind function
-- GetNextWorkRequired in main.cpp.
nextWorkRequired :: BlockHeaderStore m 
                 => BlockHeaderNode -> BlockHeader -> m Word32
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
        firstNode <- foldM (\x f -> f x) lastNode fs
        let lastTs = blockTimestamp $ nodeHeader firstNode
        return $ workFromInterval lastTs (nodeHeader lastNode)
  where
    fs    = replicate (fromIntegral diffInterval - 1) getParentNode
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
blockLocator :: BlockHeaderStore m => m BlockLocator
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
    go step n 
        | prevBlock (nodeHeader n) == 0 = return Nothing
        | step == 0 = return $ Just n
        | otherwise = go (step - 1) =<< getParentNode n

bestBlockHeaderHeight :: BlockHeaderStore m => m BlockHeight
bestBlockHeaderHeight = liftM nodeHeaderHeight getBestBlockHeader

getBlockHeaderHeight :: BlockHeaderStore m => BlockHash -> m BlockHeight
getBlockHeaderHeight h = liftM nodeHeaderHeight $ getBlockHeaderNode h

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

{- Functions for connecting blocks -}

data BlockChainAction
    = BestBlock       { actionBestBlock :: !BlockHeaderNode }
    | SideBlock       { actionSideBlock :: !BlockHeaderNode 
                      , sideBlockDist   :: !Int
                      }
    | BlockReorg      { reorgSplitPoint :: !BlockHeaderNode
                      , reorgOldBlocks  :: ![BlockHeaderNode]
                      , reorgNewBlocks  :: ![BlockHeaderNode]
                      }
    | OldBlock        { oldBlock     :: !BlockHeaderNode 
                      , distFromBest :: !Int
                      }
    deriving (Read, Show, Eq)

getActionNode :: BlockChainAction -> BlockHeaderNode
getActionNode a = case a of
    BestBlock n -> n
    SideBlock n _ -> n
    BlockReorg _ _ ns -> last ns
    OldBlock n _ -> n

-- | Connect a block to the blockchain. Blocks need to be imported in the order
-- of the parent links (oldest to newest).
connectBlock :: BlockHeaderStore m 
             => BlockHash
             -> m (Maybe BlockChainAction)
connectBlock h = do
    newNode <- liftM (\n -> n{ nodeHaveBlock = True }) $ getBlockHeaderNode h
    parNode <- getParentNode newNode
    if nodeHaveBlock parNode 
        then do
            putBlockHeaderNode newNode -- save the nodeHaveBlock = True
            bestNode <- getBestBlock
            if nodeBlockHash parNode == nodeBlockHash bestNode
                then do
                    setBestBlock newNode
                    return $ Just $ BestBlock newNode
                else go =<< findSplitNode bestNode newNode
        else return Nothing
  where
    go (s,o,n) 
        | length n == 1 = return $ Just $ OldBlock (last n) (length o - 1)
        | nodeChainWork (last n) > nodeChainWork (last o) = do
            setBestBlock $ last n
            return $ Just $ BlockReorg s o n
        | otherwise = return $ Just $ SideBlock (last n) (length n - 1)

-- | Find the split point between two nodes. It also returns the two partial
-- chains leading from the split point to the respective nodes.
findSplitNode :: BlockHeaderStore m => BlockHeaderNode -> BlockHeaderNode
              -> m (BlockHeaderNode, [BlockHeaderNode], [BlockHeaderNode])
findSplitNode n1 n2 = go [] [] n1 n2
  where
    go xs ys x y
        | nodeBlockHash x == nodeBlockHash y = return (x, x:xs, y:ys)
        | nodeHeaderHeight x > nodeHeaderHeight y = do
            par <- getParentNode x
            go (x:xs) ys par y
        | otherwise = do
            par <- getParentNode y
            go xs (y:ys) x par

-- | Find all blocks that need to be downloaded
blocksToDownload :: BlockHeaderStore m 
                 => m [(BlockHeight, BlockHash)]
blocksToDownload = do
    toDwn <- go [] =<< getBestBlockHeader
    return $ map f toDwn
  where
    f n = (nodeHeaderHeight n, nodeBlockHash n)
    go acc n | nodeHaveBlock n = return acc
             | otherwise = go (n:acc) =<< getParentNode n

{- LevelDB Instance of a HeaderChain -}

-- | Default LevelDB implementation of a HeaderChain
type LevelDBChain = S.StateT DB.DB IO

runLevelDBChain :: DB.DB -> LevelDBChain a -> IO a
runLevelDBChain db m = S.evalStateT m db

setLevelDBKey :: Binary a => BS.ByteString -> a -> LevelDBChain ()
setLevelDBKey key val = do
    db <- S.get
    DB.put db def key $ encode' val

setLevelDBKey' :: BS.ByteString -> BlockHeaderNode -> LevelDBChain ()
setLevelDBKey' key val = do
    db <- S.get
    DB.put db def key (indexKey $ nodeBlockHash val)

getLevelDBKey :: Binary a => BS.ByteString -> LevelDBChain a
getLevelDBKey key = do
    db <- S.get
    res <- DB.get db def key
    when (isNothing res) $ error "getLevelDBKey: Key does not exist"
    return $ fromJust $ decodeToMaybe =<< res

getLevelDBKey' :: Binary a => BS.ByteString -> LevelDBChain a
getLevelDBKey' key = do
    db <- S.get
    res <- DB.get db def key
    when (isNothing res) $ error "getLevelDBKey': Key does not exist"
    getLevelDBKey $ fromJust res

existsLevelDBKey :: BS.ByteString -> LevelDBChain Bool
existsLevelDBKey key = do
    db <- S.get
    res <- DB.get db def key
    return $ isJust res

indexKey :: BlockHash -> BS.ByteString
indexKey h = "index_" `BS.append` encode' h

instance BlockHeaderStore LevelDBChain where

    getBlockHeaderNode h    = getLevelDBKey $ indexKey h
    putBlockHeaderNode v    = setLevelDBKey (indexKey $ nodeBlockHash v) v
    existsBlockHeaderNode h = existsLevelDBKey $ indexKey h
    getBestBlockHeader      = getLevelDBKey' "bestblockheader"
    setBestBlockHeader v    = setLevelDBKey' "bestblockheader" v
    getBestBlock            = getLevelDBKey' "bestblock"
    setBestBlock v          = setLevelDBKey' "bestblock" v
    getFastCatchup          = getLevelDBKey "fastcatchup"
    setFastCatchup v        = setLevelDBKey "fastcatchup" v
    getFastCatchupHeight    = getLevelDBKey "fastcatchupheight"
    setFastCatchupHeight v  = setLevelDBKey "fastcatchupheight" v

    sourceBlockHeader = do
        db <- lift S.get
        snap <- lift $ DB.createSnapshot db
        it <- lift $ DB.createIter db def{ DB.useSnapshot = Just snap }
        lift $ DB.iterFirst it
        go it
        lift $ DB.releaseIter it
        lift $ DB.releaseSnapshot db snap
      where
        go it = do
            valM <- lift $ DB.iterValue it
            when (isJust valM) $ do
                let nodeM = decodeToMaybe $ fromJust valM
                when (isJust nodeM) $ C.yield $ fromJust nodeM
                lift $ DB.iterNext it 
                go it

