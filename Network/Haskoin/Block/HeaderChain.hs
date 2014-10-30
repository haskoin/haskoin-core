module Network.Haskoin.Block.HeaderChain 
( BlockHeaderNode(..)
, BlockHeaderStore(..)
, BlockHeaderAction(..)
, BlockChainAction(..)
, getActionNode
, genesisBlockHeaderNode
, initHeaderChain
, connectBlockHeader
, connectBlock
, downloadBlockHeaders
, blockLocator
, rescanFrom
, bestBlockHeaderHeight
, bestBlockHeight
, lastSeenCheckpoint
, findSplitNode
, getParentNode
, nextWorkRequired
, workFromInterval
, isValidPOW
, headerPOW
, headerWork
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, when, unless, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (left, runEitherT)

import Data.Word (Word32)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isNothing)
import Data.List (sort, nub)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import qualified Data.ByteString as BS (reverse)

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
    , nodeHeaderHeight :: !Word32
    , nodeChainWork    :: !Integer
    , nodeChild        :: !(Maybe BlockHash)
    , nodeMedianTimes  :: ![Word32]
    , nodeMinWork      :: !Word32 -- Only used for testnet
    } deriving (Show, Read, Eq)

instance Binary BlockHeaderNode where

    get = BlockHeaderNode <$> get 
                          <*> get
                          <*> getWord32le 
                          <*> get
                          <*> get
                          <*> get
                          <*> get

    put (BlockHeaderNode i b h w c t m) = do
        put i 
        put b 
        putWord32le h 
        put w 
        put c
        put t 
        put m

-- Return value of linking a new block header in the chain
-- TODO: Add more options if required
data BlockHeaderAction
    = RejectHeader String
    | HeaderAlreadyExists BlockHeaderNode
    | AcceptHeader BlockHeaderNode
    deriving (Show, Read, Eq)

data BlockChainAction
    = BestBlock  { actionBestBlock :: BlockHeaderNode }
    | SideBlock  { actionSideBlock :: BlockHeaderNode }
    | BlockReorg { reorgSplitPoint :: BlockHeaderNode
                 , reorgOldBlocks  :: [BlockHeaderNode]
                 , reorgNewBlocks  :: [BlockHeaderNode]
                 }
    deriving (Read, Show, Eq)

getActionNode :: BlockChainAction -> BlockHeaderNode
getActionNode a = case a of
    BestBlock n -> n
    SideBlock n -> n
    BlockReorg _ _ ns -> last ns

class Monad m => BlockHeaderStore m where
    getBlockHeaderNode    :: BlockHash -> m BlockHeaderNode
    putBlockHeaderNode    :: BlockHeaderNode -> m ()
    existsBlockHeaderNode :: BlockHash -> m Bool
    getBestBlockHeader    :: m BlockHeaderNode
    setBestBlockHeader    :: BlockHeaderNode -> m ()
    getBestBlock          :: m BlockHeaderNode
    setBestBlock          :: BlockHeaderNode -> m ()
    getLastDownload       :: m BlockHeaderNode
    setLastDownload       :: BlockHeaderNode -> m ()

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
    , nodeChild        = Nothing
    , nodeMedianTimes  = [blockTimestamp genesisHeader]
    , nodeMinWork      = blockBits genesisHeader
    }

-- TODO: If dwnStart is not equal to the one in the database, issue a warning
-- or an error.
-- | Initialize the block header chain by inserting the genesis block if
-- it doesn't already exist.
initHeaderChain :: BlockHeaderStore m => m ()
initHeaderChain = do
    existsGen <- existsBlockHeaderNode $ nodeBlockHash genesisBlockHeaderNode
    unless existsGen $ do
        putBlockHeaderNode genesisBlockHeaderNode
        setBestBlockHeader genesisBlockHeaderNode
        setBestBlock       genesisBlockHeaderNode
        setLastDownload    genesisBlockHeaderNode

-- TODO: Add DOS return values
-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectBlockHeader :: BlockHeaderStore m 
                   => BlockHeader 
                   -> Word32 
                   -> Word32
                   -> m BlockHeaderAction
connectBlockHeader bh adjustedTime fastCatchup = ((liftM f) . runEitherT) $ do
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
    lift $ storeBlockHeader prevNode bh fastCatchup
  where
    f (Right x) = x
    f (Left  x) = x
    bid = headerHash bh

storeBlockHeader :: BlockHeaderStore m 
                 => BlockHeaderNode 
                 -> BlockHeader 
                 -> Word32
                 -> m BlockHeaderAction
storeBlockHeader prevNode bh fastCatchup = do
    putBlockHeaderNode newNode
    putBlockHeaderNode $ prevNode{ nodeChild = Just bid }
    currentHead <- getBestBlockHeader
    when (newWork > nodeChainWork currentHead) $ do
        setBestBlockHeader newNode
        -- Update the block head if we are before the fast catchup time
        -- By setting a new best block, we sort of flag it as downloaded
        when (blockTimestamp bh < fastCatchup) $ do
            setBestBlock newNode
            setLastDownload newNode
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
    newNode = BlockHeaderNode { nodeBlockHash    = bid
                              , nodeHeader       = bh
                              , nodeHeaderHeight = newHeight
                              , nodeChainWork    = newWork
                              , nodeChild        = Nothing
                              , nodeMedianTimes  = newMedian
                              , nodeMinWork      = minWork
                              }

-- | Returns a list of BlockHash that needs to be downloaded.
downloadBlockHeaders :: BlockHeaderStore m => Int -> m [BlockHash]
downloadBlockHeaders count = do
    n <- getLastDownload
    (res, lstDwn) <- go [] count n
    setLastDownload lstDwn
    return $ reverse res
  where
    go acc step n
        | step == 0 = return (acc, n)
        | isNothing $ nodeChild n = do
            bestHead <- getBestBlockHeader
            -- The pointer could be stuck in an orphaned fork
            if nodeChainWork bestHead > nodeChainWork n
                then do
                    (split,_,_) <- findSplitNode bestHead n
                    go ((nodeBlockHash split):acc) (step-1) split
                else return (acc, n)
        | otherwise = do
            c <- getBlockHeaderNode $ fromJust $ nodeChild n
            go ((nodeBlockHash c):acc) (step-1) c 

-- | Connect a block to blockchain. Blocks need to be imported in the order
-- of the parent links (oldest to newest).
connectBlock :: BlockHeaderStore m => BlockHeader -> m BlockChainAction
connectBlock bh = do
    newNode   <- getBlockHeaderNode bid
    bestBlock <- getBestBlock
    if prevBlock (nodeHeader newNode) == nodeBlockHash bestBlock
        -- We connect to the best chain
        then do
            setBestBlock newNode
            return $ BestBlock newNode
        else if nodeChainWork newNode > nodeChainWork bestBlock
                 then handleNewBestChain newNode bestBlock
                 else return $ SideBlock newNode
  where
    bid = headerHash bh

handleNewBestChain :: BlockHeaderStore m 
                   => BlockHeaderNode -> BlockHeaderNode -> m BlockChainAction
handleNewBestChain newChainHead oldChainHead = do
    (splitPoint, oldChain, newChain) <- findSplitNode oldChainHead newChainHead
    setBestBlock newChainHead
    return $ BlockReorg splitPoint oldChain newChain

-- | Get the last checkpoint that we have seen
lastSeenCheckpoint :: BlockHeaderStore m => m (Maybe (Int, BlockHash))
lastSeenCheckpoint = 
    foldM f Nothing $ reverse checkpointList
  where
    f res@(Just _) _  = return res
    f Nothing (i,chk) = do
        existsChk <- existsBlockHeaderNode chk
        return $ if existsChk then Just (i,chk) else Nothing

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
workFromInterval :: Word32 -> BlockHeader -> Word32
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

bestBlockHeaderHeight :: BlockHeaderStore m => m Word32
bestBlockHeaderHeight = liftM nodeHeaderHeight getBestBlockHeader

bestBlockHeight :: BlockHeaderStore m => m Word32
bestBlockHeight = liftM nodeHeaderHeight getBestBlock

-- | Reset the BestBlock and LastDownload pointers to a block prior to the
-- given timestamp
rescanFrom :: BlockHeaderStore m => Word32 -> m ()
rescanFrom t = do
    -- Find the position of the new best header and download pointer
    currentHead <- getBestBlockHeader
    newHead <- findNewBestBlock t currentHead
    setBestBlock newHead
    setLastDownload newHead
  where
    findNewBestBlock fastCatchup n
        | prevBlock (nodeHeader n) == 0 = return n
        | blockTimestamp (nodeHeader n) < fastCatchup = return n
        | otherwise = findNewBestBlock fastCatchup =<< getParentNode n

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

