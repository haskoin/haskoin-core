module Network.Haskoin.Block.HeaderTree 
( HeaderTree(..)
, BlockHeaderNode(..)
, BlockChainAction(..)
, initHeaderTree
, connectHeader
, connectHeaders
, commitAction
, actionNewNodes
, isBestChain
, isChainReorg
, isSideChain
, blockLocator
, partialLocator
, getNodeWindow
, bestBlockHeaderHeight
, getBlockHeaderHeight
, genesisNode
, getParentNode
) where

import Control.Monad (foldM, when, unless, liftM, (<=<), forM, forM_)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Either (EitherT, left, runEitherT)
import Control.Monad.State (MonadState(..), StateT, get)

import Data.Word (Word32)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import Data.List (sort)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Default (def)
import qualified Data.Binary as B (Binary, get, put)
import qualified Data.ByteString as BS (ByteString, reverse, append)

import qualified Database.LevelDB.Base as L (DB, get, put)

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Checkpoints
import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

class Monad m => HeaderTree m where
    getBlockHeaderNode     :: BlockHash -> m (Maybe BlockHeaderNode)
    putBlockHeaderNode     :: BlockHeaderNode -> m ()
    -- The height is only updated when the node is part of the main chain.
    -- Side chains are not indexed by their height.
    putBlockHeaderHeight   :: BlockHeaderNode -> m ()
    getBlockHeaderByHeight :: BlockHeight -> m (Maybe BlockHeaderNode)
    getBestBlockHeader     :: m BlockHeaderNode
    setBestBlockHeader     :: BlockHeaderNode -> m ()

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

-- | Returns True if the action is a best chain
isBestChain :: BlockChainAction -> Bool
isBestChain (BestChain _) = True
isBestChain _             = False

-- | Returns True if the action is a chain reorg
isChainReorg :: BlockChainAction -> Bool
isChainReorg (ChainReorg _ _ _) = True
isChainReorg _                  = False

-- | Returns True if the action is a side chain
isSideChain :: BlockChainAction -> Bool
isSideChain (SideChain _) = True
isSideChain _             = False

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
initHeaderTree :: HeaderTree m => m ()
initHeaderTree = do
    genM <- getBlockHeaderNode $ nodeBlockHash genesisNode
    when (isNothing genM) $ do
        putBlockHeaderNode genesisNode
        setBestBlockHeader genesisNode

-- A more efficient way of connecting a list of BlockHeaders than connecting
-- them individually. The work check will only be done once for the whole
-- chain. The list of BlockHeaders have to form a valid chain, linked by their
-- parents.
connectHeaders :: HeaderTree m
               => [BlockHeader]
               -> Timestamp
               -> Bool
               -> m (Either String BlockChainAction)
connectHeaders bhs adjustedTime commit
    | null bhs = return $ Left "Invalid empty BlockHeaders in connectHeaders"
    | validChain bhs = runEitherT $ do
        newNodes <- forM bhs $ \bh -> do
            parNode <- verifyBlockHeader bh adjustedTime
            lift $ storeBlockHeader bh parNode
        -- Best header will only be updated if we have no errors
        lift $ evalNewChain commit $ last newNodes
    | otherwise = return $ Left "BlockHeaders do not form a valid chain."
  where
    validChain (a:b:xs) =  prevBlock b == headerHash a && validChain (b:xs)
    validChain (_:[]) = True
    validChain _ = False

-- | Connect a block header to this block header chain. Corresponds to bitcoind
-- function ProcessBlockHeader and AcceptBlockHeader in main.cpp.
connectHeader :: HeaderTree m 
              => BlockHeader
              -> Timestamp
              -> Bool
              -> m (Either String BlockChainAction)
connectHeader bh adjustedTime commit = runEitherT $ do
    parNode <- verifyBlockHeader bh adjustedTime
    lift $ evalNewChain commit =<< storeBlockHeader bh parNode

evalNewChain :: HeaderTree m 
             => Bool -> BlockHeaderNode -> m BlockChainAction
evalNewChain commit newNode = do
    currentHead <- getBestBlockHeader
    action <- go =<< findSplitNode currentHead newNode
    when commit $ commitAction action
    return action
  where
    go (split, old, new)
        | length old == 0 && length new >= 1 = return $ BestChain new
        | length old > 0 && length new > 0 && 
          nodeChainWork (last new) > nodeChainWork (last old) = 
            return $ ChainReorg split old new
        | otherwise = return $ SideChain new

-- | Update the best block header of the action in the header tree
commitAction :: HeaderTree m => BlockChainAction -> m ()
commitAction action = do
    currentHead <- getBestBlockHeader
    case action of
        BestChain nodes -> unless (null nodes) $ do
            updateChildren $ currentHead:nodes
            forM_ nodes putBlockHeaderHeight
            setBestBlockHeader $ last nodes
        ChainReorg s _ ns -> unless (null ns) $ do
            updateChildren $ s:ns
            forM_ ns putBlockHeaderHeight
            setBestBlockHeader $ last ns
        SideChain _ -> return ()
  where
    updateChildren (a:b:xs) = do
        putBlockHeaderNode a{ nodeChild = Just $ nodeBlockHash b }
        updateChildren (b:xs)
    updateChildren _ = return ()
    
-- TODO: Add DOS return values
verifyBlockHeader :: HeaderTree m 
                  => BlockHeader 
                  -> Timestamp
                  -> EitherT String m BlockHeaderNode
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
storeBlockHeader :: HeaderTree m 
                 => BlockHeader 
                 -> BlockHeaderNode
                 -> m BlockHeaderNode
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
getNodeWindow :: HeaderTree m 
              => BlockHash -> Int -> m (Maybe BlockChainAction)
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
findSplitNode :: HeaderTree m
              => BlockHeaderNode 
              -> BlockHeaderNode
              -> m (BlockHeaderNode, [BlockHeaderNode], [BlockHeaderNode])
findSplitNode n1 n2 = 
    go [] [] n1 n2
  where
    go xs ys x y
        | nodeBlockHash x == nodeBlockHash y = return (x, xs, ys)
        | nodeHeaderHeight x > nodeHeaderHeight y = do
            par <- liftM fromJust $ getParentNode x
            go (x:xs) ys par y
        | otherwise = do
            par <- liftM fromJust $ getParentNode y
            go xs (y:ys) x par

-- | Finds the parent of a BlockHeaderNode
getParentNode :: HeaderTree m => BlockHeaderNode -> m (Maybe BlockHeaderNode)
getParentNode node
    | p == 0    = return Nothing
    | otherwise = getBlockHeaderNode p
  where
    p = prevBlock $ nodeHeader node

-- | Finds the child of a BlockHeaderNode if it exists. If a node has
-- multiple children, this function will always return the child on the
-- main branch.
getChildNode :: HeaderTree m => BlockHeaderNode -> m (Maybe BlockHeaderNode)
getChildNode node = case nodeChild node of
    Just child -> getBlockHeaderNode child
    Nothing    -> return Nothing

-- | Get the last checkpoint that we have seen
lastSeenCheckpoint :: HeaderTree m => m (Maybe (Int, BlockHash))
lastSeenCheckpoint = 
    go $ reverse checkpointList
  where
    go ((i, chk):xs) = do
        existsChk <- liftM isJust $ getBlockHeaderNode chk
        if existsChk then return $ Just (i, chk) else go xs
    go [] = return Nothing

-- | Returns the work required for a BlockHeader given the previous
-- BlockHeaderNode. This function coresponds to bitcoind function
-- GetNextWorkRequired in main.cpp.
nextWorkRequired :: HeaderTree m => BlockHeaderNode -> BlockHeader -> m Word32
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
    fs    = replicate len (liftM fromJust . getParentNode)
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

-- | Returns a BlockLocator object (newest block first, genesis at the end)
blockLocator :: HeaderTree m => m BlockLocator
blockLocator = do
    h <- liftM fromIntegral $ bestBlockHeaderHeight
    -- Take only indices > 0 to avoid the genesis block
    let is = takeWhile (> (0 :: Int)) $ 
            [h,(h-1)..(h-9)] ++ [(h-10) - 2^x | x <- [(0 :: Int)..]]
    ns <- liftM catMaybes $ forM (map fromIntegral is) getBlockHeaderByHeight
    return $ (map nodeBlockHash ns) ++ [headerHash genesisHeader]

-- | Returns a partial BlockLocator object.
partialLocator :: HeaderTree m => Int -> m BlockLocator
partialLocator i 
    | i < 1 = error "Locator length must be greater than 0"
    | otherwise = do
        h <- getBestBlockHeader
        liftM (map nodeBlockHash . reverse) $ go [] i h
  where
    go acc 1 node = return $ node:acc
    go acc step node = getParentNode node >>= \parM -> case parM of
        Just par -> go (node:acc) (step - 1) par
        Nothing  -> return $ node:acc

bestBlockHeaderHeight :: HeaderTree m => m BlockHeight
bestBlockHeaderHeight = liftM nodeHeaderHeight getBestBlockHeader

getBlockHeaderHeight :: HeaderTree m => BlockHash -> m (Maybe BlockHeight)
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

blockHashKey :: BlockHash -> BS.ByteString
blockHashKey bid = "b_" `BS.append` (encode' bid)

bestBlockKey :: BS.ByteString
bestBlockKey = "bestblockheader_"

heightKey :: BlockHeight -> BS.ByteString
heightKey h = "h_" `BS.append` (encode' h)

-- Get a node which is directly referenced by the key
getLevelDBNode :: MonadIO m
               => BS.ByteString -> StateT L.DB m (Maybe BlockHeaderNode)
getLevelDBNode key = do
    db <- get
    resM <- liftIO $ L.get db def key
    return $ decodeToMaybe =<< resM

-- Get a node that has 1 level of indirection
getLevelDBNode' :: MonadIO m
                => BS.ByteString -> StateT L.DB m (Maybe BlockHeaderNode)
getLevelDBNode' key = do
    db <- get
    resM <- liftIO $ L.get db def key
    maybe (return Nothing) getLevelDBNode resM

instance MonadIO m => HeaderTree (StateT L.DB m) where
    getBlockHeaderNode = getLevelDBNode . blockHashKey
    putBlockHeaderNode node = do
        db <- get
        liftIO $ L.put db def (blockHashKey $ nodeBlockHash node) $ encode' node
    getBlockHeaderByHeight = getLevelDBNode' . heightKey
    putBlockHeaderHeight node = do
        db <- get 
        let val = blockHashKey $ nodeBlockHash node
        liftIO $ L.put db def (heightKey $ nodeHeaderHeight node) val
    getBestBlockHeader = do
        db <- get
        keyM <- liftIO $ L.get db def bestBlockKey
        case keyM of
            Just key -> liftM fromJust $ getLevelDBNode key
            Nothing  -> error 
                "GetBestBlockHeader: Best block header does not exist"
    setBestBlockHeader node = do
        db <- get
        liftIO $ L.put db def bestBlockKey $ blockHashKey $ nodeBlockHash node

