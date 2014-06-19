{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.BlockChain where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)
import Control.Monad.Logger 

import Data.Word
import Data.Bits
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time 
    ( UTCTime(..)
    , Day(..)
    )
import qualified Data.ByteString as BS

import Database.Persist
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , getBy
    , selectList
    , insert_
    , delete
    )

import Network.Haskoin.Node.BlockStore
import Network.Haskoin.Node.Checkpoints
import Network.Haskoin.Node.Util
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

-- | Lower bound of proof of work difficulty
proofOfWorkLimit :: Integer
proofOfWorkLimit = fromIntegral (maxBound `shiftR` 32 :: Hash256)

-- | Time between difficulty cycles (2 weeks on average)
targetTimespan :: Int
targetTimespan = 14 * 24 * 60 * 60

-- | Time between blocks (10 minutes per block)
targetSpacing :: Int
targetSpacing = 10 * 60

-- | Number of blocks on average between difficulty cycles (2016 blocks)
diffInterval :: Int
diffInterval = targetTimespan `div` targetSpacing

-- | Are we running on testnet ?
isTestnet :: Bool
isTestnet = False

data BlockChainAction b
    = BestBlock  { getBestBlock    :: DbBlockHeaderGeneric b }
    | SideBlock  { getSideBlock    :: DbBlockHeaderGeneric b }
    | BlockReorg { reorgSplitPoint :: DbBlockHeaderGeneric b
                 , reorgOldBlocks  :: [DbBlockHeaderGeneric b]
                 , reorgNewBlocks  :: [DbBlockHeaderGeneric b]
                 }
    | NewOrphan  { getNewOrphan    :: DbOrphanHeaderGeneric b }
    deriving (Show, Eq)
 
chainInit :: PersistUnique m => m ()
chainInit = do
    isInit <- dbInit
    when (not isInit) $ do
        -- Insert genesis block into the database
        gen <- dbPutHeader $ DbBlockHeader genId parId 0 work 1 genesis nullTime
        insert_ $ DbChainHeight genId (dbBlockHeaderCreated gen)
  where
    genId = blockid genesis
    parId = prevBlock genesis
    work  = blockWork genesis

dbBestHeight :: (PersistQuery m, PersistUnique m) 
             => m Word32
dbBestHeight = liftM (fromIntegral . dbBlockHeaderHeight) dbGetChainHead

-- We don't implement the exponential version as scanning all the block
-- headers takes time. We add the last 100 headers in the block locator 
-- with the genesis block. If there is a reorg greater than 100 blocks,
-- we will end up downloading the entire chain.
dbBlockLocator :: (MonadLogger m, PersistQuery m, PersistUnique m)
               => m BlockLocator
dbBlockLocator = do
    chainHead <- dbGetChainHead
    loc  <- goback (100 :: Int) [] chainHead
    if (head loc) == (blockid genesis)
        then return $ reverse loc
        else return $ reverse $ (blockid genesis) : loc
  where
    goback 1 acc e = return $ (dbBlockHeaderHash e):acc
    goback c acc e = do
        parM <- dbGetParent e
        case parM of
            Just par -> goback (c-1) ((dbBlockHeaderHash e):acc) par
            Nothing  -> return $ (dbBlockHeaderHash e):acc

dbAddBlock :: ( MonadLogger m
              , PersistQuery m
              , PersistUnique m
              , b ~ PersistMonadBackend m
              )
           => BlockHeader
           -> Word32
           -> m [BlockChainAction b]
dbAddBlock block adjustedTime = dbAddBlock' block adjustedTime True

-- TODO: Review block header rules according to headersfirst rules (no orphans)
-- See AcceptBlockHeader in Sipa's branch.
dbAddBlock' :: ( MonadLogger m
               , PersistQuery m
               , PersistUnique m
               , b ~ PersistMonadBackend m
               )
            => BlockHeader
            -> Word32
            -> Bool
            -> m [BlockChainAction b]
dbAddBlock' block adjustedTime connect = do
    bHead <- dbGetChainHead 
    when (dbBlockHeaderHash bHead == blockid block) $ liftIO $ throwIO $
        BlockChainException $ unwords
            [ "The header"
            , bsToHex $ encode' $ blockid block
            , "is already the chain head"
            ]
    when connect $ do
        orphM <- dbGetOrphan $ blockid block
        when (isJust orphM) $ liftIO $ throwIO $
            BlockChainException $ unwords
                [ "The header"
                , bsToHex $ encode' $ blockid block
                , "is in the orphan pool"
                ]
    unless (verifyBlockHeader block adjustedTime) $ liftIO $ throwIO $
        BlockChainException $ unwords
            [ "The Header"
            , bsToHex $ encode' $ blockid block
            , "failed verification"  
            ]
    prevM <- dbGetHeader $ prevBlock block
    case prevM of
        Just prev -> do
            -- Check the difficulty transition
            requiredWork <- dbNextWorkRequired prev
            unless (blockBits block == requiredWork) $ liftIO $ throwIO $
                BlockChainException $ unwords
                    [ "The header"
                    , bsToHex $ encode' $ blockid block
                    , "failed work transition verification"
                    ]
            actions <- dbConnectBlock prev block
            if connect
                then do
                    orphanActions <- dbConnectOrphans adjustedTime
                    return $ actions ++ orphanActions
                else return actions
        Nothing -> do
            newOrphan <- dbPutOrphan block
            return [NewOrphan newOrphan]

dbConnectOrphans :: ( MonadLogger m
                    , PersistQuery m
                    , PersistUnique m
                    , b ~ PersistMonadBackend m
                    )
                 => Word32
                 -> m [BlockChainAction b]
dbConnectOrphans adjustedTime = do
    orphans <- selectList [] []
    res <- forM orphans $ \(Entity key orphan) -> do
        parEnt <- getBy $ BlockHash $ dbOrphanHeaderParent orphan
        if isNothing parEnt then return [] else do
            let block = dbOrphanHeaderValue orphan
            actions <- dbAddBlock' block adjustedTime False
            delete key
            return actions
    f $ concat res
  where
    f [] = return []
    f xs = liftM (xs ++) $ dbConnectOrphans adjustedTime
    
verifyBlockHeader :: BlockHeader -> Word32 -> Bool
verifyBlockHeader bh adjustedTime =
    checkProofOfWork bh && blockTimestamp bh <= adjustedTime + 2 * 60 * 60

getProofOfWork :: BlockHeader -> Integer
getProofOfWork =  bsToInteger . BS.reverse . encode' . blockid

checkProofOfWork :: BlockHeader -> Bool
checkProofOfWork bh
    | target <= 0 || target > proofOfWorkLimit = False
    | otherwise = getProofOfWork bh <= fromIntegral target
  where
    target = decodeCompact $ blockBits bh

-- | Compute the work required for the next block. If a difficulty jump is 
-- required, this function will go back into the block header history and
-- compute the correct value.
dbNextWorkRequired :: ( MonadLogger m
                      , PersistUnique m
                      , b ~ PersistMonadBackend m
                      )
                   => DbBlockHeaderGeneric b
                   -> m Word32
dbNextWorkRequired lastB
    | ((lastHeight + 1) `mod` diffInterval) /= 0 = do
        return $ blockBits lastBlock
    | otherwise = do
        first <- goback (diffInterval-1) lastB
        let firstBlock = dbBlockHeaderValue first
        return $ getNewWork firstBlock lastBlock
  where
    lastHeight = dbBlockHeaderHeight lastB
    lastBlock  = dbBlockHeaderValue lastB
    goback 0 h = return h
    goback c h = do
        parM <- dbGetParent h
        when (isNothing parM) $ liftIO $ throwIO $
            BlockChainException "dbNextWorkRequired: block has no parent"
        goback (c-1) $ fromJust parM

-- | Given two block headers, compute the work required for the block following
-- the second block. The two input blocks should be spaced out by the number of
-- blocks between difficulty jumps (2016 in prodnet). 
getNewWork :: BlockHeader -> BlockHeader -> Word32
getNewWork firstB lastB
    | newDiff > proofOfWorkLimit = encodeCompact proofOfWorkLimit
    | otherwise                  = encodeCompact newDiff
  where
    t = fromIntegral $ (blockTimestamp lastB) - (blockTimestamp firstB)
    actualTime 
        | t < targetTimespan `div` 4 = targetTimespan `div` 4
        | t > targetTimespan * 4     = targetTimespan * 4
        | otherwise                  = t
    lastDiff = decodeCompact $ blockBits lastB
    newDiff = lastDiff * (toInteger actualTime) `div` (toInteger targetTimespan)

dbConnectBlock :: ( MonadLogger m
                  , PersistUnique m
                  , PersistQuery m
                  , b ~ PersistMonadBackend m
                  )
               => DbBlockHeaderGeneric b
               -> BlockHeader 
               -> m [BlockChainAction b]
dbConnectBlock prev block = do
    unless (verifyCheckpoint (prevHeight + 1) bid) $ liftIO $ throwIO $
        BlockChainException $ unwords 
            [ "Block failed checkpoint at height"
            , show $ prevHeight + 1
            ]
    chainHead <- dbGetChainHead
    if (dbBlockHeaderHash prev) == (dbBlockHeaderHash chainHead)
        then do
            newHead <- dbPutHeader newBlock
            dbSetChainHead $ dbBlockHeaderHash newHead
            return [BestBlock newHead]
        else do
            let newWork  = dbBlockHeaderChainWork newBlock
                headWork = dbBlockHeaderChainWork chainHead
            if newWork > headWork
                then dbHandleNewBestChain newBlock 
                else do
                    split <- dbFindSplit newBlock chainHead
                    if dbBlockHeaderHash split /= dbBlockHeaderHash newBlock
                        then do
                            newSide <- dbPutHeader newBlock
                            return [SideBlock newSide]
                        else return []
  where
    newBlock   = buildNextBlock prev block 1
    prevHeight = dbBlockHeaderHeight prev
    bid        = blockid block

-- | Handle a reorganization of the blockchain
dbHandleNewBestChain :: ( MonadLogger m
                        , PersistUnique m
                        , PersistQuery m
                        , b ~ PersistMonadBackend m
                        )
                     => DbBlockHeaderGeneric b
                     -> m [BlockChainAction b]
dbHandleNewBestChain newChainHead = do
    chainHead  <- dbGetChainHead
    splitPoint <- dbFindSplit newChainHead chainHead
    newHead    <- dbPutHeader newChainHead
    dbSetChainHead $ dbBlockHeaderHash newHead
    oldBlocks  <- getPartialChain chainHead splitPoint
    newBlocks  <- getPartialChain newHead splitPoint
    return [BlockReorg splitPoint oldBlocks newBlocks]

-- | Construct the next stored block header given a previous stored block 
-- and the new block header.
buildNextBlock :: DbBlockHeaderGeneric b 
               -> BlockHeader 
               -> Int
               -> DbBlockHeaderGeneric b
buildNextBlock prev block txCnt = 
    DbBlockHeader bid prevHash newHeight newWork txCnt block nullTime
  where
    prevWork  = dbBlockHeaderChainWork prev
    newWork   = prevWork + blockWork block
    prevHash  = dbBlockHeaderHash prev
    newHeight = dbBlockHeaderHeight prev + 1
    bid       = blockid block

-- | Returns the work represented by this block. Work is defined as the number 
-- of tries needed to solve a block in the average case with respect to the
-- target.
blockWork :: BlockHeader -> Integer
blockWork bh = 
    largestHash `div` (target + 1)
  where
    target      = decodeCompact (blockBits bh)
    largestHash = 1 `shiftL` 256

dbFindSplit :: (MonadLogger m, PersistUnique m, b ~ PersistMonadBackend m)
            => DbBlockHeaderGeneric b 
            -> DbBlockHeaderGeneric b
            -> m (DbBlockHeaderGeneric b)
dbFindSplit c1 c2
    | dbBlockHeaderHash c1 == dbBlockHeaderHash c2 = return c1
    | dbBlockHeaderHeight c1 > dbBlockHeaderHeight c2 = do
        parM <- dbGetParent c1
        when (isNothing parM) $ liftIO $ throwIO $
            BlockChainException "dbFindSplit: Block has no parent"
        dbFindSplit (fromJust parM) c2
    | otherwise = do
        parM <- dbGetParent c2
        when (isNothing parM) $ liftIO $ throwIO $
            BlockChainException "dbFindSplit: Block has no parent"
        dbFindSplit c1 (fromJust parM)

getPartialChain :: (MonadLogger m, PersistUnique m, b ~ PersistMonadBackend m)
                => DbBlockHeaderGeneric b
                -> DbBlockHeaderGeneric b
                -> m [DbBlockHeaderGeneric b]
getPartialChain higher lower
    | hHeight > lHeight = do
        parM <- dbGetParent higher
        when (isNothing parM) $ liftIO $ throwIO $
            BlockChainException "getPartialChain: Block has no parent"
        liftM (higher :) $ getPartialChain (fromJust parM) lower
    | dbBlockHeaderHash higher == dbBlockHeaderHash lower = return []
    | otherwise = liftIO $ throwIO $ 
        BlockChainException "getPartialChain: Lower is higher than Higher"
  where
    hHeight = dbBlockHeaderHeight higher
    lHeight = dbBlockHeaderHeight lower

dbGetParent :: (MonadLogger m, PersistUnique m, b ~ PersistMonadBackend m)
            => DbBlockHeaderGeneric b
            -> m (Maybe (DbBlockHeaderGeneric b))
dbGetParent block 
    | dbBlockHeaderHash block == blockid genesis = return Nothing
    | otherwise = do
        resM <- getBy $ BlockHash par
        return $ entityVal <$> resM
  where
    par    = dbBlockHeaderParent block

dbGetOrphanRoot :: (PersistUnique m, b ~ PersistMonadBackend m)
                => (DbOrphanHeaderGeneric b) -> m (DbOrphanHeaderGeneric b)
dbGetOrphanRoot orphan = do
    parM <- dbGetOrphan $ dbOrphanHeaderParent orphan
    if isJust parM
        then dbGetOrphanRoot $ fromJust parM
        else return orphan

genesis :: BlockHeader
genesis = BlockHeader 
            (fromIntegral $ genesisHeader !! 0)
            (fromIntegral $ genesisHeader !! 1)
            (fromIntegral $ genesisHeader !! 2)
            (fromIntegral $ genesisHeader !! 3)
            (fromIntegral $ genesisHeader !! 4)
            (fromIntegral $ genesisHeader !! 5)

nullTime :: UTCTime
nullTime = UTCTime (ModifiedJulianDay 0) 0

