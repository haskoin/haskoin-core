{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.PeerManager
( runManager
) where

import System.Random

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkFinally, ThreadId, myThreadId)
import Control.Monad.Logger 
import Control.Monad.Trans
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Monad.Trans.Resource
import qualified Control.Monad.State as S

import Data.Maybe
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as Q
import Data.Conduit 
    ( Sink
    , awaitForever
    , ($$) 
    )
import Data.Conduit.Network 
    ( ClientSettings
    , runTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Database.LevelDB.Base as DB

import Database.Persist.Sql

import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Wallet
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util

type ManagerHandle = S.StateT ManagerSession (LoggingT IO)

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan (ThreadId, ManagerRequest)
    , peerMap          :: M.Map ThreadId PeerData
    , syncPeer         :: Maybe ThreadId
    , dbHandle         :: DB.DB
    , walletPool       :: ConnectionPool
    , blocksToDownload :: Q.Seq (Word32, Hash256)
    -- Transactions not received but advertised in a merkle block
    , inflightTxs      :: M.Map Hash256 UTCTime
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerHandshake  :: Bool
    , peerHeight     :: Word32
    , peerMsgChan    :: TBMChan Message
    -- TODO: Move to ManagerSession and key by hash. So that even if another
    -- peer downloads a inflight block, we still detect it and remove it.
    , inflightBlocks :: [(Word32, Hash256)]
    , inflightStart  :: Maybe UTCTime
    }

runManager :: ConnectionPool -> FilePath -> IO ()
runManager pool fp = do
    db <- DB.open fp
              DB.defaultOptions{ DB.createIfMissing = True
                               , DB.cacheSize       = 2048
                               }
    mChan <- atomically $ newTBMChan 1024
    vers  <- buildVersion
    let session = ManagerSession { mngrVersion      = vers
                                 , mngrChan         = mChan
                                 , peerMap          = M.empty
                                 , syncPeer         = Nothing
                                 , dbHandle         = db
                                 , walletPool       = pool
                                 , blocksToDownload = Q.empty
                                 , inflightTxs      = M.empty
                                 }

    runStdoutLoggingT $ flip S.evalStateT session $ do 

        $(logDebug) "Building list of merkle blocks to download..."

        -- Initialize the database
        toDwn <- runDB $ initDB >> getBlocksToDownload
        -- Put the missing blocks into the download queue
        S.modify $ \s -> s{ blocksToDownload = Q.fromList toDwn }

        -- Spin up some peer threads
        startPeer $ clientSettings 8333 "localhost"
        startPeer $ clientSettings 8333 "haskoin.com"

        -- Process messages
        -- TODO: Close database handle on exception with DB.close
        sourceTBMChan mChan $$ managerSink

startPeer :: ClientSettings -> ManagerHandle ()
startPeer remote = do
    vers  <- S.gets mngrVersion
    mChan <- S.gets mngrChan
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 1024
        writeTBMChan c $ MVersion vers
        return c
    $(logDebug) "Starting peer ..."
    tid   <- liftIO $ forkFinally 
        (runTCPClient remote $ peer pChan mChan) $ \ret -> do
        -- Thread cleanup
        tid <- myThreadId
        -- TODO: If the peer had some inflight blocks, move them to the 
        -- download queue.
        -- TODO: Check if the peer had pending work in pChan
        atomically $ do
            closeTBMChan pChan
            writeTBMChan mChan (tid, PeerDisconnect)

    let peerData = PeerData { peerHandshake = False
                            , peerMsgChan   = pChan
                            , peerHeight    = 0
                            , inflightBlocks = []
                            , inflightStart  = Nothing
                            }
    S.modify $ \s -> s{ peerMap = M.insert tid peerData (peerMap s) }

buildVersion :: IO Version
buildVersion = do
    -- TODO: Get our correct IP here
    let zeroAddr = (0x00, 0xffff00000000)
        add      = NetworkAddress 1 zeroAddr 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: Sink (ThreadId, ManagerRequest) ManagerHandle ()
managerSink = awaitForever $ \(tid,req) -> lift $ do
    exists <- peerExists tid
    -- Discard messages from peers that are gone
    -- TODO: Is this always the correct behavior ?
    when exists $ case req of
        PeerHandshake v -> processPeerHandshake tid v
        PeerDisconnect  -> processPeerDisconnect tid
        PeerMessage msg -> case msg of
            MHeaders headers   -> processHeaders tid headers
            MMerkleBlock block -> processMerkleBlock tid block
            _                  -> return () -- Ignore them for now

processPeerDisconnect :: ThreadId -> ManagerHandle ()
processPeerDisconnect tid = do
    $(logDebug) "Peer disconnected"
    deletePeerData tid
    syn <- S.gets syncPeer
    -- TODO: Implement a smarter algorithm for choosing the download peer
    -- Here, when the download peer dies, we just select the next one in
    -- the list of peers that has a remote peer version.
    when (syn == Just tid) $ do
        m <- S.gets peerMap
        -- Choose only peers that have finished the connection handshake
        let vals = filter (\(_,d) -> peerHandshake d) $ M.assocs m
            tidM = fst <$> listToMaybe vals
        S.modify $ \s -> s{ syncPeer = tidM }
        -- Continue the header download if we have a new peer
        when (isJust tidM) $ sendGetHeaders $ fromJust tidM

processPeerHandshake :: ThreadId -> Version -> ManagerHandle ()
processPeerHandshake tid remoteVer = do
    $(logDebug) "Peer connected"
    modifyPeerData tid $ \d -> d{ peerHandshake = True 
                                , peerHeight    = startHeight remoteVer
                                }
    -- Set the bloom filter for this connection
    bloom <- runWallet dbGetBloomFilter
    sendMessage tid $ MFilterLoad $ FilterLoad bloom

    -- TODO: Implement a smarter algorithm for choosing download peer. Here,
    -- we just select the first node that completed the remote peer handshake.
    syn <- S.gets syncPeer
    when (isNothing syn) $ do
        S.modify $ \s -> s{ syncPeer = Just tid }
        sendGetHeaders tid
    -- Download more block if some are pending
    downloadBlocks tid

processHeaders :: ThreadId -> Headers -> ManagerHandle ()
processHeaders tid (Headers h) = do
    -- TODO: The time here is incorrect. It should be a median of all peers.
    adjustedTime <- liftIO getPOSIXTime
    -- TODO: If a block headers can't be added, update DOS status
    (newBest, newToDwn) <- runDB $ do
        before <- bestHeaderHeight
        -- TODO: Handle errors in addBlockHeader. We only don't do anything
        -- in case of RejectHeader
        newToDwn <- forM (map fst h) $ \x -> do
            res <- addBlockHeader x $ round adjustedTime
            case res of
                -- Return the data that will be inserted at the end of
                -- the download queue
                (AcceptHeader n) -> 
                    return $ Just (nodeHeaderHeight n, nodeBlockId n)
                -- TODO: Remove? This is for debug only
                x -> do
                    liftIO $ print x
                    return Nothing
        after  <- bestHeaderHeight
        return $ (after > before, newToDwn)

    -- Add the blocks to download to the end of the queue
    -- TODO: Should we only download blocks if the headers is 
    -- a best header? (i.e. not a side block). Probably not but we
    -- need to consider the case when an old fork may become the
    -- new head.
    let newToDwnQ = Q.fromList $ catMaybes newToDwn
    S.modify $ \s -> s{ blocksToDownload = (blocksToDownload s) Q.>< newToDwnQ }

    -- Continue syncing from this node
    -- TODO: If this is another node than the sync node, we could have issues
    when newBest $ sendGetHeaders tid

    -- Request block downloads for all peers that are currently idling
    -- TODO: Even for the peer that is syncing the headers?
    tids <- M.keys <$> S.gets peerMap 
    forM_ tids downloadBlocks

processMerkleBlock :: ThreadId -> MerkleBlock -> ManagerHandle ()
processMerkleBlock tid b@(MerkleBlock h ntx hs fs) = do
    -- TODO: We read the node both here and in addMerkleBlock. It's duplication
    -- of work. Can we avoid it?
    nodeM <- runDB $ getBlockHeaderNode bid
    -- Ignore unsolicited merkle blocks
    when (isJust nodeM) $ do
        dat <- getPeerData tid
        -- Remove this merkle block from the inflight inventory
        let newInflight = filter (\(_,h) -> h /= bid) $ inflightBlocks dat
            newTime = if null newInflight then Nothing else inflightStart dat
        putPeerData tid dat{ inflightBlocks = newInflight
                           , inflightStart  = newTime
                           }

        when (ntx > 0) $ do
            let matchesE    = extractMatches fs hs $ fromIntegral ntx
                (root, txs) = fromRight matchesE
            -- TODO: Handle this error better
            when (isLeft matchesE) $
                error $ fromLeft matchesE
            -- TODO: Handle this error better
            when (root /= (merkleRoot $ nodeHeader $ fromJust nodeM)) $
                error "Invalid partial merkle tree received"
            -- TODO: The transactions txs are going to arrive now. Track them
            -- as we know they are valid if they passed the merkle checks.
        
        runDB $ addMerkleBlock b
        -- If this peer is done, get more merkle blocks to download
        when (null newInflight) $ downloadBlocks tid
  where
    bid = blockid h

-- Send out a GetHeaders request for a given peer
sendGetHeaders :: ThreadId -> ManagerHandle ()
sendGetHeaders tid = do
    d    <- getPeerData tid
    -- TODO: height is only for the log. Maybe remove it?
    -- TODO: Only build a block locator the first time. Then send the last
    -- known hash from the previous headers message
    (loc,height) <- runDB $ (,) <$> blockLocator <*> bestHeaderHeight
    sendMessage tid $ MGetHeaders $ GetHeaders 0x01 loc 0x00
    $(logInfo) $ T.pack $ unwords
        [ "Requesting block headers:"
        -- TODO: This can be negative if the remote note got a new block. More
        -- generally, we need to correctly track peerHeight when a remote peer
        -- receives new blocks
        , show $ (peerHeight d) - fromIntegral height 
        , "left to download."
        , show tid
        ]

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: ThreadId -> ManagerHandle ()
downloadBlocks tid = do
    -- Request block downloads if some are pending
    queue           <- S.gets blocksToDownload
    peerData        <- getPeerData tid
    let remoteHeight    = peerHeight peerData
        currentInflight = inflightBlocks peerData
        handshake       = peerHandshake peerData
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    -- TODO: The peer downloading the headers should not also download
    -- merkle blocks
    -- TODO: Only download blocks after the wallet first key timestamp
    when ((not $ Q.null queue) && handshake && null currentInflight) $ do
        let firstHeight   = fst $ Q.index queue 0
            (toDwn, rest) = Q.spanl f queue
            -- First 500 blocks that match the peer height requirements
            f (i,_) = i <= remoteHeight && i < firstHeight + 500
        when (not $ Q.null toDwn) $ do
            height <- runDB bestBlockHeight
            $(logInfo) $ T.pack $ unwords
                [ "Requesting merkle blocks:"
                -- TODO: This can be negative if the remote note got a new block. More
                -- generally, we need to correctly track peerHeight when a remote peer
                -- receives new blocks
                , show $ (peerHeight peerData) - fromIntegral height 
                , "left to download."
                , show tid
                ]
            let dwnList = toList toDwn
            currTime <- liftIO getCurrentTime
            modifyPeerData tid $ \d -> d{ inflightBlocks = dwnList
                                        , inflightStart  = Just currTime
                                        }
            S.modify $ \s -> s{ blocksToDownload = rest }
            sendMessage tid $ MGetData $ GetData $ 
                map ((InvVector InvMerkleBlock) . snd) dwnList

getPeerData :: ThreadId -> ManagerHandle PeerData
getPeerData tid = do
    m <- S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup tid m

putPeerData :: ThreadId -> PeerData -> ManagerHandle ()
putPeerData tid d = S.modify $ \s -> s{ peerMap = M.insert tid d (peerMap s) }

modifyPeerData :: ThreadId -> (PeerData -> PeerData) -> ManagerHandle ()
modifyPeerData tid f = do
    d <- getPeerData tid
    putPeerData tid $ f d

deletePeerData :: ThreadId -> ManagerHandle ()
deletePeerData tid = S.modify $ \s -> s{ peerMap = M.delete tid $ peerMap s }

peerExists :: ThreadId -> ManagerHandle Bool
peerExists tid = do
    m <- S.gets peerMap
    return $ M.member tid m

sendMessage :: ThreadId -> Message -> ManagerHandle ()
sendMessage tid msg = do
    d <- getPeerData tid
    -- The message is discarded if the channel is closed.
    liftIO . atomically $ writeTBMChan (peerMsgChan d) msg

runDB :: DBHandle a -> ManagerHandle a
runDB m = do
    db <- S.gets dbHandle
    liftIO $ S.evalStateT m $ LevelSession db

runWallet :: SqlPersistT (LoggingT (ResourceT IO)) a -> ManagerHandle a
runWallet m = do
    pool <- S.gets walletPool
    liftIO $ runResourceT $ runStderrLoggingT $ runSqlPool m pool

-- TODO: Remove this if not needed
splitIn :: Int -> [a] -> [[a]]
splitIn i xs 
    | i < 1 = error "Split count must be greater than 0"
    | otherwise = go i xs
  where
    len = length xs `div` i
    go 1 ls = [ls]
    go n ls = (take len ls) : (go (n-1) (drop len ls))
    
