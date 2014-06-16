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
import Data.Time.Clock.POSIX
import qualified Data.Map as M
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
import Network.Haskoin.Protocol
import Network.Haskoin.Util

type ManagerHandle = S.StateT ManagerSession (LoggingT IO)

data ManagerSession = ManagerSession
    { mngrVersion :: Version
    , mngrChan    :: TBMChan (ThreadId, ManagerRequest)
    , peerMap     :: M.Map ThreadId PeerData
    , syncPeer    :: Maybe ThreadId
    , dbHandle    :: DB.DB
    , walletPool  :: ConnectionPool
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerHandshake :: Bool
    , peerHeight    :: Word32
    , peerMsgChan   :: TBMChan Message
    }

runManager :: ConnectionPool -> FilePath -> IO ()
runManager pool fp = do
    db <- DB.open fp
              DB.defaultOptions{ DB.createIfMissing = True
                               , DB.cacheSize       = 2048
                               }
    mChan <- atomically $ newTBMChan 1024
    vers  <- buildVersion
    let session = ManagerSession { mngrVersion = vers
                                 , mngrChan    = mChan
                                 , peerMap     = M.empty
                                 , syncPeer    = Nothing
                                 , dbHandle    = db
                                 , walletPool  = pool
                                 }

    runStdoutLoggingT $ flip S.evalStateT session $ do 

        -- Initialize the database
        runDB initDB

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
        -- TODO: remote this or move it into the PeerDisconnect message to log
        print ret
        -- Thread cleanup
        tid <- myThreadId
        -- TODO: Check if the peer had pending work
        atomically $ do
            closeTBMChan pChan
            writeTBMChan mChan (tid, PeerDisconnect)

    let peerData = PeerData { peerHandshake = False
                            , peerMsgChan   = pChan
                            , peerHeight    = 0
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
            MHeaders headers -> processHeaders tid headers
            _                -> return () -- Ignore them for now

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

processHeaders :: ThreadId -> Headers -> ManagerHandle ()
processHeaders tid (Headers h) = do
    -- TODO: The time here is incorrect. It should be a median of all peers.
    adjustedTime <- liftIO getPOSIXTime
    -- TODO: If a block headers can't be added, update DOS status
    newBest <- runDB $ do
        before <- bestHeight
        -- TODO: Handle errors in addBlockHeader
        forM_ (map fst h) $ \x -> do
            res <- addBlockHeader x $ round adjustedTime
            -- TODO: This is for debug only
            case res of
                (AcceptHeader _) -> return ()
                x                -> liftIO $ print x
        after  <- bestHeight
        return $ after > before
    -- Continue syncing from this node
    when newBest $ sendGetHeaders tid

sendGetHeaders :: ThreadId -> ManagerHandle ()
sendGetHeaders tid = do
    d    <- getPeerData tid
    -- TODO: height is only for the log. Maybe remove it?
    -- TODO: Only build a block locator the first time. Then send the last
    -- known hash from the previous headers message
    (loc,height) <- runDB $ (,) <$> blockLocator <*> bestHeight
    sendMessage tid $ MGetHeaders $ GetHeaders 0x01 loc 0x00
    $(logInfo) $ T.pack $ unwords
        [ "Requesting block headers:"
        -- TODO: This can be negative if the remote note got a new block More
        -- generallz, we need to correctly track peerHeight when a remote peer
        -- receives new blocks
        , show $ (peerHeight d) - fromIntegral height 
        , "left to download."
        ]

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
    liftIO $ S.evalStateT m $ Session db

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
    
