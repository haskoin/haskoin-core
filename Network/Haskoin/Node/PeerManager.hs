{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.Haskoin.Node.PeerManager 
( ManagerSession(..)
, PeerManager(..)
, PeerData(..)
, MngrHandle
, withAsyncNode
, sendMessage
, getPeerData
, putPeerData
, modifyPeerData
, deletePeerData
, peerExists
, getPeerKeys
, getPeerValues
, getPeers
, increasePeerHeight
, getBestPeerHeight
)
where

import System.Random (randomIO)

import Control.Monad (when, unless, forM_, liftM)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (Async, withAsync)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, MonadLogger, logInfo)
import qualified Control.Monad.State as S (StateT, evalStateT, gets, modify)

import qualified Data.Text as T (pack)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as M 
    ( Map
    , insert
    , member
    , delete
    , lookup
    , keys
    , elems
    , toList
    , empty
    )
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.Network 
    ( runTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , newTBMChan
    , sourceTBMChan
    , writeTBMChan
    , closeTBMChan
    , (>=<)
    )

import Network.Socket
    ( SockAddr (SockAddrInet)
    , PortNumber (PortNum)
    )

import Network.Haskoin.Node.Peer
import Network.Haskoin.Block
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Util
import Network.Haskoin.Constants

type MngrHandle a = 
    S.StateT a (S.StateT ManagerSession (LoggingT (ResourceT IO)))

runMngrHandle :: a -> ManagerSession -> MngrHandle a () -> IO ()
runMngrHandle a s m = 
    runResourceT $ runStdoutLoggingT $ S.evalStateT (S.evalStateT m a) s

class PeerManager r a | a -> r where
    initNode :: MngrHandle a ()
    nodeRequest :: r -> MngrHandle a ()
    peerHandshake :: RemoteHost -> Version -> MngrHandle a ()
    peerDisconnect :: RemoteHost -> MngrHandle a ()
    startPeer :: String -> Int -> MngrHandle a ()
    restartPeer :: RemoteHost -> MngrHandle a ()
    peerMessage :: RemoteHost -> Message -> MngrHandle a ()
    peerMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> MngrHandle a ()

data ManagerSession = ManagerSession
    { mngrVersion  :: !Version
    , mngrChan     :: !(TBMChan ManagerRequest)
    , mngrPeerChan :: !(TBMChan PeerMessage)
    , peerMap      :: !(M.Map RemoteHost PeerData)
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerCompleteHandshake :: !Bool
    , peerHeight            :: !BlockHeight
    , peerMsgChan           :: !(TBMChan Message)
    , peerReconnectTimer    :: !Int
    }

data MergedRequest r
    = MergedPeerMessage !PeerMessage
    | MergedNodeRequest !r
    | MergedMngrRequest !ManagerRequest

data ManagerRequest 
    = RestartPeer !RemoteHost

withAsyncNode :: (PeerManager r a)
              => [(String, Int)]
              -> a
              -> (TBMChan r -> Async () -> IO ())
              -> IO ()
withAsyncNode hosts a f = do
    vers  <- buildVersion
    pChan <- liftIO $ atomically $ newTBMChan 10000
    rChan <- liftIO $ atomically $ newTBMChan 10000
    mChan <- liftIO $ atomically $ newTBMChan 10000

    let session = ManagerSession 
            { mngrVersion  = vers
            , mngrChan     = mChan
            , mngrPeerChan = pChan
            , peerMap      = M.empty
            }
        pSource = mapOutput MergedPeerMessage (sourceTBMChan pChan)
        rSource = mapOutput MergedNodeRequest (sourceTBMChan rChan)
        mSource = mapOutput MergedMngrRequest (sourceTBMChan mChan)

    -- Launch node
    flip withAsync (\a -> f rChan a) $ runMngrHandle a session $ do
        -- Run user init hook
        initNode
        -- Merge 3 channels into 1
        mergedSource <- (mSource >=<) =<< rSource >=< pSource
        -- Start peers
        forM_ hosts $ \(host,port) -> processStartPeer host port
        -- Process messages
        mergedSource $$ managerSink

buildVersion :: MonadIO m => m Version
buildVersion = do
    -- TODO: Get our correct IP here
    let add      = NetworkAddress 1 $ SockAddrInet (PortNum 0) 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- liftIO getPOSIXTime
    rdmn <- liftIO randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: PeerManager r a => Sink (MergedRequest r) (MngrHandle a) ()
managerSink = awaitForever $ \req -> lift $ case req of
    MergedPeerMessage pm -> case pm of
        PeerHandshake remote v -> processPeerHandshake remote v
        PeerDisconnect remote -> processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> peerMerkleBlock remote dmb
        PeerMessage remote msg -> peerMessage remote msg
    MergedNodeRequest nr -> nodeRequest nr
    MergedMngrRequest mr -> case mr of
        RestartPeer r -> processRestartPeer r

processStartPeer :: PeerManager r a => String -> Int -> MngrHandle a ()
processStartPeer host port = peerExists remote >>= \exists -> unless exists $ do
    $(logInfo) $ T.pack $ unwords
        [ "Starting peer"
        , "(", show remote, ")" 
        ] 

    vers  <- lift $ S.gets mngrVersion
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 10000
        writeTBMChan c $ MVersion vers
        return c
    let peerData = PeerData 
            { peerCompleteHandshake = False
            , peerMsgChan           = pChan
            , peerHeight            = 0
            , peerReconnectTimer    = 1
            }
    lift $ S.modify $ \s -> s{ peerMap = M.insert remote peerData (peerMap s) }
    startPeer host port -- Run user hook
    startPeerThread remote
  where
    remote = RemoteHost host port

processRestartPeer :: PeerManager r a => RemoteHost -> MngrHandle a ()
processRestartPeer remote = do
    $(logInfo) $ T.pack $ unwords
        [ "Restarting peer"
        , "(", show remote, ")" 
        ] 

    vers  <- lift $ S.gets mngrVersion
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 10000
        writeTBMChan c $ MVersion vers
        return c

    modifyPeerData remote $ \d -> d{ peerMsgChan = pChan }
    restartPeer remote -- Run user hook
    startPeerThread remote

startPeerThread :: PeerManager r a => RemoteHost -> MngrHandle a ()
startPeerThread remote = do
    mpChan <- lift $ S.gets mngrPeerChan
    pChan <- liftM peerMsgChan $ getPeerData remote
    _ <- liftIO $ forkFinally 
        (runTCPClient cs $ runPeer pChan mpChan remote) $ \_ -> 
            atomically $ do
                closeTBMChan pChan
                writeTBMChan mpChan $ PeerDisconnect remote
    return ()
  where
    cs = clientSettings (remotePort remote) (stringToBS $ remoteHost remote) 

processPeerDisconnect :: PeerManager r a => RemoteHost -> MngrHandle a ()
processPeerDisconnect remote = do
    dat <- getPeerData remote
    let reconnect = peerReconnectTimer dat
        -- TODO: Put this value in a config file
        maxDelay = 1000000 * 900 -- 15 minutes

    $(logInfo) $ T.pack $ unwords
        [ "Peer disconnected. Reconnecting in"
        , show $ min (maxDelay `div` 1000000) reconnect
        , "second(s)"
        , "(", show remote, ")" 
        ]

    modifyPeerData remote $ \d -> 
        d{ peerCompleteHandshake = False 
         , peerHeight            = 0
         , peerReconnectTimer    = reconnect*2
         }

    peerDisconnect remote -- Run user hook

    -- Handle reconnection
    mChan <- lift $ S.gets mngrChan
    _ <- liftIO $ forkIO $ do
        -- reconnect is in microseconds
        threadDelay $ min maxDelay (1000000 * reconnect) 
        atomically $ writeTBMChan mChan $ RestartPeer remote
    return ()

processPeerHandshake :: PeerManager r a 
                     => RemoteHost -> Version -> MngrHandle a ()
processPeerHandshake remote remoteVer = do
    $(logInfo) $ T.pack $ unwords
        [ "Peer handshake complete"
        , "(", show remote, ")" 
        ]
    modifyPeerData remote $ \d -> 
        d{ peerCompleteHandshake = True 
         , peerHeight            = startHeight remoteVer
         , peerReconnectTimer    = 1
         }
    peerHandshake remote remoteVer -- Run user hook

sendMessage :: PeerManager r a => RemoteHost -> Message -> MngrHandle a ()
sendMessage remote msg = do
    dat <- getPeerData remote
    when (peerCompleteHandshake dat) $ do
        -- The message is discarded if the channel is closed.
        liftIO . atomically $ writeTBMChan (peerMsgChan dat) msg

getPeerData :: PeerManager r a => RemoteHost -> MngrHandle a PeerData
getPeerData remote = do
    m <- lift $ S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup remote m

putPeerData :: PeerManager r a => RemoteHost -> PeerData -> MngrHandle a ()
putPeerData remote d = 
    lift $ S.modify $ \s -> s{ peerMap = M.insert remote d (peerMap s) }

modifyPeerData :: PeerManager r a 
               => RemoteHost 
               -> (PeerData -> PeerData) 
               -> MngrHandle a ()
modifyPeerData remote f = do
    d <- getPeerData remote
    putPeerData remote $ f d

deletePeerData :: PeerManager r a => RemoteHost -> MngrHandle a ()
deletePeerData remote = 
    lift $ S.modify $ \s -> s{ peerMap = M.delete remote $ peerMap s }

peerExists :: PeerManager r a => RemoteHost -> MngrHandle a Bool
peerExists remote = do
    m <- lift $ S.gets peerMap
    return $ M.member remote m

getPeerKeys :: PeerManager r a => MngrHandle a [RemoteHost]
getPeerKeys = liftM M.keys $ lift $ S.gets peerMap

getPeerValues :: PeerManager r a => MngrHandle a [PeerData]
getPeerValues = liftM M.elems $ lift $ S.gets peerMap

getPeers :: PeerManager r a => MngrHandle a [(RemoteHost, PeerData)]
getPeers = liftM M.toList $ lift $ S.gets peerMap

-- Increase the height of a peer to the given height if it is greater than
-- the existing one.
increasePeerHeight :: PeerManager r a 
                   => RemoteHost -> BlockHeight -> MngrHandle a ()
increasePeerHeight remote h = do
    dat <- getPeerData remote
    when (h > peerHeight dat) $ do
        modifyPeerData remote $ \d -> d{ peerHeight = h }
        $(logInfo) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show h
            , "(", show remote, ")" 
            ]

getBestPeerHeight :: PeerManager r a => MngrHandle a BlockHeight
getBestPeerHeight = do
    peerValues <- getPeerValues
    return $ maximum $ 0 : map peerHeight peerValues

