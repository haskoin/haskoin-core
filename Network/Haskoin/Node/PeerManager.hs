{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (Async, withAsync)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logInfo)
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
import Network.Haskoin.Network

type MngrHandle a b = 
    S.StateT b (S.StateT (ManagerSession a) (LoggingT (ResourceT IO)))

runMngrHandle :: b -> ManagerSession a -> MngrHandle a b () -> IO ()
runMngrHandle a s m = 
    runResourceT $ runStdoutLoggingT $ S.evalStateT (S.evalStateT m a) s

class PeerManager a r b | b -> r where
    initNode :: MngrHandle a b ()
    nodeRequest :: r -> MngrHandle a b ()
    peerHandshake :: RemoteHost -> Version -> MngrHandle a b ()
    peerDisconnect :: RemoteHost -> MngrHandle a b ()
    startPeer :: String -> Int -> MngrHandle a b ()
    restartPeer :: RemoteHost -> MngrHandle a b ()
    peerMessage :: RemoteHost -> (Message a) -> MngrHandle a b ()
    peerMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> MngrHandle a b ()

data ManagerSession a = ManagerSession
    { mngrVersion  :: !Version
    , mngrChan     :: !(TBMChan ManagerRequest)
    , mngrPeerChan :: !(TBMChan (PeerMessage a))
    , peerMap      :: !(M.Map RemoteHost (PeerData a))
    } 

-- Data stored about a peer in the Manager
data PeerData a = PeerData
    { peerCompleteHandshake :: !Bool
    , peerHeight            :: !BlockHeight
    , peerMsgChan           :: !(TBMChan (Message a))
    , peerReconnectTimer    :: !Int
    }

data MergedRequest a r
    = MergedPeerMessage !(PeerMessage a)
    | MergedNodeRequest !r
    | MergedMngrRequest !ManagerRequest

data ManagerRequest 
    = RestartPeer !RemoteHost

withAsyncNode :: forall a r b. (Network a, PeerManager a r b)
              => a
              -> [(String, Int)]
              -> b
              -> (TBMChan r -> Async () -> IO ())
              -> IO ()
withAsyncNode net hosts a f = do
    vers  <- buildVersion net
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
    flip withAsync (\ac -> f rChan ac) $ runMngrHandle a session $ do
        -- Run user init hook
        initNode
        -- Merge 3 channels into 1
        mergedSource <- (mSource >=<) =<< rSource >=< pSource
        -- Start peers
        forM_ hosts $ \(host,port) -> processStartPeer host port
        -- Process messages
        mergedSource $$
            (managerSink :: Sink (MergedRequest a r) (MngrHandle a b) ())

buildVersion :: (Network a, MonadIO m) => a -> m Version
buildVersion net = do
    -- TODO: Get our correct IP here
    let add      = NetworkAddress 1 $ SockAddrInet (PortNum 0) 0
        ua       = VarString $ stringToBS (haskoinUserAgent net)
    time <- liftIO getPOSIXTime
    rdmn <- liftIO randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: (Network a, PeerManager a r b)
            => Sink (MergedRequest a r) (MngrHandle a b) ()
managerSink = awaitForever $ \req -> lift $ case req of
    MergedPeerMessage pm -> case pm of
        PeerHandshake remote v -> processPeerHandshake remote v
        PeerDisconnect remote -> processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> peerMerkleBlock remote dmb
        PeerMessage remote msg -> peerMessage remote msg
    MergedNodeRequest nr -> nodeRequest nr
    MergedMngrRequest mr -> case mr of
        RestartPeer r -> processRestartPeer r

processStartPeer :: (Network a, PeerManager a r b)
                 => String -> Int -> MngrHandle a b ()
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

processRestartPeer :: (Network a, PeerManager a r b)
                   => RemoteHost -> MngrHandle a b ()
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

startPeerThread :: (Network a, PeerManager a r b)
                => RemoteHost -> MngrHandle a b ()
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

processPeerDisconnect :: PeerManager a r b => RemoteHost -> MngrHandle a b ()
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

processPeerHandshake :: PeerManager a r b 
                     => RemoteHost -> Version -> MngrHandle a b ()
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

sendMessage :: PeerManager a r b
            => RemoteHost -> Message a -> MngrHandle a b ()
sendMessage remote msg = do
    dat <- getPeerData remote
    when (peerCompleteHandshake dat) $ do
        -- The message is discarded if the channel is closed.
        liftIO . atomically $ writeTBMChan (peerMsgChan dat) msg

getPeerData :: PeerManager a r b => RemoteHost -> MngrHandle a b (PeerData a)
getPeerData remote = do
    m <- lift $ S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup remote m

putPeerData :: PeerManager a r b
            => RemoteHost -> PeerData a -> MngrHandle a b ()
putPeerData remote d = 
    lift $ S.modify $ \s -> s{ peerMap = M.insert remote d (peerMap s) }

modifyPeerData :: PeerManager a r b 
               => RemoteHost 
               -> (PeerData a -> PeerData a) 
               -> MngrHandle a b ()
modifyPeerData remote f = do
    d <- getPeerData remote
    putPeerData remote $ f d

deletePeerData :: PeerManager a r b => RemoteHost -> MngrHandle a b ()
deletePeerData remote = 
    lift $ S.modify $ \s -> s{ peerMap = M.delete remote $ peerMap s }

peerExists :: PeerManager a r b => RemoteHost -> MngrHandle a b Bool
peerExists remote = do
    m <- lift $ S.gets peerMap
    return $ M.member remote m

getPeerKeys :: PeerManager a r b => MngrHandle a b [RemoteHost]
getPeerKeys = liftM M.keys $ lift $ S.gets peerMap

getPeerValues :: PeerManager a r b => MngrHandle a b [PeerData a]
getPeerValues = liftM M.elems $ lift $ S.gets peerMap

getPeers :: PeerManager a r b => MngrHandle a b [(RemoteHost, PeerData a)]
getPeers = liftM M.toList $ lift $ S.gets peerMap

-- Increase the height of a peer to the given height if it is greater than
-- the existing one.
increasePeerHeight :: PeerManager a r b 
                   => RemoteHost -> BlockHeight -> MngrHandle a b ()
increasePeerHeight remote h = do
    dat <- getPeerData remote
    when (h > peerHeight dat) $ do
        modifyPeerData remote $ \d -> d{ peerHeight = h }
        $(logInfo) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show h
            , "(", show remote, ")" 
            ]

getBestPeerHeight :: PeerManager a r b => MngrHandle a b BlockHeight
getBestPeerHeight = do
    peerValues <- getPeerValues
    return $ maximum $ 0 : map peerHeight peerValues

