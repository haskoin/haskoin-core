{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.Haskoin.Node.PeerManager 
( ManagerSession(..)
, PeerManager(..)
, PeerData(..)
, withAsyncNode
, getSession
, modifySession
, sendEvent
, sendMessage
, getPeerData
, putPeerData
, modifyPeerData
, deletePeerData
, peerExists
, getPeerKeys
, getPeerValues
, increasePeerHeight
, getBestPeerHeight
)
where

import System.Random (randomIO)

import Control.Applicative ((<$>))
import Control.Monad 
    ( when
    , unless
    , forM
    , forM_
    , filterM
    , foldM
    , void
    , forever
    , replicateM
    , liftM
    )
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (Async, withAsync)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State as S (StateT, evalStateT, gets, modify)
import Control.Monad.Logger 
    ( LoggingT
    , MonadLogger
    , runStdoutLoggingT
    , logInfo
    , logWarn
    , logDebug
    , logError
    )
import Control.Monad.Logger 
    ( LoggingT
    , MonadLogger
    , runStdoutLoggingT
    , logInfo
    , logWarn
    , logDebug
    , logError
    )

import qualified Data.Text as T (pack)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes)
import Data.Word (Word32)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Default (def)
import Data.List (nub, partition, delete, maximumBy)
import qualified Data.ByteString as BS (empty)
import qualified Data.Map as M 
    ( Map
    , insert
    , member
    , delete
    , lookup
    , fromList
    , keys
    , elems
    , null
    , empty
    , partition
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
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants

type BlockHeight = Word32

class ( MonadIO m
      , MonadLogger m
      , MonadState (ManagerSession e s) m
      , MonadResource m
      , MonadBaseControl IO m
      ) 
    => PeerManager e r s m | m -> e r s where
    initNode :: m ()
    nodeRequest :: r -> m ()
    peerHandshake :: RemoteHost -> Version -> m ()
    peerDisconnect :: RemoteHost -> m ()
    startPeer :: String -> Int -> m ()
    restartPeer :: RemoteHost -> m ()
    peerMessage :: RemoteHost -> Message -> m ()
    peerMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> m ()

data ManagerSession e s = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan ManagerRequest
    , mngrPeerChan     :: TBMChan PeerMessage
    , mngrEventChan    :: TBMChan e
    , peerMap          :: M.Map RemoteHost PeerData
    , userSession      :: s
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerCompleteHandshake :: Bool
    , peerHeight            :: BlockHeight
    , peerMsgChan           :: TBMChan Message
    , peerReconnectTimer    :: Int
    }

data MergedRequest r
    = MergedPeerMessage PeerMessage
    | MergedNodeRequest r
    | MergedMngrRequest ManagerRequest

data ManagerRequest 
    = RestartPeer RemoteHost

withAsyncNode :: PeerManager e r s m 
              => [(String, Int)]
              -> s
              -> (ManagerSession e s -> m () -> IO ())
              -> (TBMChan e -> TBMChan r -> Async () -> IO ())
              -> IO ()
withAsyncNode peers uSession runPeerManager f = do
    vers  <- buildVersion
    pChan <- liftIO $ atomically $ newTBMChan 10000
    rChan <- liftIO $ atomically $ newTBMChan 10000
    mChan <- liftIO $ atomically $ newTBMChan 10000
    eChan <- liftIO $ atomically $ newTBMChan 10000

    let runNode = do
        let session = ManagerSession 
                { mngrVersion      = vers
                , mngrChan         = mChan
                , mngrPeerChan     = pChan
                , mngrEventChan    = eChan
                , peerMap          = M.empty
                , userSession      = uSession
                }
            pSource = mapOutput MergedPeerMessage (sourceTBMChan pChan)
            rSource = mapOutput MergedNodeRequest (sourceTBMChan rChan)
            mSource = mapOutput MergedMngrRequest (sourceTBMChan mChan)
        runPeerManager session $ do
            -- Run user init hook
            initNode
            -- Merge 3 channels into 1
            mergedSource <- (mSource >=<) =<< rSource >=< pSource
            -- Start peers
            forM_ peers $ \(host,port) -> processStartPeer host port
            -- Process messages
            mergedSource $$ managerSink

    -- Launch node
    withAsync runNode $ \a -> f eChan rChan a

buildVersion :: MonadIO m => m Version
buildVersion = do
    -- TODO: Get our correct IP here
    let add      = NetworkAddress 1 $ SockAddrInet (PortNum 0) 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- liftIO getPOSIXTime
    rdmn <- liftIO randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: PeerManager e r s m => Sink (MergedRequest r) m ()
managerSink = awaitForever $ \req -> lift $ case req of
    MergedPeerMessage pm -> case pm of
        PeerHandshake remote v -> processPeerHandshake remote v
        PeerDisconnect remote -> processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> peerMerkleBlock remote dmb
        PeerMessage remote msg -> peerMessage remote msg
    MergedNodeRequest nr -> nodeRequest nr
    MergedMngrRequest mr -> case mr of
        RestartPeer r -> processRestartPeer r

processStartPeer :: PeerManager e r s m => String -> Int -> m ()
processStartPeer host port = peerExists remote >>= \exists -> unless exists $ do
    $(logInfo) $ T.pack $ unwords
        [ "Starting peer"
        , "(", show remote, ")" 
        ] 

    vers  <- S.gets mngrVersion
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
    S.modify $ \s -> s{ peerMap = M.insert remote peerData (peerMap s) }
    startPeer host port -- Run user hook
    startPeerThread remote
  where
    remote = RemoteHost host port

processRestartPeer :: PeerManager e r s m => RemoteHost -> m ()
processRestartPeer remote = do
    $(logInfo) $ T.pack $ unwords
        [ "Restarting peer"
        , "(", show remote, ")" 
        ] 

    vers  <- S.gets mngrVersion
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 10000
        writeTBMChan c $ MVersion vers
        return c

    modifyPeerData remote $ \d -> d{ peerMsgChan = pChan }
    restartPeer remote -- Run user hook
    startPeerThread remote

startPeerThread :: PeerManager e r s m => RemoteHost -> m ()
startPeerThread remote = do
    mpChan <- S.gets mngrPeerChan
    pChan <- liftM peerMsgChan $ getPeerData remote
    _ <- liftIO $ forkFinally 
        (runTCPClient cs $ runPeer pChan mpChan remote) $ \_ -> 
            atomically $ do
                closeTBMChan pChan
                writeTBMChan mpChan $ PeerDisconnect remote
    return ()
  where
    cs = clientSettings (remotePort remote) (stringToBS $ remoteHost remote) 

processPeerDisconnect :: PeerManager e r s m => RemoteHost -> m ()
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
    mChan <- S.gets mngrChan
    _ <- liftIO $ forkIO $ do
        -- reconnect is in microseconds
        threadDelay $ min maxDelay (1000000 * reconnect) 
        atomically $ writeTBMChan mChan $ RestartPeer remote
    return ()

processPeerHandshake :: PeerManager e r s m => RemoteHost -> Version -> m ()
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

getSession :: PeerManager e r s m => m s
getSession = S.gets userSession

modifySession :: PeerManager e r s m => (s -> s) -> m ()
modifySession f = S.modify $ \s -> s{ userSession = f $ userSession s }

sendEvent :: PeerManager e r s m => e -> m ()
sendEvent event = do
    chan <- S.gets mngrEventChan
    -- The message is discarded if the channel is closed.
    liftIO . atomically $ writeTBMChan chan event

sendMessage :: PeerManager e r s m => RemoteHost -> Message -> m ()
sendMessage remote msg = do
    dat <- getPeerData remote
    when (peerCompleteHandshake dat) $ do
        -- The message is discarded if the channel is closed.
        liftIO . atomically $ writeTBMChan (peerMsgChan dat) msg

getPeerData :: PeerManager e r s m => RemoteHost -> m PeerData
getPeerData remote = do
    m <- S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup remote m

putPeerData :: PeerManager e r s m => RemoteHost -> PeerData -> m ()
putPeerData remote d = 
    S.modify $ \s -> s{ peerMap = M.insert remote d (peerMap s) }

modifyPeerData :: PeerManager e r s m 
               => RemoteHost -> (PeerData -> PeerData) -> m ()
modifyPeerData remote f = do
    d <- getPeerData remote
    putPeerData remote $ f d

deletePeerData :: PeerManager e r s m => RemoteHost -> m ()
deletePeerData remote = 
    S.modify $ \s -> s{ peerMap = M.delete remote $ peerMap s }

peerExists :: PeerManager e r s m => RemoteHost -> m Bool
peerExists remote = do
    m <- S.gets peerMap
    return $ M.member remote m

getPeerKeys :: PeerManager e r s m => m [RemoteHost]
getPeerKeys = liftM M.keys $ S.gets peerMap

getPeerValues :: PeerManager e r s m => m [PeerData]
getPeerValues = liftM M.elems $ S.gets peerMap

-- Increase the height of a peer to the given height if it is greater than
-- the existing one.
increasePeerHeight :: PeerManager e r s m => RemoteHost -> BlockHeight -> m ()
increasePeerHeight remote h = do
    dat <- getPeerData remote
    when (h > peerHeight dat) $ do
        modifyPeerData remote $ \d -> d{ peerHeight = h }
        $(logInfo) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show h
            , "(", show remote, ")" 
            ]

getBestPeerHeight :: PeerManager e r s m => m BlockHeight
getBestPeerHeight = do
    peerValues <- getPeerValues
    return $ maximum $ 0 : map peerHeight peerValues

