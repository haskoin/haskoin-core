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
      , MonadState ManagerSession m
      , MonadResource m
      , MonadBaseControl IO m
      ) 
    => PeerManager r m | m -> r where
    initNode :: m ()
    nodeRequest :: r -> m ()
    peerHandshake :: RemoteHost -> Version -> m ()
    peerDisconnect :: RemoteHost -> m ()
    startPeer :: String -> Int -> m ()
    restartPeer :: RemoteHost -> m ()
    peerMessage :: RemoteHost -> Message -> m ()
    peerMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> m ()

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan ManagerRequest
    , mngrPeerChan     :: TBMChan PeerMessage
    , peerMap          :: M.Map RemoteHost PeerData
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

withAsyncNode :: PeerManager r m
              => [(String, Int)]
              -> (ManagerSession -> m () -> IO ())
              -> (TBMChan r -> Async () -> IO ())
              -> IO ()
withAsyncNode peers runPeerManager f = do
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
                , peerMap          = M.empty
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
    withAsync runNode $ \a -> f rChan a

buildVersion :: MonadIO m => m Version
buildVersion = do
    -- TODO: Get our correct IP here
    let add      = NetworkAddress 1 $ SockAddrInet (PortNum 0) 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- liftIO getPOSIXTime
    rdmn <- liftIO randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: PeerManager r m => Sink (MergedRequest r) m ()
managerSink = awaitForever $ \req -> lift $ case req of
    MergedPeerMessage pm -> case pm of
        PeerHandshake remote v -> processPeerHandshake remote v
        PeerDisconnect remote -> processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> peerMerkleBlock remote dmb
        PeerMessage remote msg -> peerMessage remote msg
    MergedNodeRequest nr -> nodeRequest nr
    MergedMngrRequest mr -> case mr of
        RestartPeer r -> processRestartPeer r

processStartPeer :: PeerManager r m => String -> Int -> m ()
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

processRestartPeer :: PeerManager r m => RemoteHost -> m ()
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

startPeerThread :: PeerManager r m => RemoteHost -> m ()
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

processPeerDisconnect :: PeerManager r m => RemoteHost -> m ()
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

processPeerHandshake :: PeerManager r m => RemoteHost -> Version -> m ()
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

sendMessage :: PeerManager r m => RemoteHost -> Message -> m ()
sendMessage remote msg = do
    dat <- getPeerData remote
    when (peerCompleteHandshake dat) $ do
        -- The message is discarded if the channel is closed.
        liftIO . atomically $ writeTBMChan (peerMsgChan dat) msg

getPeerData :: PeerManager r m => RemoteHost -> m PeerData
getPeerData remote = do
    m <- S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup remote m

putPeerData :: PeerManager r m => RemoteHost -> PeerData -> m ()
putPeerData remote d = 
    S.modify $ \s -> s{ peerMap = M.insert remote d (peerMap s) }

modifyPeerData :: PeerManager r m 
               => RemoteHost -> (PeerData -> PeerData) -> m ()
modifyPeerData remote f = do
    d <- getPeerData remote
    putPeerData remote $ f d

deletePeerData :: PeerManager r m => RemoteHost -> m ()
deletePeerData remote = 
    S.modify $ \s -> s{ peerMap = M.delete remote $ peerMap s }

peerExists :: PeerManager r m => RemoteHost -> m Bool
peerExists remote = do
    m <- S.gets peerMap
    return $ M.member remote m

getPeerKeys :: PeerManager r m => m [RemoteHost]
getPeerKeys = liftM M.keys $ S.gets peerMap

getPeerValues :: PeerManager r m => m [PeerData]
getPeerValues = liftM M.elems $ S.gets peerMap

-- Increase the height of a peer to the given height if it is greater than
-- the existing one.
increasePeerHeight :: PeerManager r m => RemoteHost -> BlockHeight -> m ()
increasePeerHeight remote h = do
    dat <- getPeerData remote
    when (h > peerHeight dat) $ do
        modifyPeerData remote $ \d -> d{ peerHeight = h }
        $(logInfo) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show h
            , "(", show remote, ")" 
            ]

getBestPeerHeight :: PeerManager r m => m BlockHeight
getBestPeerHeight = do
    peerValues <- getPeerValues
    return $ maximum $ 0 : map peerHeight peerValues

