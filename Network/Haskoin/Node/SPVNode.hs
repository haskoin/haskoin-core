{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.SPVNode 
( SPVNode(..)
, SPVSession(..)
, SPVRequest(..)
, SPVEvent(..)
, withAsyncSPV
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

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Peer

data SPVRequest 
    = BloomFilterUpdate BloomFilter
    | PublishTx Tx
    | NodeRescan Word32

data SPVEvent 
    = MerkleBlockEvent [(BlockChainAction, [TxHash])]
    | TxEvent [Tx]

data SPVSession = SPVSession
    { spvSyncPeer :: Maybe RemoteHost
    , spvEventChan :: TBMChan SPVEvent
    }

type SPVHandle m = S.StateT ManagerSession (S.StateT SPVSession m)

class ( BlockHeaderStore s 
      , MonadIO m
      , MonadLogger m
      , MonadResource m
      , MonadBaseControl IO m
      )
    => SPVNode s m | m -> s where
    runHeaderChain :: s a -> SPVHandle m a

instance SPVNode s m => PeerManager SPVRequest (S.StateT SPVSession m) where
    initNode = spvInitNode
    nodeRequest r = spvNodeRequest r
    peerHandshake remote ver = spvPeerHandshake remote ver
    peerDisconnect remote = spvPeerDisconnect remote
    startPeer host port = spvStartPeer host port
    restartPeer remote = spvRestartPeer remote
    peerMessage remote msg = spvPeerMessage remote msg
    peerMerkleBlock remote dmb = spvPeerMerkleBlock remote dmb

withAsyncSPV :: SPVNode s m
             => [(String, Int)] 
             -> (m () -> IO ())
             -> (TBMChan SPVEvent -> TBMChan SPVRequest -> Async () -> IO ())
             -> IO ()
withAsyncSPV peers runStack f = do
    eChan <- liftIO $ atomically $ newTBMChan 10000
    let session = SPVSession
            { spvSyncPeer = Nothing
            , spvEventChan = eChan
            }
        g = runStack . (flip S.evalStateT session)
    withAsyncNode peers g $ \rChan a -> f eChan rChan a

spvInitNode :: SPVNode s m => SPVHandle m ()
spvInitNode = runHeaderChain initHeaderChain 

spvNodeRequest :: SPVNode s m => SPVRequest -> SPVHandle m ()
spvNodeRequest r = return ()

spvPeerHandshake :: SPVNode s m => RemoteHost -> Version -> SPVHandle m ()
spvPeerHandshake remote ver = do
    -- Send a GetHeaders regardless if there is already a peerSync. This peer
    -- could still be faster and become the new peerSync.
    sendGetHeaders remote True 0x00

spvPeerDisconnect :: SPVNode s m => RemoteHost -> SPVHandle m ()
spvPeerDisconnect remote = do
    syn <- lift $ S.gets spvSyncPeer
    when (syn == Just remote) $ do
        $(logInfo) "Finding a new block header syncing peer"
        lift $ S.modify $ \s -> s{ spvSyncPeer = Nothing }
        remotes <- getPeerKeys
        forM_ remotes $ \r -> sendGetHeaders r True 0x00

spvStartPeer :: SPVNode s m => String -> Int -> SPVHandle m ()
spvStartPeer host port = return ()

spvRestartPeer :: SPVNode s m => RemoteHost -> SPVHandle m ()
spvRestartPeer remote = return ()

spvPeerMessage :: SPVNode s m => RemoteHost -> Message -> SPVHandle m ()
spvPeerMessage remote msg = case msg of
    MHeaders headers -> processHeaders remote headers
    _ -> return ()

spvPeerMerkleBlock :: SPVNode s m 
                   => RemoteHost -> DecodedMerkleBlock -> SPVHandle m ()
spvPeerMerkleBlock remote dmb = return ()

processHeaders :: SPVNode s m => RemoteHost -> Headers -> SPVHandle m ()
processHeaders remote (Headers hs) = do
    adjustedTime <- liftM round $ liftIO getPOSIXTime
    workBefore <- liftM nodeChainWork $ runHeaderChain getBestBlockHeader
    newBlocks <- liftM catMaybes $ forM (map fst hs) $ \bh -> do
        res <- runHeaderChain $ connectBlockHeader bh adjustedTime
        case res of
            AcceptHeader n -> return $ Just n
            HeaderAlreadyExists n -> do
                $(logWarn) $ T.pack $ unwords
                    [ "Block header already exists at height"
                    , show $ nodeHeaderHeight n
                    , "["
                    , encodeBlockHashLE $ nodeBlockHash n
                    , "]"
                    ]
                return Nothing
            RejectHeader err -> do
                $(logError) $ T.pack err
                return Nothing
    workAfter <- liftM nodeChainWork $ runHeaderChain getBestBlockHeader

    -- Continue syncing from this node only if it made some progress.
    -- Otherwise, another peer is probably faster/ahead already.
    when (workAfter > workBefore) $ do
        let newHeight = nodeHeaderHeight $ last newBlocks
        increasePeerHeight remote newHeight

        -- Update the sync peer 
        isSynced <- blockHeadersSynced
        lift $ S.modify $ \s -> 
            s{ spvSyncPeer = if isSynced then Nothing else Just remote }

        $(logInfo) $ T.pack $ unwords
            [ "New best header height:"
            , show newHeight
            ]

        -- Requesting more headers
        sendGetHeaders remote False 0x00

-- Send out a GetHeaders request for a given peer
sendGetHeaders :: SPVNode s m 
               => RemoteHost -> Bool -> BlockHash -> SPVHandle m ()
sendGetHeaders remote full hstop = do
    handshake <- liftM peerCompleteHandshake $ getPeerData remote
    -- Only peers that have finished the connection handshake
    when handshake $ do
        loc <- runHeaderChain $ if full then blockLocator else do
            h <- getBestBlockHeader
            return [nodeBlockHash h]
        $(logInfo) $ T.pack $ unwords 
            [ "Requesting more BlockHeaders"
            , "["
            , if full 
                then "BlockLocator = Full" 
                else "BlockLocator = Best header only"
            , "]"
            , "(", show remote, ")" 
            ]
        sendMessage remote $ MGetHeaders $ GetHeaders 0x01 loc hstop

-- Header height = network height
blockHeadersSynced :: SPVNode s m => SPVHandle m Bool
blockHeadersSynced = do
    networkHeight <- getBestPeerHeight
    ourHeight <- runHeaderChain bestBlockHeaderHeight
    return $ ourHeight >= networkHeight

