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
, getSPVSession
, modifySPVSession
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

data SPVSession u = SPVSession
    { spvSyncPeer :: Maybe RemoteHost
    , spvSession :: u
    }

class ( BlockHeaderStore s 
      , MonadIO m
      , MonadLogger m
      , MonadState (ManagerSession SPVEvent (SPVSession u)) m
      , MonadResource m
      , MonadBaseControl IO m
      )
    => SPVNode s u m | m -> s u where
    runHeaderChain :: s a -> m a

instance SPVNode s u m => PeerManager SPVEvent SPVRequest (SPVSession u) m where
    initNode = spvInitNode
    nodeRequest r = spvNodeRequest r
    peerHandshake remote ver = spvPeerHandshake remote ver
    peerDisconnect remote = spvPeerDisconnect remote
    startPeer host port = spvStartPeer host port
    restartPeer remote = spvRestartPeer remote
    peerMessage remote msg = spvPeerMessage remote msg
    peerMerkleBlock remote dmb = spvPeerMerkleBlock remote dmb

withAsyncSPV :: SPVNode s u m 
             => [(String, Int)] 
             -> u
             -> (ManagerSession SPVEvent (SPVSession u) -> m () -> IO ())
             -> (TBMChan SPVEvent -> TBMChan SPVRequest -> Async () -> IO ())
             -> IO ()
withAsyncSPV peers uSession runPeerManager f = 
    withAsyncNode peers session runPeerManager f
  where
    session = SPVSession
        { spvSyncPeer = Nothing
        , spvSession  = uSession
        }

spvInitNode :: SPVNode s u m => m ()
spvInitNode = runHeaderChain initHeaderChain 

spvNodeRequest :: SPVNode s u m => SPVRequest -> m ()
spvNodeRequest r = return ()

spvPeerHandshake :: SPVNode s u m => RemoteHost -> Version -> m ()
spvPeerHandshake remote ver = do
    -- Send a GetHeaders regardless if there is already a peerSync. This peer
    -- could still be faster and become the new peerSync.
    sendGetHeaders remote True 0x00

spvPeerDisconnect :: SPVNode s u m => RemoteHost -> m ()
spvPeerDisconnect remote = do
    syn <- liftM spvSyncPeer getSession
    when (syn == Just remote) $ do
        $(logInfo) "Finding a new block header syncing peer"
        modifySession $ \s -> s{ spvSyncPeer = Nothing }
        remotes <- getPeerKeys
        forM_ remotes $ \r -> sendGetHeaders r True 0x00

spvStartPeer :: SPVNode s u m => String -> Int -> m ()
spvStartPeer host port = return ()

spvRestartPeer :: SPVNode s u m => RemoteHost -> m ()
spvRestartPeer remote = return ()

spvPeerMessage :: SPVNode s u m => RemoteHost -> Message -> m ()
spvPeerMessage remote msg = case msg of
    MHeaders headers -> processHeaders remote headers
    _ -> return ()

spvPeerMerkleBlock :: SPVNode s u m => RemoteHost -> DecodedMerkleBlock -> m ()
spvPeerMerkleBlock remote dmb = return ()

processHeaders :: SPVNode s u m => RemoteHost -> Headers -> m ()
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
        modifySession $ \s -> 
            s{ spvSyncPeer = if isSynced then Nothing else Just remote }

        $(logInfo) $ T.pack $ unwords
            [ "New best header height:"
            , show newHeight
            ]

        -- Requesting more headers
        sendGetHeaders remote False 0x00

-- Send out a GetHeaders request for a given peer
sendGetHeaders :: SPVNode s u m => RemoteHost -> Bool -> BlockHash -> m ()
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
blockHeadersSynced :: SPVNode s u m => m Bool
blockHeadersSynced = do
    networkHeight <- getBestPeerHeight
    ourHeight <- runHeaderChain bestBlockHeaderHeight
    return $ ourHeight >= networkHeight
    
getSPVSession :: SPVNode s u m => m u
getSPVSession = liftM spvSession getSession

modifySPVSession :: SPVNode s u m => (u -> u) -> m ()
modifySPVSession f = modifySession $ \s -> s{ spvSession = f $ spvSession s }

