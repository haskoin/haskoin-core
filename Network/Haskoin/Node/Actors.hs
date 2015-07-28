{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Actors 
( -- * Node
  withNode
, WalletMessage(..)
, NodeRequest(..)
, RemoteHost(..)
, MerkleTxs
) 
where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.Logger (MonadLogger, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Catch (MonadMask)

import Data.Text (Text, pack)
import Data.Conduit.TMChan (TBMChan, writeTBMChan)
import Data.Conduit (awaitForever, ($$))
import Data.Conduit.TMChan (newTBMChan, sourceTBMChan)

import qualified Database.LevelDB.Base as L (Options(..))

import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Actors.Types
import Network.Haskoin.Node.Actors.TxManager

-- | Start an bitcoin node. This function will return a channel for receiving
-- node events and a channel for sending requests to the node.
withNode :: (MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadMask m)
         => FilePath
         -> L.Options
         -> (TBMChan WalletMessage -> TBMChan NodeRequest -> m ())
         -> m ()
withNode fp opts f = do
    wletChan <- liftIO $ atomically $ newTBMChan 10
    reqChan  <- liftIO $ atomically $ newTBMChan 10
    withTxManager fp opts wletChan $ \txmgChan bkchChan mngrChan -> do
        -- Listen for and process wallet requests
        let run = do
            $(logDebug) $ formatWallet "User request thread started"
            sourceTBMChan reqChan $$ (go txmgChan bkchChan mngrChan)
        withAsync run $ \a -> link a >> f wletChan reqChan
  where
    go txmgChan bkchChan mngrChan = awaitForever $ \req -> case req of
        NodeBloomFilter bloom -> do
            $(logDebug) $ formatWallet "Setting a new bloom filter"
            liftIO $ atomically $ writeTBMChan bkchChan $ SetBloomFilter bloom
        NodeStartMerkleDownload valE -> do
            $(logDebug) $ formatWallet "Requesting a merkle block download"
            liftIO $ atomically $ 
                writeTBMChan bkchChan $ StartMerkleDownload valE
        NodeStartBlockDownload valE -> do
            $(logDebug) $ formatWallet "Requesting a block download"
            liftIO $ atomically $ 
                writeTBMChan bkchChan $ StartBlockDownload valE
        NodeConnectPeers peers -> do
            $(logDebug) $ formatWallet "Advertising new peers to connect to"
            liftIO $ atomically $ writeTBMChan mngrChan $ AddRemoteHosts peers
        NodeSendTx tx -> do
            $(logDebug) $ formatWallet $ unwords
                [ "Sending transaction", encodeTxHashLE (txHash tx) ]
            liftIO $ atomically $ writeTBMChan txmgChan $ TxManagerSendTx tx
        NodePublishTxs txids -> do
            $(logDebug) $ formatWallet "Publishing a transaction to broadcast"
            -- Publish a job with priority 1 on all peers
            liftIO $ atomically $ writeTBMChan mngrChan $ 
                PublishJob (JobSendTxInv txids) (AllPeers1 0) 1
        NodeBatchSize i -> do
            $(logDebug) $ formatWallet $ unwords
                [ "Setting the node batch size to", show i]
            liftIO $ atomically $ writeTBMChan bkchChan $ SetBatchSize i
        NodeStatus -> do
            $(logDebug) $ formatWallet "Requesting node status"
            liftIO $ atomically $ writeTBMChan txmgChan TxManagerStatus
            liftIO $ atomically $ writeTBMChan bkchChan BkchStatus
            liftIO $ atomically $ writeTBMChan mngrChan MngrStatus

formatWallet :: String -> Text
formatWallet str = pack $ unwords [ "[Wallet Request]", str ]

