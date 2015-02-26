{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.SpvMempool
( withSpvNode
, withSpvMempool
) where

import Control.Applicative ((<$>))
import Control.Monad ( when, unless, forM, forM_, foldM, forever, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (Async, withAsync, link)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug, logError)
import Control.Monad.Trans.Control 
    ( MonadBaseControl
    , StM
    , control
    , liftBaseDiscard
    )

import qualified Data.Text as T (pack)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (nub, partition, delete, (\\))
import Data.Conduit.TMChan (TBMChan, writeTBMChan)
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , writeTBMChan
    , newTBMChan
    , sourceTBMChan
    )
import qualified Data.Map as M 
    ( Map, member, delete, lookup, fromList, fromListWith
    , keys, elems, toList, toAscList, empty, map, filter
    , adjust, update, singleton, unionWith
    )

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Chan
import Network.Haskoin.Node.SpvBlockChain

-- | Start an SPV node. This function will return a channel for receiving
-- node events and a channel for sending requests to the node.
withSpvNode :: (HeaderTree m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
            => (TBMChan WalletMessage -> TBMChan NodeRequest -> m ())
            -> m ()
withSpvNode f = do
    wletChan <- liftIO $ atomically $ newTBMChan 10000
    reqChan  <- liftIO $ atomically $ newTBMChan 10000
    withSpvMempool wletChan $ \bkchChan mngrChan -> do
        -- Listen for and process wallet requests
        let run = sourceTBMChan reqChan $$ (go bkchChan mngrChan)
        withAsync run $ \a -> link a >> f wletChan reqChan
  where
    go bkchChan mngrChan = awaitForever $ \req -> case req of
        NodeBloomFilter bloom -> 
            liftIO $ atomically $ writeTBMChan mngrChan $ SetBloomFilter bloom
        NodeStartDownload valE ->
            liftIO $ atomically $ writeTBMChan bkchChan $ StartDownload valE
        NodeConnectPeers peers ->
            liftIO $ atomically $ writeTBMChan mngrChan $ AddRemoteHosts peers
        NodePublishTx tx ->
            -- Publish a job with priority 1 on all peers
            liftIO $ atomically $ writeTBMChan mngrChan $ 
                PublishJob (JobSendTx tx) (AllPeers1 0) 1

{- Mempool Actor -}

data MempoolSession = MempoolSession
    { -- Blockchain message channel
      mngrChan :: !(TBMChan ManagerMessage)
      -- Blockchain message channel
    , wletChan :: !(TBMChan WalletMessage)
      -- Flag that tells us if the node is synced or not. As long as the node
      -- is not synced, we hold solo transactions in a bufer.
    , nodeSynced :: !Bool
      -- Transactions that have not been sent in a merkle block.
      -- We stall solo transactions until the merkle blocks are synced.
    , txBuffer :: ![Tx]
      -- Inflight transaction requests for each peer. We are waiting for
      -- the GetData response. We stall merkle blocks if there are pending
      -- transaction downloads.
    , inflightTxs :: ![TxHash]
      -- Merkle block buffer. We buffer merkle blocks while transactions are
      -- inflight. This is to prevent a race condition where a transaction
      -- could miss his confirmation.
    , merkleBuffer :: ![(BlockChainAction, [DecodedMerkleBlock])]
    }

-- | Start the SPV mempool. The job of this actor is to request tx downloads
-- when receiving tx invs. It also has to buffer solo transactions until
-- the chain is synced up before sending them to the wallet. It will also
-- suspend merkle block delivery while there are inflight transaction downloads.
-- This is to prevent race conditions where the wallet could miss confirmations.
withSpvMempool 
    :: (HeaderTree m, MonadLogger m, MonadIO m, MonadBaseControl IO m) 
    => TBMChan WalletMessage
    -> (TBMChan BlockChainMessage -> TBMChan ManagerMessage -> m ())
    -> m ()
withSpvMempool wletChan f = do
    mempChan <- liftIO $ atomically $ newTBMChan 10000
    withSpvBlockChain mempChan $ \bkchChan mngrChan -> do
        let nodeSynced   = False
            txBuffer     = []
            inflightTxs  = []
            merkleBuffer = []
            session      = MempoolSession{..}
            -- Run the main mempool message processing loop
            run = sourceTBMChan mempChan $$ processMempoolMessage

        withAsync (evalStateT run session) $ \a ->
            link a >> f bkchChan mngrChan

processMempoolMessage :: (MonadLogger m, MonadIO m) 
                      => Sink MempoolMessage (StateT MempoolSession m) ()
processMempoolMessage = awaitForever $ \req -> lift $ case req of
    MempoolTxInv pid tids     -> processTxInv pid tids
    MempoolTx tx              -> processTx tx
    MempoolMerkle action dmbs -> processMerkle action dmbs
    MempoolSynced             -> processSynced
    
    _ -> return () -- Ignore Blocks

-- | Decide if we want to download a transaction or not. We store inflight
-- transactions.
processTxInv :: (MonadLogger m, MonadIO m) 
             => PeerId -> [TxHash] -> StateT MempoolSession m ()
processTxInv _ [] = return () -- Ignore empty INVs
processTxInv pid tids = do
    $(logInfo) $ T.pack $ unwords
        [ "Got tx inv" , "[", unwords $ map encodeTxHashLE tids, "]" ]
    inflight <- gets inflightTxs
    soloTxs  <- liftM (map txHash) $ gets txBuffer
    let f h   = not (h `elem` inflight) && not (h `elem` soloTxs)
        toDwn = filter f tids
    unless (null toDwn) $ do
        -- Save inflight downloads
        modify $ \s -> s{ inflightTxs = inflight ++ toDwn }
        -- Publish the transaction download job with average priority
        sendManager $ PublishJob (JobDwnTxs toDwn) (ThisPeer pid) 5

-- | Send transactions to the wallet only if we are synced. This is to prevent
-- a problem where a transaction that belongs to the wallet in the future
-- might still pass through the early bloom filter and not be recognized 
-- by the wallet.
processTx :: (MonadLogger m, MonadIO m) => Tx -> StateT MempoolSession m ()
processTx tx = do
    synced <- gets nodeSynced 
    if synced
        -- We are synced. We send solo transactions to the wallet
        then do
            $(logInfo) $ T.pack $ unwords 
                [ "Got solo tx", encodeTxHashLE tid
                , "Sending to the wallet."
                ]
            sendWallet $ WalletTx tx
        -- We are not synced. We buffer transactions until we are synced.
        else do
            $(logInfo) $ T.pack $ unwords 
                [ "Got solo tx", encodeTxHashLE tid
                , "We are not synced. Buffering it."
                ]
            modify $ \s -> s{ txBuffer = txBuffer s ++ [tx] }

    -- Remove this transaction from the inflight list
    modify $ \s -> s{ inflightTxs = delete tid $ inflightTxs s }

    -- Try to import pending merkles
    merkles <- gets merkleBuffer
    modify $ \s -> s{ merkleBuffer = [] }
    forM_ merkles $ \(action, dmbs) -> processMerkle action dmbs
  where
    tid = txHash tx

-- | Import merkle blocks into the wallet if there are no infligh transactions.
-- Buffer the transaction otherwise.
processMerkle :: (MonadIO m, MonadLogger m)
              => BlockChainAction 
              -> [DecodedMerkleBlock] 
              -> StateT MempoolSession m ()
processMerkle action dmbs = gets inflightTxs >>= \inflight -> if null inflight
    -- No infligh transactions. We can send the merkles to the wallet
    then do
        -- Check if we have solo transactions that belong to these merkle blocks
        txs <- gets txBuffer
        let (txs', dmbs') = merge [] txs dmbs
        modify $ \s -> s{ txBuffer = txs' }
        -- Send the merkles to the wallet
        sendWallet $ WalletMerkle action dmbs'
        -- Some logging
        case action of
            BestChain nodes -> $(logInfo) $ T.pack $ unwords
                [ "Best merkle chain height:"
                , show $ nodeHeaderHeight $ last nodes, ":"
                , encodeBlockHashLE $ nodeBlockHash $ last nodes
                ]
            ChainReorg s o n -> $(logInfo) $ T.pack $ unwords
                [ "Merkle chain reorg. Orphaned blocks:"
                , "[", unwords $ map (encodeBlockHashLE . nodeBlockHash) o ,"]"
                , "New blocks:"
                , "[", unwords $ map (encodeBlockHashLE . nodeBlockHash) n ,"]"
                , "New height:"
                , show $ nodeHeaderHeight $ last n
                ]
            SideChain _ -> $(logWarn) "Got a merkle side chain."
    -- We stall merkle block imports when transactions are inflight. This
    -- is to prevent this race condition where tx1 would miss it's
    -- confirmation:
    -- INV tx1 -> MerkleBlock (all tx except tx1) -> Tx1
    -- This problem arises because bitcoin-core adds txs to a "known inventory"
    -- when it advertises a tx to us in an INV. Those known inventory txs are
    -- no sent in merkle blocks afterwards.
    else modify $ \s -> s{ merkleBuffer = merkleBuffer s ++ [(action, dmbs)] }
  where
    merge acc txs [] = (txs, reverse acc)
    merge acc txs (dmb:dmbs) = do
        let (xs, rs) = partition ((`elem` expectedTxs dmb) . txHash) txs
            dmb' = dmb{ merkleTxs = nub $ merkleTxs dmb ++ xs }
        merge (dmb':acc) rs dmbs

-- When the node is synced, the blockchain notifies us. We can then safely send
-- solo transactions to the wallet.
processSynced :: MonadIO m => StateT MempoolSession m ()
processSynced = do
    txs <- gets txBuffer
    forM_ txs $ sendWallet . WalletTx
    modify $ \s -> s{ nodeSynced = True 
                    , txBuffer   = []
                    }

{- Helpers -}

-- Send a message to the PeerManager
sendManager :: MonadIO m => ManagerMessage -> StateT MempoolSession m ()
sendManager msg = do
    chan <- gets mngrChan
    liftIO . atomically $ writeTBMChan chan msg

-- Send a message to the PeerManager
sendWallet :: MonadIO m => WalletMessage -> StateT MempoolSession m ()
sendWallet msg = do
    chan <- gets wletChan
    liftIO . atomically $ writeTBMChan chan msg

