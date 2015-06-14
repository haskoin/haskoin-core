{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Mempool
( withNode
, withMempool
) where

import Control.Monad (unless, forM_, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Map.Strict (Map, findWithDefault, insertWith)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import Data.List (nub, partition, delete)
import Data.Unique (hashUnique)
import Data.Conduit.TMChan (TBMChan, writeTBMChan)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (newTBMChan, sourceTBMChan)

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Chan
import Network.Haskoin.Node.BlockChain

-- | Start an bitcoin node. This function will return a channel for receiving
-- node events and a channel for sending requests to the node.
withNode :: (HeaderTree m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
         => (TBMChan WalletMessage -> TBMChan NodeRequest -> m ())
         -> m ()
withNode f = do
    wletChan <- liftIO $ atomically $ newTBMChan 10000
    reqChan  <- liftIO $ atomically $ newTBMChan 10000
    withMempool wletChan $ \mempChan bkchChan mngrChan -> do
        -- Listen for and process wallet requests
        let run = do
            $(logDebug) $ formatWallet "User request thread started"
            sourceTBMChan reqChan $$ (go mempChan bkchChan mngrChan)
        withAsync run $ \a -> link a >> f wletChan reqChan
  where
    go mempChan bkchChan mngrChan = awaitForever $ \req -> case req of
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
            liftIO $ atomically $ writeTBMChan mempChan $ MempoolSendTx tx
        NodePublishTxs txids -> do
            $(logDebug) $ formatWallet "Publishing a transaction to broadcast"
            -- Publish a job with priority 1 on all peers
            liftIO $ atomically $ writeTBMChan mngrChan $ 
                PublishJob (JobSendTxInv txids) (AllPeers1 0) 1

formatWallet :: String -> Text
formatWallet str = pack $ unwords [ "[Wallet Request]", str ]

{- Mempool Actor -}

data MempoolSession = MempoolSession
    { -- Blockchain message channel
      mngrChan :: !(TBMChan ManagerMessage)
      -- Blockchain message channel
    , wletChan :: !(TBMChan WalletMessage)
      -- Flag that tells us if the node is synced or not. As long as the node
      -- is not synced, we hold solo transactions in a bufer.
    , nodeSynced :: !Bool
      -- Transactions that have not been sent in a block or merkle block.
      -- We stall solo transactions until the node is synced.
    , txBuffer :: ![Tx]
      -- Inflight transaction requests for each peer. We are waiting for
      -- the GetData response. We stall merkle blocks if there are pending
      -- transaction downloads.
    , inflightTxs :: ![TxHash]
      -- Merkle block buffer. We buffer merkle blocks while transactions are
      -- inflight. This is to prevent a race condition where a transaction
      -- could miss his confirmation.
    , merkleBuffer :: ![(BlockChainAction, [DecodedMerkleBlock])]
      -- Map of transactions to peer ids to respond to GetData requests
      -- from peers.
    , txPeerMap :: !(Map TxHash [PeerId])
    }

-- | Start the mempool. The job of this actor is to request tx downloads
-- when receiving tx invs. It also has to buffer solo transactions until
-- the chain is synced up before sending them to the wallet. It will also
-- suspend merkle block delivery while there are inflight transaction downloads.
-- This is to prevent race conditions where the wallet could miss confirmations.
withMempool 
    :: (HeaderTree m, MonadLogger m, MonadIO m, MonadBaseControl IO m) 
    => TBMChan WalletMessage
    -> (    TBMChan MempoolMessage 
         -> TBMChan BlockChainMessage 
         -> TBMChan ManagerMessage 
         -> m ()
       )
    -> m ()
withMempool wletChan f = do
    mempChan <- liftIO $ atomically $ newTBMChan 10000
    withBlockChain mempChan $ \bkchChan mngrChan -> do
        let nodeSynced   = False
            txBuffer     = []
            inflightTxs  = []
            merkleBuffer = []
            txPeerMap    = M.empty
            session      = MempoolSession{..}
            -- Run the main mempool message processing loop
            run = do
                $(logDebug) $ format "Mempool thread started"
                sourceTBMChan mempChan $$ processMempoolMessage

        withAsync (evalStateT run session) $ \a ->
            link a >> f mempChan bkchChan mngrChan

processMempoolMessage :: (MonadLogger m, MonadIO m) 
                      => Sink MempoolMessage (StateT MempoolSession m) ()
processMempoolMessage = awaitForever $ \req -> lift $ case req of
    MempoolTxInv pid tids       -> processTxInv pid tids
    MempoolTx tx                -> processTx tx
    MempoolGetTx pid tid        -> processGetTx pid tid
    MempoolSendTx tid           -> processSendTx tid
    MempoolMerkles action dmbs  -> processMerkles action dmbs
    MempoolBlocks action blocks -> processBlocks action blocks
    MempoolSynced               -> processSynced
    MempoolStartDownload valE   -> processStartDownload valE 

-- | Decide if we want to download a transaction or not. We store inflight
-- transactions.
processTxInv :: (MonadLogger m, MonadIO m) 
             => PeerId -> [TxHash] -> StateT MempoolSession m ()
processTxInv _ [] = return () -- Ignore empty INVs
processTxInv pid tids = do
    $(logDebug) $ format $ unlines $
        unwords [ "Received transaction INV from peer"
                , show $ hashUnique pid
                ]
        : map (("  " ++) . encodeTxHashLE) tids 

    inflight <- gets inflightTxs
    soloTxs  <- liftM (map txHash) $ gets txBuffer
    let f h   = not (h `elem` inflight) && not (h `elem` soloTxs)
        toDwn = filter f tids
    unless (null toDwn) $ do
        $(logDebug) $ format $ unwords
            [ "Requesting", show $ length toDwn
            , "transaction downloads from peer"
            , show $ hashUnique pid
            ]
        -- Save inflight downloads
        modify $ \s -> s{ inflightTxs = inflight ++ toDwn }
        -- Publish the transaction download job with average priority
        sendManager $ PublishJob (JobDwnTxs toDwn) (ThisPeer pid) 5

processGetTx :: (MonadIO m, MonadLogger m)
             => PeerId
             -> TxHash
             -> StateT MempoolSession m ()
processGetTx pid tid = do
    $(logDebug) $ format $ unwords
        [ "Requesting transaction", encodeTxHashLE tid, "from wallet." ]
    modify $
        \s -> s{ txPeerMap = insertWith (flip (++)) tid [pid] (txPeerMap s) }
    sendWallet $ WalletGetTx tid

processSendTx :: (MonadIO m, MonadLogger m)
              => Tx
              -> StateT MempoolSession m ()
processSendTx tx = do
    $(logDebug) $ format $ unwords
        [ "Sending transaction", encodeTxHashLE (txHash tx), "from wallet." ]
    peers <- findWithDefault [] (txHash tx) `liftM` gets txPeerMap
    forM_ peers $ \pid -> sendManager $
        PublishJob (JobSendTx tx) (ThisPeer pid) 1
    modify $
        \s -> s{ txPeerMap = M.delete (txHash tx) (txPeerMap s) }

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
            $(logInfo) $ format $ concat 
                [ "Received solo tx ", encodeTxHashLE tid
                , ". Sending it to the wallet."
                ]
            sendWallet $ WalletTx tx
        -- We are not synced. We buffer transactions until we are synced.
        else do
            $(logInfo) $ format $ concat 
                [ "Received solo tx ", encodeTxHashLE tid
                , ". Buffering it as we are not synced."
                ]
            modify $ \s -> s{ txBuffer = txBuffer s ++ [tx] }

    -- Remove this transaction from the inflight list
    modify $ \s -> s{ inflightTxs = delete tid $ inflightTxs s }

    -- Try to import pending merkles
    merkles <- gets merkleBuffer
    modify $ \s -> s{ merkleBuffer = [] }
    forM_ merkles $ \(action, dmbs) -> processMerkles action dmbs
  where
    tid = txHash tx

-- | Import merkle blocks into the wallet if there are no infligh transactions.
-- Buffer the transaction otherwise.
processMerkles :: (MonadIO m, MonadLogger m)
               => BlockChainAction 
               -> [DecodedMerkleBlock] 
               -> StateT MempoolSession m ()
processMerkles action dmbs = gets inflightTxs >>= \inflight -> if null inflight
    -- No infligh transactions. We can send the merkles to the wallet
    then do
        -- Check if we have solo transactions that belong to these merkle blocks
        txs <- gets txBuffer
        let (newTxs, newDmbs) = merge [] txs dmbs
        modify $ \s -> s{ txBuffer = newTxs }

        $(logDebug) $ format $ unwords
            [ "Sending", show $ length newDmbs, "merkle blocks and"
            , show $ length $ concat $ map merkleTxs newDmbs
            , "transactions to the wallet."
            ]
            
        -- Send the merkles to the wallet
        sendWallet $ WalletMerkles action newDmbs
        -- Some logging
        logBlockChainAction action
    -- We stall merkle block imports when transactions are inflight. This
    -- is to prevent this race condition where tx1 would miss it's
    -- confirmation:
    -- INV tx1 -> MerkleBlock (all tx except tx1) -> Tx1
    -- This problem arises because bitcoin-core adds txs to a "known inventory"
    -- when it advertises a tx to us in an INV. Those known inventory txs are
    -- no sent in merkle blocks afterwards.
    else do
        $(logDebug) $ format $ unwords
            [ "Received a merkle block while transactions are inflight."
            , "Buffering it."
            ]
        modify $ \s -> s{ merkleBuffer = merkleBuffer s ++ [(action, dmbs)] }
  where
    merge acc txs [] = (txs, reverse acc)
    merge acc txs (d:ds) = 
        let (xs, rs) = partition ((`elem` expectedTxs d) . txHash) txs
            d'       = d{ merkleTxs = nub $ merkleTxs d ++ xs }
        in  merge (d':acc) rs ds

-- | Import merkle blocks into the wallet if there are no infligh transactions.
-- Buffer the transaction otherwise.
processBlocks :: (MonadIO m, MonadLogger m)
              => BlockChainAction 
              -> [Block] 
              -> StateT MempoolSession m ()
processBlocks action blocks = do
    $(logDebug) $ format $ unwords
        [ "Sending", show $ length blocks, "blocks to the wallet." ]
    -- Send the merkles to the wallet
    sendWallet $ WalletBlocks action blocks
    -- some logging
    logBlockChainAction action

logBlockChainAction :: (MonadIO m, MonadLogger m)
                   => BlockChainAction -> StateT MempoolSession m ()
logBlockChainAction action = case action of
    BestChain nodes -> $(logInfo) $ format $ unwords
        [ "Best chain height"
        , show $ nodeHeaderHeight $ last nodes
        , "(", encodeBlockHashLE $ nodeBlockHash $ last nodes, ")"
        ]
    ChainReorg _ o n -> $(logInfo) $ format $ unlines $
           [ "Chain reorg."
           , "Orphaned blocks:" 
           ]
        ++ map (("  " ++) . encodeBlockHashLE . nodeBlockHash) o
        ++ [ "New blocks:" ]
        ++ map (("  " ++) . encodeBlockHashLE . nodeBlockHash) n
        ++ [ unwords [ "Best merkle chain height"
                     , show $ nodeHeaderHeight $ last n
                     ]
           ]
    SideChain n -> $(logWarn) $ format $ unlines $
        "Side chain:" : 
        map (("  " ++) . encodeBlockHashLE . nodeBlockHash) n

-- When the node is synced, the blockchain notifies us. We can then safely send
-- solo transactions to the wallet.
processSynced :: (MonadLogger m, MonadIO m) => StateT MempoolSession m ()
processSynced = do
    txs <- gets txBuffer
    $(logInfo) $ format "Blocks are in sync with the block headers."
    unless (null txs) $ do
        $(logInfo) $ format $ unwords
            [ "Sending", show $ length txs, "txs to the wallet." ]
        forM_ txs $ sendWallet . WalletTx
    modify $ \s -> s{ nodeSynced = True 
                    , txBuffer   = []
                    }
    sendWallet $ WalletSynced

processStartDownload :: (MonadLogger m, MonadIO m) 
                     => Either Timestamp BlockHash -> StateT MempoolSession m ()
processStartDownload _ = do
    $(logDebug) $ format $ unwords
        [ "(Re)starting a new merkle block download."
        , "Cleaning up the merkle buffer."
        ]
    modify $ \s -> s{ nodeSynced   = False
                    , merkleBuffer = []
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

format :: String -> Text
format str = pack $ unwords [ "[Mempool]", str ]

