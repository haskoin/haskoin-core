{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Actors.TxManager (withTxManager) where

import Control.Monad (when, unless, forM_, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.State (StateT, evalStateT, get, gets)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Catch (MonadMask)
import Control.DeepSeq (NFData(..))

import Data.Text (Text, pack)
import Data.List (nub, partition, delete)
import Data.Unique (hashUnique)
import Data.Conduit.TMChan (TBMChan, writeTBMChan)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (newTBMChan, sourceTBMChan)
import qualified Data.Map.Strict as M 
    (Map, empty, delete, elems, findWithDefault, insertWith)

import qualified Database.LevelDB.Base as L (Options(..))

import Network.Haskoin.Util
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Actors.Types
import Network.Haskoin.Node.Actors.BlockChain

{- TxManager Actor -}

data TxManagerSession = TxManagerSession
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
    , merkleBuffer :: ![(BlockChainAction, [MerkleTxs])]
      -- Map of transactions to peer ids to respond to GetData requests
      -- from peers.
    , txPeerMap :: !(M.Map TxHash [PeerId])
    }

instance NFData TxManagerSession where
    rnf TxManagerSession{..} =
        rnf nodeSynced `seq`
        rnf txBuffer `seq`
        rnf inflightTxs `seq`
        rnf merkleBuffer `seq`
        rnf txPeerMap

-- | Start the tx manager. The job of this actor is to request tx downloads
-- when receiving tx invs. It also has to buffer solo transactions until the
-- chain is synced up before sending them to the wallet. It will also suspend
-- merkle block delivery while there are inflight transaction downloads. This
-- is to prevent race conditions where the wallet could miss confirmations.
withTxManager 
    :: (MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadMask m) 
    => FilePath
    -> L.Options
    -> TBMChan WalletMessage
    -> (    TBMChan TxManagerMessage 
         -> TBMChan BlockChainMessage 
         -> TBMChan ManagerMessage 
         -> m ()
       )
    -> m ()
withTxManager fp opts wletChan f = do
    txmgChan <- liftIO $ atomically $ newTBMChan 10
    withBlockChain fp opts txmgChan $ \bkchChan mngrChan -> do
        let nodeSynced    = False
            txBuffer      = []
            inflightTxs   = []
            merkleBuffer  = []
            txPeerMap     = M.empty
            session       = TxManagerSession{..}
            -- Run the main txmanager message processing loop
            run = do
                $(logDebug) $ format "TxManager thread started"
                sourceTBMChan txmgChan $$ processTxManagerMessage

        withAsync (evalStateT run session) $ \a ->
            link a >> f txmgChan bkchChan mngrChan

processTxManagerMessage :: (MonadLogger m, MonadIO m) 
                      => Sink TxManagerMessage (StateT TxManagerSession m) ()
processTxManagerMessage = awaitForever $ \req -> lift $ case req of
    TxManagerTxInv pid tids       -> processTxInv pid tids
    TxManagerTx tx fromMerkle     -> processTx tx fromMerkle
    TxManagerGetTx pid tid        -> processGetTx pid tid
    TxManagerSendTx tid           -> processSendTx tid
    TxManagerMerkles action txs   -> processMerkles action txs
    TxManagerBlocks action blocks -> processBlocks action blocks
    TxManagerSynced               -> processSynced
    TxManagerStartDownload valE   -> processStartDownload valE 
    TxManagerStatus               -> processTxManagerStatus

-- | Decide if we want to download a transaction or not. We store inflight
-- transactions.
processTxInv :: (MonadLogger m, MonadIO m) 
             => PeerId -> [TxHash] -> StateT TxManagerSession m ()
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
        modify' $ \s -> s{ inflightTxs = inflight ++ toDwn }
        -- Publish the transaction download job with average priority
        sendManager $ PublishJob (JobDwnTxs toDwn) (ThisPeer pid) 5

processGetTx :: (MonadIO m, MonadLogger m)
             => PeerId
             -> TxHash
             -> StateT TxManagerSession m ()
processGetTx pid tid = do
    $(logDebug) $ format $ unwords
        [ "Requesting transaction", encodeTxHashLE tid, "from wallet." ]
    modify' $ \s -> s{ txPeerMap = M.insertWith addPid tid [pid] (txPeerMap s) }
    sendWallet $ WalletGetTx tid
  where
    addPid p = nub . (p ++)

processSendTx :: (MonadIO m, MonadLogger m)
              => Tx
              -> StateT TxManagerSession m ()
processSendTx tx = do
    $(logDebug) $ format $ unwords
        [ "Sending transaction", encodeTxHashLE (txHash tx), "from wallet." ]
    peers <- M.findWithDefault [] (txHash tx) `liftM` gets txPeerMap
    forM_ peers $ \pid -> sendManager $
        PublishJob (JobSendTx tx) (ThisPeer pid) 1
    modify' $ \s -> s{ txPeerMap = M.delete (txHash tx) (txPeerMap s) }

-- | Send transactions to the wallet only if we are synced. This is to prevent
-- a problem where a transaction that belongs to the wallet in the future
-- might still pass through the early bloom filter and not be recognized 
-- by the wallet.
processTx :: (MonadLogger m, MonadIO m) 
          => Tx 
          -> Bool 
          -> StateT TxManagerSession m ()
processTx tx fromMerkle = do
    synced <- gets nodeSynced 
    when (synced && not fromMerkle) $ $(logInfo) $ format $ concat 
        [ "Received solo tx ", encodeTxHashLE tid
        , ". Sending it to the wallet."
        ]
    if synced || fromMerkle
        -- We are synced. We send solo transactions to the wallet
        -- We also always send merkle transactions to the wallet
        then sendWallet $ WalletTx tx fromMerkle
        -- We are not synced. We buffer transactions until we are synced.
        else do
            $(logInfo) $ format $ concat 
                [ "Received solo tx ", encodeTxHashLE tid
                , ". Buffering it as we are not synced."
                ]
            modify' $ \s -> s{ txBuffer = txBuffer s ++ [tx] }

    -- Remove this transaction from the inflight list
    modify' $ \s -> s{ inflightTxs = delete tid $ inflightTxs s }

    -- Try to import pending merkles
    merkles <- gets merkleBuffer
    modify' $ \s -> s{ merkleBuffer = [] }
    forM_ merkles $ \(action, mTxs) -> processMerkles action mTxs
  where
    tid = txHash tx

-- | Import merkle blocks into the wallet if there are no inflight transactions.
-- Buffer the transaction otherwise.
processMerkles :: (MonadIO m, MonadLogger m)
               => BlockChainAction 
               -> [MerkleTxs] 
               -> StateT TxManagerSession m ()
processMerkles action mTxs = gets inflightTxs >>= \inflight -> if null inflight
    -- No infligh transactions. We can send the merkles to the wallet
    then do
        -- Check if we have solo transactions that belong to these merkle blocks
        txs <- gets txBuffer
        let allExpTxs = concat mTxs
            (toSend, rest) = partition ((`elem` allExpTxs) . txHash) txs
        unless (null toSend) $ do
            $(logDebug) $ format $ unwords
                [ "Sending", show $ length toSend
                , "solo txs to the wallet that belong to merkle blocks" 
                ]
            -- Send those transactions to the wallet
            forM_ toSend $ sendWallet . (flip WalletTx True)
            modify' $ \s -> s{ txBuffer = rest }

        -- There is 1 list of [TxHash] per merkle block
        $(logDebug) $ format $ unwords
            [ "Sending", show $ length mTxs, "merkle blocks to the wallet" ]
            
        -- Send the merkles to the wallet
        sendWallet $ WalletMerkles action mTxs
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
        modify' $ \s -> s{ merkleBuffer = merkleBuffer s ++ [(action, mTxs)] }

-- | Import merkle blocks into the wallet if there are no infligh transactions.
-- Buffer the transaction otherwise.
processBlocks :: (MonadIO m, MonadLogger m)
              => BlockChainAction 
              -> [Block] 
              -> StateT TxManagerSession m ()
processBlocks action blocks = do
    $(logDebug) $ format $ unwords
        [ "Sending", show $ length blocks, "blocks to the wallet." ]
    -- Send the merkles to the wallet
    sendWallet $ WalletBlocks action blocks
    -- some logging
    logBlockChainAction action

logBlockChainAction :: (MonadIO m, MonadLogger m)
                   => BlockChainAction -> StateT TxManagerSession m ()
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
processSynced :: (MonadLogger m, MonadIO m) => StateT TxManagerSession m ()
processSynced = do
    txs <- gets txBuffer
    synced <- gets nodeSynced
    $(logInfo) $ format "Blocks are in sync with the block headers."
    unless (null txs) $ do
        $(logInfo) $ format $ unwords
            [ "Sending", show $ length txs, "txs to the wallet." ]
        forM_ txs $ sendWallet . (flip WalletTx False)
    modify' $ \s -> s{ nodeSynced = True 
                     , txBuffer   = []
                     }
    unless synced $ sendManager $ PublishJob JobMempool (AllPeers1 0) 5
    sendWallet WalletSynced

processStartDownload :: (MonadLogger m, MonadIO m) 
                     => Either Timestamp BlockHash 
                     -> StateT TxManagerSession m ()
processStartDownload _ = do
    $(logDebug) $ format $ unwords
        [ "(Re)starting a new merkle block download."
        , "Cleaning up the merkle buffer."
        ]
    modify' $ \s -> s{ merkleBuffer = [] }

processTxManagerStatus :: (MonadLogger m, MonadIO m) 
                       => StateT TxManagerSession m ()
processTxManagerStatus = do
    TxManagerSession{..} <- get
    $(logInfo) $ format $ unlines
        [ ""
        , "Node Synced   : " ++ show nodeSynced
        , "Tx Buffer     : " ++ (show $ length txBuffer)
        , "Inflight Txs  : " ++ (show $ length inflightTxs)
        , "Merkle Buffer : " ++ (show $ length merkleBuffer)
        , "TxPeer Map    : " ++ (show $ length $ concat $ M.elems txPeerMap)
        ]

{- Helpers -}

-- Send a message to the PeerManager
sendManager :: MonadIO m => ManagerMessage -> StateT TxManagerSession m ()
sendManager msg = do
    chan <- gets mngrChan
    liftIO . atomically $ writeTBMChan chan msg

-- Send a message to the PeerManager
sendWallet :: MonadIO m => WalletMessage -> StateT TxManagerSession m ()
sendWallet msg = do
    chan <- gets wletChan
    liftIO . atomically $ writeTBMChan chan msg

format :: String -> Text
format str = pack $ unwords [ "[TxManager]", str ]

