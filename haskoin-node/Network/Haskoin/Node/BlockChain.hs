{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Network.Haskoin.Node.BlockChain where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async.Lifted (link, mapConcurrently,
                                                  waitAny, waitAnyCancel, withAsync)
import           Control.Concurrent.STM          (STM, atomically, isEmptyTMVar,
                                                  putTMVar, readTVar, retry,
                                                  takeTMVar, tryReadTMVar,
                                                  tryTakeTMVar)
import           Control.Concurrent.STM.Lock     (locked)
import qualified Control.Concurrent.STM.Lock     as Lock (with)
import           Control.Concurrent.STM.TBMChan  (isEmptyTBMChan, readTBMChan)
import           Control.Exception.Lifted        (throw)
import           Control.Monad                   (forM, forM_, forever, unless,
                                                  void, when)
import           Control.Monad.Logger            (MonadLoggerIO, logDebug,
                                                  logError, logInfo, logWarn)
import           Control.Monad.Reader            (ask, asks)
import           Control.Monad.Trans             (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control     (MonadBaseControl, liftBaseOp_)
import qualified Data.ByteString.Char8           as C (unpack)
import           Data.Conduit                    (Source, yield)
import           Data.List                       (nub)
import qualified Data.Map                        as M (delete, keys, lookup,
                                                       null)
import           Data.Maybe                      (listToMaybe)
import qualified Data.Sequence                   as S (length)
import           Data.String.Conversions         (cs)
import           Data.Text                       (pack)
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import           Data.Unique                     (hashUnique)
import           Data.Word                       (Word32)
import           Network.Haskoin.Block
import           Network.Haskoin.Node
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Node.Peer
import           Network.Haskoin.Node.STM
import           Network.Haskoin.Transaction
import           System.Random                   (randomIO)

startSPVNode :: (MonadLoggerIO m, MonadBaseControl IO m)
             => [PeerHost]
             -> BloomFilter
             -> Int
             -> NodeT m ()
startSPVNode hosts bloom elems = do
    $(logDebug) "Setting our bloom filter in the node"
    atomicallyNodeT $ sendBloomFilter bloom elems
    $(logDebug) $ pack $ unwords
        [ "Starting SPV node with", show $ length hosts, "hosts" ]
    withAsync (void $ mapConcurrently startReconnectPeer hosts) $ \a1 -> do
        link a1
        $(logInfo) "Starting the initial header sync"
        headerSync
        $(logInfo) "Initial header sync complete"
        $(logDebug) "Starting the tickle processing thread"
        withAsync processTickles $ \a2 -> link a2 >> do
            _ <- liftIO $ waitAnyCancel [a1, a2]
            return ()
        $(logDebug) "Exiting SPV-node thread"

-- Source of all transaction broadcasts
txSource :: (MonadLoggerIO m, MonadBaseControl IO m)
         => Source (NodeT m) Tx
txSource = do
    chan <- lift $ asks sharedTxChan
    $(logDebug) "Waiting to receive a transaction..."
    resM <- liftIO $ atomically $ readTBMChan chan
    case resM of
        Just (pid, ph, tx) -> do
            $(logInfo) $ formatPid pid ph $ unwords
                [ "Received transaction broadcast", cs $ txHashToHex $ txHash tx ]
            yield tx >> txSource
        _ -> $(logError) "Tx channel closed unexpectedly"

handleGetData :: (MonadLoggerIO m, MonadBaseControl IO m)
              => (TxHash -> m (Maybe Tx))
              -> NodeT m ()
handleGetData handler = forever $ do
    $(logDebug) "Waiting for GetData transaction requests..."
    -- Wait for tx GetData requests to be available
    txids <- atomicallyNodeT $ do
        datMap <- readTVarS sharedTxGetData
        if M.null datMap then lift retry else return $ M.keys datMap
    forM (nub txids) $ \tid -> lift (handler tid) >>= \txM -> do
        $(logDebug) $ pack $ unwords
            [ "Processing GetData txid request", cs $ txHashToHex tid ]
        pidsM <- atomicallyNodeT $ do
            datMap <- readTVarS sharedTxGetData
            writeTVarS sharedTxGetData $ M.delete tid datMap
            return $ M.lookup tid datMap
        case (txM, pidsM) of
            -- Send the transaction to the required peers
            (Just tx, Just pids) -> forM_ pids $ \(pid, ph) -> do
                $(logDebug) $ formatPid pid ph $ unwords
                    [ "Sending tx", cs $ txHashToHex tid, "to peer" ]
                atomicallyNodeT $ trySendMessage pid $ MTx tx
            _ -> return ()

broadcastTxs :: (MonadLoggerIO m, MonadBaseControl IO m)
             => [TxHash]
             -> NodeT m ()
broadcastTxs txids = do
    forM_ txids $ \tid -> $(logInfo) $ pack $ unwords
        [ "Transaction INV broadcast:", cs $ txHashToHex tid ]
    -- Broadcast an INV message for new transactions
    let msg = MInv $ Inv $ map (InvVector InvTx . getTxHash) txids
    atomicallyNodeT $ sendMessageAll msg

rescanTs :: Timestamp -> NodeT STM ()
rescanTs ts = do
    rescanTMVar <- asks sharedRescan
    lift $ do
        -- Make sure the TMVar is empty
        _ <- tryTakeTMVar rescanTMVar
        putTMVar rescanTMVar $ Left ts

rescanHeight :: BlockHeight -> NodeT STM ()
rescanHeight h = do
    rescanTMVar <- asks sharedRescan
    lift $ do
        -- Make sure the TMVar is empty
        _ <- tryTakeTMVar rescanTMVar
        putTMVar rescanTMVar $ Right h

-- Wait for the next merkle batch to be available. This function will check for
-- rescans.
merkleDownload
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => BlockHash
    -> Word32
    -> NodeT m
       (BlockChainAction, Source (NodeT m) (Either (MerkleBlock, MerkleTxs) Tx))
merkleDownload walletHash batchSize = do
    -- Store the best block received from the wallet for information only.
    -- This will be displayed in `hw status`
    merkleSyncedActions walletHash
    walletBlockM <- runSqlNodeT $ getBlockByHash walletHash
    walletBlock <- case walletBlockM of
        Just walletBlock -> do
            atomicallyNodeT $ writeTVarS sharedBestBlock walletBlock
            return walletBlock
        Nothing ->
            error "Could not find wallet best block in headers"
    rescanTMVar <- asks sharedRescan
    -- Wait either for a new block to arrive or a rescan to be triggered
    $(logDebug) "Waiting for a new block or a rescan..."
    resE <- atomicallyNodeT $ orElseNodeT
        (fmap Left $ lift $ takeTMVar rescanTMVar)
        (const (Right ()) <$> waitNewBlock walletHash)
    resM <- case resE of
        -- A rescan was triggered
        Left valE -> do
            $(logInfo) $ pack $ unwords
                [ "Got rescan request", show valE ]
            -- Wait until rescan conditions are met
            newValE <- waitRescan rescanTMVar valE
            $(logDebug) $ pack $ unwords
                [ "Rescan condition reached:", show newValE ]
            case newValE of
                Left ts -> tryMerkleDwnTimestamp ts batchSize
                Right _ -> tryMerkleDwnHeight walletBlock batchSize
        -- Continue download from a hash
        Right _ -> tryMerkleDwnBlock walletBlock batchSize
    case resM of
        Just res -> return res
        _ -> do
            $(logWarn) "Invalid merkleDownload result. Retrying ..."
            -- Sleep 10 seconds and retry
            liftIO $ threadDelay $ 10*1000000
            merkleDownload walletHash batchSize
  where
    waitRescan rescanTMVar valE = do
        resE <- atomicallyNodeT $ orElseNodeT
            (fmap Left (lift $ takeTMVar rescanTMVar))
            (waitVal valE >> return (Right valE))
        case resE of
            Left newValE -> waitRescan rescanTMVar newValE
            Right res -> return res
    waitVal valE = case valE of
        Left ts -> waitFastCatchup ts
        Right h -> waitHeight h

-- | Perform some actions only when headers have been synced.
merkleSyncedActions
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => BlockHash -- ^ Wallet best block
    -> NodeT m ()
merkleSyncedActions walletHash =
    asks sharedSyncLock >>= \lock -> liftBaseOp_ (Lock.with lock) $ do
    -- Check if we are synced
    (synced, mempool, header) <- atomicallyNodeT $ do
        header <- readTVarS sharedBestHeader
        synced <- areBlocksSynced walletHash
        mempool <- readTVarS sharedMempool
        return (synced, mempool, header)
    when synced $ do
        $(logInfo) $ pack $ unwords
            [ "Merkle blocks are in sync with the"
            , "network at height", show walletHash
            ]
        -- Prune side chains
        bestBlock <- runSqlNodeT $ pruneChain header
        atomicallyNodeT $ do
            -- Update shared best header after pruning
            writeTVarS sharedBestHeader bestBlock
            writeTVarS sharedMerklePeer Nothing
        -- Do a mempool sync on the first merkle sync
        unless mempool $ do
            atomicallyNodeT $ do
                sendMessageAll MMempool
                writeTVarS sharedMempool True
            $(logInfo) "Requesting a mempool sync"

-- Wait for headers to catch up to the given height
waitHeight :: BlockHeight -> NodeT STM ()
waitHeight height = do
    node <- readTVarS sharedBestHeader
    -- Check if we passed the timestamp condition
    unless (height < nodeBlockHeight node) $ lift retry

-- Wait for headers to catch up to the given timestamp
waitFastCatchup :: Timestamp -> NodeT STM ()
waitFastCatchup ts = do
    node <- readTVarS sharedBestHeader
    -- Check if we passed the timestamp condition
    unless (ts < blockTimestamp (nodeHeader node)) $
        lift retry

-- Wait for a new block to be available for download
waitNewBlock :: BlockHash -> NodeT STM ()
waitNewBlock bh = do
    node <- readTVarS sharedBestHeader
    -- We have more merkle blocks to download
    unless (bh /= nodeHash node) $
        lift retry

tryMerkleDwnHeight
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => NodeBlock
    -> Word32
    -> NodeT m (Maybe (BlockChainAction,
                       Source (NodeT m) (Either (MerkleBlock, MerkleTxs) Tx)))
tryMerkleDwnHeight block batchSize = do
    $(logInfo) $ pack $ unwords
        [ "Requesting merkle blocks at height", show $ nodeBlockHeight block
        , "with batch size", show batchSize
        ]
    -- Request height - 1 as we want to start downloading at height
    nodeM <- runSqlNodeT $ getParentBlock block
    case nodeM of
        Just bn ->
            tryMerkleDwnBlock bn batchSize
        _ -> do
            $(logDebug) $ pack $ unwords
                [ "Can't download merkle blocks."
                , "Waiting for headers to sync ..."
                ]
            return Nothing

tryMerkleDwnTimestamp
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Timestamp
    -> Word32
    -> NodeT m (Maybe (BlockChainAction,
                       Source (NodeT m) (Either (MerkleBlock, MerkleTxs) Tx)))
tryMerkleDwnTimestamp ts batchSize = do
    $(logInfo) $ pack $ unwords
        [ "Requesting merkle blocks after timestamp", show ts
        , "with batch size", show batchSize
        ]
    nodeM <- runSqlNodeT $ getBlockAfterTime ts
    case nodeM of
        Just bh ->
            tryMerkleDwnBlock bh batchSize
        _ -> do
            $(logDebug) $ pack $ unwords
                [ "Can't download merkle blocks."
                , "Waiting for headers to sync ..."
                ]
            return Nothing

tryMerkleDwnBlock
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => NodeBlock
    -> Word32
    -> NodeT m (Maybe (BlockChainAction,
                       Source (NodeT m) (Either (MerkleBlock, MerkleTxs) Tx)))
tryMerkleDwnBlock bh batchSize = do
    $(logDebug) $ pack $ unwords
        [ "Requesting merkle download from block"
        , cs $ blockHashToHex (nodeHash bh)
        , "and batch size", show batchSize
        ]
    -- Get the list of merkle blocks to download from our headers
    best <- atomicallyNodeT $ readTVarS sharedBestHeader
    action <- runSqlNodeT $ getBlockWindow best bh batchSize
    case actionNodes action of
        [] -> do
            $(logError) "BlockChainAction was empty"
            return Nothing
        ns -> do
            -- Wait for a peer available for merkle download
            (pid, PeerSession{..}) <- waitMerklePeer $
                nodeBlockHeight $ last ns

            $(logDebug) $ formatPid pid peerSessionHost $ unwords
                [ "Found merkle downloading peer with score"
                , show peerSessionScore
                ]

            let source = peerMerkleDownload pid peerSessionHost action
            return $ Just (action, source)
  where
    waitMerklePeer height = atomicallyNodeT $ do
        pidM <- readTVarS sharedHeaderPeer
        allPeers <- getPeersAtHeight (>= height)
        let f (pid,_) = Just pid /= pidM
            -- Filter out the peer syncing headers (if there is one)
            peers = filter f allPeers
        case listToMaybe peers of
            Just res@(pid,_) -> do
                writeTVarS sharedMerklePeer $ Just pid
                return res
            _ -> lift retry

peerMerkleDownload
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => PeerId
    -> PeerHost
    -> BlockChainAction
    -> Source (NodeT m) (Either (MerkleBlock, MerkleTxs) Tx)
peerMerkleDownload pid ph action = do
    let bids = map nodeHash $ actionNodes action
        vs   = map (InvVector InvMerkleBlock . getBlockHash) bids
    $(logInfo) $ formatPid pid ph $ unwords
        [ "Requesting", show $ length bids, "merkle block(s)" ]
    nonce <- liftIO randomIO
    -- Request a merkle batch download
    sessM <- lift . atomicallyNodeT $ do
        _ <- trySendMessage pid $ MGetData $ GetData vs
        -- Send a ping to have a recognizable end message for
        -- the last merkle block download.
        _ <- trySendMessage pid $ MPing $ Ping nonce
        tryGetPeerSession pid
    case sessM of
        Just PeerSession{..} -> checkOrder peerSessionMerkleChan bids
        _ -> lift . atomicallyNodeT $
            writeTVarS sharedMerklePeer Nothing
  where
    -- Build a source that that will check the order of the received merkle
    -- blocks against the initial request. If merkle blocks are sent out of
    -- order, the source will close and the peer will be flagged as
    -- misbehaving. The source will also close once all the requested merkle
    -- blocks have been received from the peer.
    checkOrder _ [] = lift . atomicallyNodeT $
        writeTVarS sharedMerklePeer Nothing
    checkOrder chan (bid:bids) = do
        -- Read the channel or disconnect the peer after waiting for 2 minutes
        resM <- lift $ raceTimeout 120
                    (disconnectPeer pid ph)
                    (liftIO . atomically $ readTBMChan chan)
        case resM of
            -- Forward transactions
            Right (Just res@(Right _)) ->
                yield res >> checkOrder chan (bid:bids)
            Right (Just res@(Left (MerkleBlock mHead _ _ _, _))) -> do
                let mBid = headerHash mHead
                $(logDebug) $ formatPid pid ph $ unwords
                    [ "Processing merkle block", cs $ blockHashToHex mBid ]
                -- Check if we were expecting this merkle block
                if mBid == bid
                    then yield res >> checkOrder chan bids
                    else lift $ do
                        atomicallyNodeT $ writeTVarS sharedMerklePeer Nothing
                        -- If we were not expecting this merkle block, do not
                        -- yield the merkle block and close the source
                        misbehaving pid ph moderateDoS $ unwords
                            [ "Peer sent us merkle block hash"
                            , cs $ blockHashToHex $ headerHash mHead
                            , "but we expected merkle block hash"
                            , cs $ blockHashToHex bid
                            ]
                        -- Not sure how to recover from this situation.
                        -- Disconnect the peer. TODO: Is there a way to recover
                        -- without buffering the whole batch in memory and
                        -- re-order it?
                        disconnectPeer pid ph
            -- The channel closed. Stop here.
            _ -> do
                $(logWarn) $ formatPid pid ph
                    "Merkle channel closed unexpectedly"
                lift $ atomicallyNodeT $ writeTVarS sharedMerklePeer Nothing

processTickles :: (MonadLoggerIO m, MonadBaseControl IO m)
               => NodeT m ()
processTickles = forever $ do
    $(logDebug) $ pack "Waiting for a block tickle ..."
    (pid, ph, tickle) <- atomicallyNodeT waitTickle
    $(logInfo) $ formatPid pid ph $ unwords
        [ "Received block tickle", cs $ blockHashToHex tickle ]
    heightM <- fmap nodeBlockHeight <$> runSqlNodeT (getBlockByHash tickle)
    case heightM of
        Just height -> do
            $(logInfo) $ formatPid pid ph $ unwords
                [ "The block tickle", cs $ blockHashToHex tickle
                , "is already connected"
                ]
            updatePeerHeight pid ph height
        _ -> do
            $(logDebug) $ formatPid pid ph $ unwords
                [ "The tickle", cs $ blockHashToHex tickle
                , "is unknown. Requesting a peer header sync."
                ]
            peerHeaderSyncFull pid ph `catchAny` const (disconnectPeer pid ph)
            newHeightM <-
                fmap nodeBlockHeight <$> runSqlNodeT (getBlockByHash tickle)
            case newHeightM of
                Just height -> do
                    $(logInfo) $ formatPid pid ph $ unwords
                        [ "The block tickle", cs $ blockHashToHex tickle
                        , "was connected successfully"
                        ]
                    updatePeerHeight pid ph height
                _ -> $(logWarn) $ formatPid pid ph $ unwords
                    [ "Could not find the height of block tickle"
                    , cs $ blockHashToHex tickle
                    ]
  where
    updatePeerHeight pid ph height = do
        $(logInfo) $ formatPid pid ph $ unwords
            [ "Updating peer height to", show height ]
        atomicallyNodeT $ do
            modifyPeerSession pid $ \s ->
                s{ peerSessionHeight = height }
            updateNetworkHeight

waitTickle :: NodeT STM (PeerId, PeerHost, BlockHash)
waitTickle = do
    tickleChan <- asks sharedTickleChan
    resM <- lift $ readTBMChan tickleChan
    case resM of
        Just res -> return res
        _ -> throw $ NodeException "tickle channel closed unexpectedly"

syncedHeight :: MonadIO m => NodeT m (Bool, Word32)
syncedHeight = atomicallyNodeT $ do
    synced <- areHeadersSynced
    ourHeight <- nodeBlockHeight <$> readTVarS sharedBestHeader
    return (synced, ourHeight)

headerSync :: (MonadLoggerIO m, MonadBaseControl IO m)
           => NodeT m ()
headerSync = do
    -- Start the header sync
    $(logDebug) "Syncing more headers. Finding the best peer..."
    (pid, PeerSession{..}) <- atomicallyNodeT $ do
        peers <- getPeersAtNetHeight
        case listToMaybe peers of
            Just res@(pid,_) -> do
                -- Save the header syncing peer
                writeTVarS sharedHeaderPeer $ Just pid
                return res
            _ -> lift retry

    $(logDebug) $ formatPid pid peerSessionHost $ unwords
        [ "Found best header syncing peer with score"
        , show peerSessionScore
        ]

    -- Run a maximum of 10 header downloads with this peer.
    -- Then we re-evaluate the best peer
    continue <- catchAny (peerHeaderSyncLimit pid peerSessionHost 10) $
        \e -> do
            $(logError) $ pack $ unwords ["peerHeaderSyncLimit:", show e]
            disconnectPeer pid peerSessionHost >> return True

    -- Reset the header syncing peer
    atomicallyNodeT $ writeTVarS sharedHeaderPeer Nothing

    -- Check if we should continue the header sync
    if continue then headerSync else do
        (synced, ourHeight) <- syncedHeight
        if synced
            then do
                $(logInfo) $ formatPid pid peerSessionHost $ unwords
                    [ "Block headers are in sync with the"
                    , "network at height", show ourHeight
                    ]
            -- Continue the download if we are not yet synced
            else headerSync

peerHeaderSyncLimit :: (MonadLoggerIO m, MonadBaseControl IO m)
                    => PeerId
                    -> PeerHost
                    -> Int
                    -> NodeT m Bool
peerHeaderSyncLimit pid ph initLimit
    | initLimit < 1 = error "Limit must be at least 1"
    | otherwise = go initLimit Nothing
  where
    go limit prevM = peerHeaderSync pid ph prevM >>= \actionM -> case actionM of
        Just action ->
            -- If we received a side chain or a known chain, we want to
            -- continue szncing from this peer even if the limit has been
            -- reached.
            if limit > 1 || isSideChain action || isKnownChain action
                then go (limit - 1) actionM
                -- We got a Just, so we can continue the download from
                -- this peer
                else return True
        _ -> return False

-- Sync all the headers from a given peer
peerHeaderSyncFull :: (MonadLoggerIO m, MonadBaseControl IO m)
                   => PeerId
                   -> PeerHost
                   -> NodeT m ()
peerHeaderSyncFull pid ph =
    go Nothing
  where
    go prevM = peerHeaderSync pid ph prevM >>= \actionM -> case actionM of
        Just _  -> go actionM
        Nothing -> do
            (synced, ourHeight) <- syncedHeight
            when synced $ $(logInfo) $ formatPid pid ph $ unwords
                [ "Block headers are in sync with the"
                , "network at height", show ourHeight
                ]

areBlocksSynced :: BlockHash -> NodeT STM Bool
areBlocksSynced walletHash = do
    headersSynced <- areHeadersSynced
    bestHeader    <- readTVarS sharedBestHeader
    return $ headersSynced && walletHash == nodeHash bestHeader

-- Check if the block headers are synced with the network height
areHeadersSynced :: NodeT STM Bool
areHeadersSynced = do
    ourHeight <- nodeBlockHeight <$> readTVarS sharedBestHeader
    netHeight <- readTVarS sharedNetworkHeight
    -- If netHeight == 0 then we did not connect to any peers yet
    return $ ourHeight >= netHeight && netHeight > 0

-- | Sync one batch of headers from the given peer. Accept the result of a
-- previous peerHeaderSync to correctly compute block locators in the
-- presence of side chains.
peerHeaderSync :: (MonadLoggerIO m, MonadBaseControl IO m)
               => PeerId
               -> PeerHost
               -> Maybe BlockChainAction
               -> NodeT m (Maybe BlockChainAction)
peerHeaderSync pid ph prevM = do
    $(logDebug) $ formatPid pid ph "Waiting for the HeaderSync lock"
    -- Aquire the header syncing lock
    lock <- asks sharedSyncLock
    liftBaseOp_ (Lock.with lock) $ do

        best <- atomicallyNodeT $ readTVarS sharedBestHeader

        -- Retrieve the block locator
        loc <- case prevM of
            Just (KnownChain ns) -> do
                $(logDebug) $ formatPid pid ph "Building a known chain locator"
                runSqlNodeT $ blockLocator $ last ns
            Just (SideChain ns) -> do
                $(logDebug) $ formatPid pid ph "Building a side chain locator"
                runSqlNodeT $ blockLocator $ last ns
            Just (BestChain ns) -> do
                $(logDebug) $ formatPid pid ph "Building a best chain locator"
                runSqlNodeT $ blockLocator $ last ns
            Just (ChainReorg _ _ ns) -> do
                $(logDebug) $ formatPid pid ph "Building a reorg locator"
                runSqlNodeT $ blockLocator $ last ns
            Nothing -> do
                $(logDebug) $ formatPid pid ph "Building a locator to best"
                runSqlNodeT $ blockLocator best

        $(logDebug) $ formatPid pid ph $ unwords
            [ "Requesting headers with block locator of size"
            , show $ length loc
            , "Start block:", cs $ blockHashToHex $ head loc
            , "End block:",   cs $ blockHashToHex $ last loc
            ]

        -- Send a GetHeaders message to the peer
        atomicallyNodeT $ sendMessage pid $ MGetHeaders $ GetHeaders 0x01 loc z

        $(logDebug) $ formatPid pid ph "Waiting 2 minutes for headers..."

        -- Wait 120 seconds for a response or time out
        continueE <- raceTimeout 120 (disconnectPeer pid ph) (waitHeaders best)

        -- Return True if we can continue syncing from this peer
        return $ either (const Nothing) id continueE
  where
    z = "0000000000000000000000000000000000000000000000000000000000000000"
    -- Wait for the headers to be available
    waitHeaders best = do
        (rPid, headers) <- atomicallyNodeT $ takeTMVarS sharedHeaders
        if rPid == pid
            then processHeaders best headers
            else waitHeaders best
    processHeaders _ (Headers []) = do
        $(logDebug) $ formatPid pid ph
            "Received empty headers. Finished downloading headers."
        -- Do not continue the header download
        return Nothing
    processHeaders best (Headers hs) = do
        $(logDebug) $ formatPid pid ph $ unwords
            [ "Received", show $ length hs, "headers."
            , "Start blocks:", cs $ blockHashToHex $ headerHash $ fst $ head hs
            , "End blocks:", cs $ blockHashToHex $ headerHash $ fst $ last hs
            ]
        now <- round <$> liftIO getPOSIXTime
        actionE <- runSqlNodeT $ connectHeaders best (map fst hs) now
        case actionE of
            Left err -> do
                misbehaving pid ph severeDoS err
                return Nothing
            Right action -> case actionNodes action of
                [] -> do
                    $(logWarn) $ formatPid pid ph $ unwords
                        [ "Received an empty blockchain action:", show action ]
                    return Nothing
                nodes -> do
                    $(logDebug) $ formatPid pid ph $ unwords
                        [ "Received", show $ length nodes
                        , "nodes in the action"
                        ]
                    let height = nodeBlockHeight $ last nodes
                    case action of
                        KnownChain _ ->
                            $(logInfo) $ formatPid pid ph $ unwords
                                [ "KnownChain headers received"
                                , "up to height", show height
                                ]
                        SideChain _ ->
                            $(logInfo) $ formatPid pid ph $ unwords
                                [ "SideChain headers connected successfully"
                                , "up to height", show height
                                ]
                        -- Headers extend our current best head
                        _ -> do
                            $(logInfo) $ formatPid pid ph $ unwords
                                [ "Best headers connected successfully"
                                , "up to height", show height
                                ]
                            atomicallyNodeT $
                                writeTVarS sharedBestHeader $ last nodes
                    -- If we received less than 2000 headers, we are done
                    -- syncing from this peer and we return Nothing.
                    return $ if length hs < 2000
                        then Nothing
                        else Just action

nodeStatus :: NodeT STM NodeStatus
nodeStatus = do
    nodeStatusPeers <- mapM peerStatus =<< getPeers
    SharedNodeState{..} <- ask
    lift $ do
        best   <- readTVar sharedBestBlock
        header <- readTVar sharedBestHeader
        let nodeStatusBestBlock        = nodeHash best
            nodeStatusBestBlockHeight  = nodeBlockHeight best
            nodeStatusBestHeader       = nodeHash header
            nodeStatusBestHeaderHeight = nodeBlockHeight header
        nodeStatusNetworkHeight <-
            readTVar sharedNetworkHeight
        nodeStatusBloomSize <-
            maybe 0 (S.length . bloomData . fst) <$> readTVar sharedBloomFilter
        nodeStatusHeaderPeer <-
            fmap hashUnique <$> readTVar sharedHeaderPeer
        nodeStatusMerklePeer <-
            fmap hashUnique <$> readTVar sharedMerklePeer
        nodeStatusHaveHeaders <-
            not <$> isEmptyTMVar sharedHeaders
        nodeStatusHaveTickles <-
            not <$> isEmptyTBMChan sharedTickleChan
        nodeStatusHaveTxs <-
            not <$> isEmptyTBMChan sharedTxChan
        nodeStatusGetData <-
            M.keys <$> readTVar sharedTxGetData
        nodeStatusRescan <-
            tryReadTMVar sharedRescan
        nodeStatusMempool <-
            readTVar sharedMempool
        nodeStatusSyncLock <-
            locked sharedSyncLock
        return NodeStatus{..}

peerStatus :: (PeerId, PeerSession) -> NodeT STM PeerStatus
peerStatus (pid, PeerSession{..}) = do
    hostM <- getHostSession peerSessionHost
    let peerStatusPeerId         = hashUnique pid
        peerStatusHost           = peerSessionHost
        peerStatusConnected      = peerSessionConnected
        peerStatusHeight         = peerSessionHeight
        peerStatusProtocol       = version <$> peerSessionVersion
        peerStatusUserAgent =
            C.unpack . getVarString . userAgent <$> peerSessionVersion
        peerStatusPing           = show <$> peerSessionScore
        peerStatusDoSScore       = peerHostSessionScore <$> hostM
        peerStatusLog            = peerHostSessionLog <$> hostM
        peerStatusReconnectTimer = peerHostSessionReconnect <$> hostM
    lift $ do
        peerStatusHaveMerkles <- not <$> isEmptyTBMChan peerSessionMerkleChan
        peerStatusHaveMessage <- not <$> isEmptyTBMChan peerSessionChan
        peerStatusPingNonces  <- readTVar peerSessionPings
        return PeerStatus{..}

