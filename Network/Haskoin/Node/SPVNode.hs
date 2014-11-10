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
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe)
import Data.Word (Word32)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Default (def)
import Data.List (nub, partition, delete, maximumBy, dropWhileEnd, (\\))
import qualified Data.ByteString as BS (empty)
import qualified Data.Map as M 
    ( Map
    , insert
    , member
    , delete
    , lookup
    , fromList
    , fromListWith
    , keys
    , elems
    , toList
    , toAscList
    , null
    , empty
    , partition
    , map
    , filter
    , adjust
    , update
    , singleton
    , unionWith
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

import Network.Haskoin.Util
import Network.Haskoin.Constants
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
    | NodeRescan Timestamp
    | Heartbeat

data SPVEvent 
    = MerkleBlockEvent [(BlockChainAction, [TxHash])]
    | TxEvent [Tx]

data SPVSession = SPVSession
    { -- Server event channel
      spvEventChan :: TBMChan SPVEvent
      -- Peer currently synchronizing the block headers
    , spvSyncPeer :: Maybe RemoteHost
      -- Latest bloom filter provided by the wallet
    , spvBloom :: Maybe BloomFilter
      -- Block hashes that have to be downloaded
    , blocksToDwn :: M.Map BlockHeight [BlockHash]
      -- Received merkle blocks pending to be sent to the wallet
    , receivedMerkle :: M.Map BlockHeight [DecodedMerkleBlock]
      -- The best merkle block hash
    , bestBlockHash :: BlockHash
      -- Transactions that have not been sent in a merkle block.
      -- We stall solo transactions until the merkle blocks are synced.
    , soloTxs :: [Tx]
      -- Transactions from users that need to be broadcasted
    , pendingTxBroadcast :: [Tx]
      -- This flag is set if the wallet triggered a rescan.
      -- The rescan can only be executed if no merkle block are still
      -- being downloaded.
    , pendingRescan :: Maybe Timestamp
      -- How many merkle blocks requests are batched together in a GetData
      -- request and how many merkle blocks are sent together to the wallet.
    , blockBatch :: Int
      -- Do not request merkle blocks with a timestamp before the
      -- fast catchup time.
    , fastCatchup :: Timestamp
      -- Block hashes that a peer advertised to us but we haven't linked them
      -- yet to our chain. We use this list to update the peer height once
      -- those blocks are linked.
    , peerBroadcastBlocks :: M.Map RemoteHost [BlockHash]
      -- Inflight merkle block requests for each peer
    , peerInflightMerkles :: 
        M.Map RemoteHost [((BlockHeight, BlockHash), Timestamp)]
      -- Inflight transaction requests for each peer. We are waiting for
      -- the GetData response. We stall merkle blocks if there are pending
      -- transaction downloads.
    , peerInflightTxs :: M.Map RemoteHost [(TxHash, Timestamp)]
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
    saveWalletHash :: Word256 -> SPVHandle m ()
    existsWalletHash :: Word256 -> SPVHandle m Bool
    clearWalletHash :: SPVHandle m ()

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
             -> Int
             -> Timestamp
             -> BlockHash
             -> (m () -> IO ())
             -> (TBMChan SPVEvent -> TBMChan SPVRequest -> Async () -> IO ())
             -> IO ()
withAsyncSPV hosts batch fc bb runStack f = do
    -- Channel for sending server events to the wallet
    eChan <- liftIO $ atomically $ newTBMChan 10000
    let session = SPVSession
            { spvEventChan = eChan
            , spvSyncPeer = Nothing
            , spvBloom = Nothing
            , blocksToDwn = M.empty
            , receivedMerkle = M.empty
            , bestBlockHash = bb
            , soloTxs = []
            , pendingTxBroadcast = []
            , pendingRescan = Nothing
            , blockBatch = batch
            , fastCatchup = fc
            , peerBroadcastBlocks = M.empty
            , peerInflightMerkles = M.empty
            , peerInflightTxs = M.empty
            }
        g = runStack . (flip S.evalStateT session)

    -- Launch PeerManager main loop
    withAsyncNode hosts g $ \rChan a -> 
        -- Launch heartbeat thread to monitor stalled requests
        withAsync (heartbeat rChan) $ \_ -> f eChan rChan a

heartbeat :: TBMChan SPVRequest -> IO ()
heartbeat rChan = forever $ do
    threadDelay $ 1000000 * 120 -- Sleep for 2 minutes
    atomically $ writeTBMChan rChan Heartbeat

spvPeerMessage :: SPVNode s m => RemoteHost -> Message -> SPVHandle m ()
spvPeerMessage remote msg = case msg of
    MHeaders headers -> processHeaders remote headers
    MInv inv -> processInv remote inv
    MTx tx -> processTx remote tx
    _ -> return ()

spvNodeRequest :: SPVNode s m => SPVRequest -> SPVHandle m ()
spvNodeRequest req = case req of
    BloomFilterUpdate bf -> processBloomFilter bf
    PublishTx tx -> publishTx tx
    NodeRescan ts -> processRescan ts
    Heartbeat -> heartbeatMonitor

spvInitNode :: SPVNode s m => SPVHandle m ()
spvInitNode = do
    -- Initialize the block header database
    runHeaderChain initHeaderChain 

    -- Adjust the bestBlockHash if it is before the fastCatchup time
    fc <- lift $ S.gets fastCatchup
    bb <- lift $ S.gets bestBlockHash
    bestNode <- runHeaderChain $ getBlockHeaderNode bb
    when (blockTimestamp (nodeHeader bestNode) < fc) $ do
        bestHash <- runHeaderChain $ blockBeforeTimestamp fc
        lift $ S.modify $ \s -> s{ bestBlockHash = bestHash }

    -- Set the block hashes that need to be downloaded
    bestHash <- lift $ S.gets bestBlockHash
    toDwn <- runHeaderChain $ blocksToDownload bestHash
    let toDwnList = map (\(a,b) -> (a,[b])) toDwn
    lift $ S.modify $ \s -> s{ blocksToDwn = M.fromListWith (++) toDwnList }

    -- Mark the genesis block as sent to the wallet
    saveWalletHash $ fromIntegral $ headerHash genesisHeader

{- Peer events -}

spvStartPeer :: SPVNode s m => String -> Int -> SPVHandle m ()
spvStartPeer host port = return ()

spvRestartPeer :: SPVNode s m => RemoteHost -> SPVHandle m ()
spvRestartPeer remote = return ()

spvPeerHandshake :: SPVNode s m => RemoteHost -> Version -> SPVHandle m ()
spvPeerHandshake remote ver = do
    -- Send a bloom filter if we have one
    bloomM <- lift $ S.gets spvBloom
    let filterLoad = MFilterLoad $ FilterLoad $ fromJust bloomM
    when (isJust bloomM) $ sendMessage remote filterLoad

    -- Send wallet transactions that are pending a network broadcast
    -- TODO: Is it enough just to broadcast to 1 peer ?
    -- TODO: Should we send an INV message first ?
    pendingTxs <- lift $ S.gets pendingTxBroadcast
    forM_ pendingTxs $ \tx -> sendMessage remote $ MTx tx
    lift $ S.modify $ \s -> s{ pendingTxBroadcast = [] }

    -- Send a GetHeaders regardless if there is already a peerSync. This peer
    -- could still be faster and become the new peerSync.
    sendGetHeaders remote True 0x00

    -- Trigger merkle block downloads if some are pending
    downloadBlocks remote

    logPeerSynced remote ver

spvPeerDisconnect :: SPVNode s m => RemoteHost -> SPVHandle m ()
spvPeerDisconnect remote = do
    remotePeers <- liftM (delete remote) getPeerKeys

    peerBlockMap <- lift $ S.gets peerBroadcastBlocks
    peerMerkleMap <- lift $ S.gets peerInflightMerkles
    peerTxMap <- lift $ S.gets peerInflightTxs

    -- Inflight merkle blocks are sent back to the download queue
    let toDwn = map fst $ fromMaybe [] $ M.lookup remote peerMerkleMap
    unless (null toDwn) $ do
        $(logDebug) $ T.pack $ unwords
            [ "Peer had inflight merkle blocks. Adding them to download queue:"
            , "[", unwords $ 
                map (encodeBlockHashLE . snd) toDwn
            , "]"
            ]
        -- Add the block hashes to the download queue
        addBlocksToDwn toDwn
        -- Request new merkle block downloads
        forM_ remotePeers downloadBlocks

    -- Remove any state related to this remote peer
    lift $ S.modify $ \s -> 
        s{ peerBroadcastBlocks = M.delete remote peerBlockMap
         , peerInflightMerkles = M.delete remote peerMerkleMap
         , peerInflightTxs     = M.delete remote peerTxMap
         }

    -- Find a new block header synchronizing peer
    syn <- lift $ S.gets spvSyncPeer
    when (syn == Just remote) $ do
        $(logInfo) "Finding a new peer to synchronize the block headers"
        lift $ S.modify $ \s -> s{ spvSyncPeer = Nothing }
        forM_ remotePeers $ \r -> sendGetHeaders r True 0x00

{- Network events -}

processHeaders :: SPVNode s m => RemoteHost -> Headers -> SPVHandle m ()
processHeaders remote (Headers hs) = do
    adjustedTime <- liftM round $ liftIO getPOSIXTime
    -- Save best work before the header import
    workBefore <- liftM nodeChainWork $ runHeaderChain getBestBlockHeader

    -- Import the headers into the header chain
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
    -- Save best work after the header import
    workAfter <- liftM nodeChainWork $ runHeaderChain getBestBlockHeader

    -- Update the bestBlock for blocks before the fastCatchup time
    fc <- lift $ S.gets fastCatchup
    let isAfterCatchup = (>= fc) . blockTimestamp . nodeHeader
        fcBlocks       = dropWhileEnd isAfterCatchup newBlocks
        fcBlock        = last fcBlocks
    unless (null fcBlocks) $ do
        bestBlock <- lift $ S.gets bestBlockHash
        bestNode  <- runHeaderChain $ getBlockHeaderNode bestBlock
        -- If there are nodes >= fastCatchup in between nodes < fastCatchup,
        -- those nodes will be downloaded and simply reported as SideBlocks.
        when (nodeChainWork fcBlock > nodeChainWork bestNode) $
            lift $ S.modify $ \s -> s{ bestBlockHash = nodeBlockHash fcBlock }

    -- Add blocks hashes to the download queue
    let f n = (nodeHeaderHeight n, nodeBlockHash n)
    addBlocksToDwn $ map f $ drop (length fcBlocks) newBlocks

    -- Adjust the height of peers that sent us INV messages for these headers
    forM_ newBlocks $ \n -> do
        broadcastMap <- lift $ S.gets peerBroadcastBlocks
        newList <- forM (M.toList broadcastMap) $ \(remote, hs) -> do
            when (nodeBlockHash n `elem` hs) $
                increasePeerHeight remote $ nodeHeaderHeight n
            return (remote, filter (/= nodeBlockHash n) hs)
        lift $ S.modify $ \s -> s{ peerBroadcastBlocks = M.fromList newList }

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

    -- Request block downloads for all peers that are currently idling
    remotePeers <- getPeerKeys
    forM_ remotePeers downloadBlocks

processInv :: SPVNode s m => RemoteHost -> Inv -> SPVHandle m ()
processInv remote (Inv vs) = do

    -- Process transactions
    unless (null txlist) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Got tx inv"
            , "["
            , unwords $ map encodeTxHashLE txlist
            , "]"
            , "(", show remote, ")" 
            ]
        -- Filter transactions that have not been sent to the wallet yet
        let f = (not <$>) . existsWalletHash . fromIntegral
        notHaveTxs <- filterM f txlist
        downloadTxs remote notHaveTxs

    -- Process blocks
    unless (null blocklist) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Got block inv"
            , "["
            , unwords $ map encodeBlockHashLE blocklist
            , "]"
            , "(", show remote, ")" 
            ]

        -- Partition blocks that we know and don't know
        let f (a,b) h = existsBlockHeaderNode h >>= \exists -> if exists
                then getBlockHeaderNode h >>= \r -> return (r:a,b)
                else return (a,h:b)

        (have, notHave) <- runHeaderChain $ foldM f ([],[]) blocklist

        -- Update peer height
        let maxHeight = maximum $ 0 : map nodeHeaderHeight have
        increasePeerHeight remote maxHeight

        -- Update broadcasted block list
        addBroadcastBlocks remote notHave

        -- Request headers for blocks we don't have. 
        -- TODO: Filter duplicate requests
        forM_ notHave $ \b -> sendGetHeaders remote True b
  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs

-- These are solo transactions not linked to a merkle block (yet)
processTx :: SPVNode s m => RemoteHost -> Tx -> SPVHandle m ()
processTx remote tx = do
    -- Only process the transaction if we have not sent it to the wallet yet.
    alreadyHave <- existsWalletHash $ fromIntegral txhash
    unless alreadyHave $ do
        -- Only send to wallet if we are in sync
        synced <- merkleBlocksSynced
        if synced 
            then do
                $(logInfo) $ T.pack $ unwords 
                    [ "Got solo tx"
                    , encodeTxHashLE txhash 
                    , "Sending to the wallet"
                    ]
                eChan <- lift $ S.gets spvEventChan
                liftIO $ atomically $ writeTBMChan eChan $ TxEvent [tx]
                saveWalletHash $ fromIntegral txhash
            else do
                $(logInfo) $ T.pack $ unwords 
                    [ "Got solo tx"
                    , encodeTxHashLE txhash 
                    , "We are not synced. Buffering it."
                    ]
                lift $ S.modify $ \s -> s{ soloTxs = nub $ tx : soloTxs s } 

    -- Remove the inflight transaction from all remote inflight lists
    txMap <- lift $ S.gets peerInflightTxs
    let newMap = M.map (filter ((/= txhash) . fst)) txMap
    lift $ S.modify $ \s -> s{ peerInflightTxs = newMap }

    -- Trigger merkle block downloads
    importMerkleBlocks 
  where
    txhash = txHash tx

spvPeerMerkleBlock :: SPVNode s m 
                   => RemoteHost -> DecodedMerkleBlock -> SPVHandle m ()
spvPeerMerkleBlock remote dmb = do
    existsNode <- runHeaderChain $ existsBlockHeaderNode bid
    -- Ignore unsolicited merkle blocks
    when existsNode $ do
        node <- runHeaderChain $ getBlockHeaderNode bid
        
        -- Remove merkle blocks from the inflight list
        merkleMap <- lift $ S.gets peerInflightMerkles
        let f xs   = g $ filter ((/= bid) . snd . fst) xs
            g res  = if null res then Nothing else Just res
            newMap = M.update f remote merkleMap
        lift $ S.modify $ \s -> s{ peerInflightMerkles = newMap }

        let isValid = decodedRoot dmb == (merkleRoot $ nodeHeader node)
        unless isValid $ $(logWarn) $ T.pack $ unwords
            [ "Received invalid merkle block: "
            , encodeBlockHashLE bid
            , "(", show remote, ")" 
            ]

        -- When a rescan is pending, don't store the merkle blocks
        rescan <- lift $ S.gets pendingRescan
        let hasMoreInflight = M.member remote newMap
        when (isNothing rescan && isValid) $ do

            -- Insert the merkle block into the received list
            receivedMap <- lift $ S.gets receivedMerkle
            let singletonMap = M.singleton (nodeHeaderHeight node) [dmb]
                newMap = M.unionWith (++) receivedMap singletonMap
            lift $ S.modify $ \s -> s{ receivedMerkle = newMap }
            
            -- When the peer is done with the current batch, import the
            -- merkle blocks and trigger a new download
            unless hasMoreInflight $ do
                importMerkleBlocks 
                downloadBlocks remote

        -- Try to launch the rescan if one is pending
        when (isJust rescan && not hasMoreInflight) $ 
            processRescan $ fromJust rescan
  where
    bid = headerHash $ merkleHeader $ decodedMerkle dmb

-- This function will make sure that the merkle blocks are imported in-order
-- as they may be received out-of-order from the network (concurrent download)
importMerkleBlocks :: SPVNode s m => SPVHandle m ()
importMerkleBlocks = do
    -- Find all inflight transactions
    inflightTxs <- liftM (concat . M.elems) $ lift $ S.gets peerInflightTxs
    -- If we are pending a rescan, do not import anything
    rescan  <- lift $ S.gets pendingRescan
    -- We stall merkle block imports when transactions are inflight. This
    -- is to prevent this race condition where tx1 would miss it's
    -- confirmation:
    -- INV tx1 -> GetData tx1 -> MerkleBlock (all tx except tx1) -> Tx1
    when (null inflightTxs && isNothing rescan) $ do
        eChan     <- lift $ S.gets spvEventChan
        toImport  <- liftM (concat . M.elems) $ lift $ S.gets receivedMerkle
        importRes <- liftM catMaybes $ forM toImport importMerkleBlock

        -- Send data to the user
        unless (null importRes) $ do

            let merkles  = map (\x -> (fst3 x, snd3 x)) importRes
                txGroups = map lst3 importRes

            liftIO $ atomically $ do
                -- Send transactions to the wallet
                forM_ txGroups $ \gs -> unless (null gs) $
                    writeTBMChan eChan $ TxEvent gs
                -- Send merkle blocks to the wallet
                writeTBMChan eChan $ MerkleBlockEvent merkles

            unless (null txGroups) $ $(logInfo) $ T.pack $ unwords
                [ "Merkle block import: sending"
                , show $ length $ concat txGroups
                , "transactions to the user"
                ]

            bestBlock  <- lift $ S.gets bestBlockHash
            bestHeight <- runHeaderChain $ getBlockHeaderHeight bestBlock

            $(logInfo) $ T.pack $ unwords
                [ "New block height:"
                , show bestHeight
                ]

            synced <- merkleBlocksSynced
            when synced $ do
                -- If we are synced, send solo transactions to the wallet
                solo    <- lift $ S.gets soloTxs
                notHave <- filterM filterTx solo
                liftIO $ atomically $ writeTBMChan eChan $ TxEvent notHave
                forM_ notHave $ saveWalletHash . fromIntegral . txHash
                lift $ S.modify $ \s -> s{ soloTxs = [] }

                -- Log current height
                $(logInfo) $ T.pack $ unwords
                    [ "Merkle blocks are in sync at height:"
                    , show bestHeight
                    ]

            -- Try to import more merkle blocks if some were imported this round
            importMerkleBlocks
  where
    filterTx = ((liftM not) . existsWalletHash . fromIntegral . txHash)

-- Import a single merkle block if its parent has already been imported
importMerkleBlock :: SPVNode s m 
                  => DecodedMerkleBlock
                  -> SPVHandle m (Maybe (BlockChainAction, [TxHash], [Tx]))
importMerkleBlock dmb = do

    -- Check if the previous block is before the fast catchup time
    fc <- lift $ S.gets fastCatchup
    let prevHash = prevBlock $ merkleHeader $ decodedMerkle dmb
    prevNode <- runHeaderChain $ getBlockHeaderNode prevHash
    let parentBeforeCatchup = blockTimestamp (nodeHeader prevNode) < fc
    
    -- Check if we sent the parent block to thew allet
    haveParent <- existsWalletHash $ fromIntegral prevHash

    -- We must have sent the previous merkle to the user (wallet) to import
    -- this one. Or the previous block can be before the fast catchup time.
    if not (haveParent || parentBeforeCatchup) then return Nothing else do

        -- Import the block into the blockchain
        oldBestHash <- lift $ S.gets bestBlockHash
        let newBlockHash = headerHash $ merkleHeader $ decodedMerkle dmb
        action <- runHeaderChain $ connectBlock oldBestHash newBlockHash

        -- Get the blockhash
        let bid = nodeBlockHash $ getActionNode action

        -- Remove the merkle block from the received merkle list
        receivedMap <- lift $ S.gets receivedMerkle
        let height = nodeHeaderHeight $ getActionNode action
            f xs   = g $ delete dmb xs
            g res  = if null res then Nothing else Just res
            newMap = M.update f height receivedMap
        lift $ S.modify $ \s -> s{ receivedMerkle = newMap }

        -- If solo transactions belong to this merkle block, we have
        -- to import them and remove them from the solo list.
        solo <- lift $ S.gets soloTxs
        let isInMerkle x        = txHash x `elem` expectedTxs dmb
            (soloAdd, soloKeep) = partition isInMerkle solo
            allTxs              = nub $ merkleTxs dmb ++ soloAdd
        lift $ S.modify $ \s -> s{ soloTxs = soloKeep }

        haveNode <- existsWalletHash $ fromIntegral bid 
        if haveNode then return Nothing else do

            -- Update the bestBlockHash
            case action of
                SideBlock _ -> return ()
                _           -> lift $ S.modify $ \s -> s{ bestBlockHash = bid }

            -- Filter duplicate transaction
            txToImport <- filterM filterTx allTxs

            -- Save the fact that we sent the data to the wallet
            forM_ txToImport $ saveWalletHash . fromIntegral . txHash
            saveWalletHash $ fromIntegral bid

            case action of
                BestBlock _ -> return ()
                BlockReorg _ o n -> do
                    $(logInfo) $ T.pack $ unwords
                        [ "Block reorg. Orphaned blocks:"
                        , "[", unwords $ 
                            map (encodeBlockHashLE . nodeBlockHash) o ,"]"
                        , "New blocks:"
                        , "[", unwords $ 
                            map (encodeBlockHashLE . nodeBlockHash) n ,"]"
                        , "New height:"
                        , show $ nodeHeaderHeight $ last n
                        ]
                SideBlock b -> $(logInfo) $ T.pack $ unwords
                    [ "Side block at height"
                    , show $ nodeHeaderHeight b, ":"
                    , encodeBlockHashLE $ nodeBlockHash b
                    ]

            return $ Just (action, expectedTxs dmb, txToImport)
  where
    filterTx = ((liftM not) . existsWalletHash . fromIntegral . txHash)

{- Wallet Requests -}

processBloomFilter :: SPVNode s m => BloomFilter -> SPVHandle m ()
processBloomFilter bloom = do
    prevBloom <- lift $ S.gets spvBloom
    -- Load the new bloom filter if it is not empty
    when (prevBloom /= Just bloom && (not $ isBloomEmpty bloom)) $ do
        $(logInfo) "Loading new bloom filter"
        lift $ S.modify $ \s -> s{ spvBloom = Just bloom }

        peers <- getPeers
        forM_ peers $ \(remote, dat) -> do
            -- Set the new bloom filter on all peer connections
            sendMessage remote $ MFilterLoad $ FilterLoad bloom
            -- Trigger merkle block download for all peers. Merkle block
            -- downloads are paused if no bloom filter is loaded.
            downloadBlocks remote

publishTx :: SPVNode s m => Tx -> SPVHandle m ()
publishTx tx = do
    $(logInfo) $ T.pack $ unwords
        [ "Broadcasting transaction to the network:"
        , encodeTxHashLE $ txHash tx
        ]

    peers <- getPeers
    -- TODO: Should we send the transaction through an INV message first?
    forM_ peers $ \(remote, _) -> sendMessage remote $ MTx tx

    -- If no peers are connected, we save the transaction and send it later.
    let txSent = or $ map (peerCompleteHandshake . snd) peers
    unless txSent $ lift $ S.modify $ 
        \s -> s{pendingTxBroadcast = tx : pendingTxBroadcast s}

processRescan :: SPVNode s m => Timestamp -> SPVHandle m ()
processRescan ts = do
    pending <- liftM (concat . M.elems) $ lift $ S.gets peerInflightMerkles
    -- Can't process a rescan while merkle blocks are still inflight
    if (null pending)
        then do
            $(logInfo) $ T.pack $ unwords
                [ "Running rescan from time:"
                , show ts
                ]
            clearWalletHash
            newBestBlock <- runHeaderChain $ blockBeforeTimestamp ts
            toDwn        <- runHeaderChain $ blocksToDownload newBestBlock
            let toDwnList = map (\(a,b) -> (a,[b])) toDwn

            -- Don't remember old requests
            lift $ S.modify $ \s -> 
                s{ blocksToDwn    = M.fromListWith (++) toDwnList
                 , pendingRescan  = Nothing
                 , receivedMerkle = M.empty
                 , fastCatchup    = ts
                 , bestBlockHash  = newBestBlock
                 }
            -- Trigger downloads
            remotePeers <- getPeerKeys
            forM_ remotePeers downloadBlocks
        else do
            $(logInfo) $ T.pack $ unwords
                [ "Rescan: waiting for pending merkle blocks to download" ]
            lift $ S.modify $ \s -> s{ pendingRescan = Just ts }

heartbeatMonitor :: SPVNode s m => SPVHandle m ()
heartbeatMonitor = do
    $(logDebug) "Monitoring heartbeat"

    remotePeers <- getPeerKeys
    now <- round <$> liftIO getPOSIXTime
    let isStalled t = t + 120 < now -- Stalled for over 2 minutes

    -- Check stalled merkle blocks
    merkleMap <- lift $ S.gets peerInflightMerkles
    -- M.Map RemoteHost ([(BlockHash, Timestamp)],[BlockHash, Timestamp])
    let stalledMerkleMap = M.map (partition (isStalled . snd)) merkleMap
        stalledMerkles   = map fst $ concat $ 
                             M.elems $ M.map fst stalledMerkleMap
        badMerklePeers   = M.keys $ M.filter (not . null) $ 
                             M.map fst stalledMerkleMap

    unless (null stalledMerkles) $ do
        $(logWarn) $ T.pack $ unwords
            [ "Resubmitting stalled merkle blocks:"
            , "["
            , unwords $ map (encodeBlockHashLE . snd) stalledMerkles
            , "]"
            ]
        -- Save the new inflight merkle map
        lift $ S.modify $ \s -> 
            s{ peerInflightMerkles = M.map snd stalledMerkleMap }
        -- Add stalled merkle blocks to the downloade queue
        addBlocksToDwn stalledMerkles
        -- Reissue merkle block downloads with bad peers at the end
        let reorderedPeers = (remotePeers \\ badMerklePeers) ++ badMerklePeers
        forM_ reorderedPeers downloadBlocks

    -- Check stalled transactions
    txMap <- lift $ S.gets peerInflightTxs
    -- M.Map RemoteHost ([(TxHash, Timestamp)], [(TxHash, Timestamp)])
    let stalledTxMap = M.map (partition (isStalled . snd)) txMap
        stalledTxs   = M.filter (not . null) $ M.map fst stalledTxMap

    -- Resubmit transaction download for each peer individually
    forM_ (M.toList stalledTxs) $ \(remote, xs) -> do
        let txsToDwn = map fst xs
        $(logWarn) $ T.pack $ unwords
            [ "Resubmitting stalled transactions:"
            , "["
            , unwords $ map encodeTxHashLE txsToDwn
            , "]"
            ]
        downloadTxs remote txsToDwn

-- Add transaction hashes to the inflight map and send a GetData message
downloadTxs :: SPVNode s m => RemoteHost -> [TxHash] -> SPVHandle m ()
downloadTxs remote hs 
    | null hs = return ()
    | otherwise = do
        -- Get current time
        now <- round <$> liftIO getPOSIXTime
        inflightMap <- lift $ S.gets peerInflightTxs
        -- Remove existing inflight values for the peer
        let f = not . (`elem` hs) . fst
            filteredMap = M.adjust (filter f) remote inflightMap
        -- Add transactions to the inflight map
            newMap = M.singleton remote $ map (\h -> (h,now)) hs
            newInflightMap = M.unionWith (++) filteredMap newMap
        lift $ S.modify $ \s -> s{ peerInflightTxs = newInflightMap }
        -- Send GetData message for those transactions
        let vs = map (InvVector InvTx . fromIntegral) hs
        sendMessage remote $ MGetData $ GetData vs

{- Utilities -}

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

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: SPVNode s m => RemoteHost -> SPVHandle m ()
downloadBlocks remote = canDownloadBlocks remote >>= \dwn -> when dwn $ do
    height <- liftM peerHeight $ getPeerData remote
    batch  <- lift $ S.gets blockBatch
    dwnMap <- lift $ S.gets blocksToDwn

    -- Find blocks that this peer can download
    let xs = concat $ map (\(k,vs) -> map (\v -> (k,v)) vs) $ M.toAscList dwnMap
        (ys, rest) = splitAt batch xs
        (toDwn, highRest) = span ((<= height) . fst) ys
        restToList = map (\(a,b) -> (a,[b])) $ rest ++ highRest
        restMap = M.fromListWith (++) restToList

    unless (null toDwn) $ do
        $(logInfo) $ T.pack $ unwords 
            [ "Requesting more merkle block(s)"
            , "["
            , if length toDwn == 1
                then encodeBlockHashLE $ snd $ head toDwn
                else unwords [show $ length toDwn, "block(s)"]
            , "]"
            , "(", show remote, ")" 
            ]

        -- Store the new list of blocks to download
        lift $ S.modify $ \s -> s{ blocksToDwn = restMap }

        -- Store the new blocks to download as inflght merkle blocks
        now <- round <$> liftIO getPOSIXTime
        merkleMap <- lift $ S.gets peerInflightMerkles
        let newList = map (\v -> (v, now)) toDwn
            newMap  = M.unionWith (++) merkleMap $ M.singleton remote newList
        lift $ S.modify $ \s -> s{ peerInflightMerkles = newMap }

        -- Send GetData message to receive the merkle blocks
        sendMerkleGetData remote $ map snd toDwn

-- Only download blocks from peers that have completed the handshake
-- and are idling. Do not allow downloads if a rescan is pending or
-- if no bloom filter was provided yet from the wallet.
canDownloadBlocks :: SPVNode s m => RemoteHost -> SPVHandle m Bool
canDownloadBlocks remote = do
    peerData   <- getPeerData remote
    reqM       <- liftM (M.lookup remote) $ lift $ S.gets peerInflightMerkles
    bloom      <- lift $ S.gets spvBloom
    syncPeer   <- lift $ S.gets spvSyncPeer
    rescan     <- lift $ S.gets pendingRescan
    return $ (syncPeer /= Just remote)
          && (isJust bloom)
          && (peerCompleteHandshake peerData)
          && (isNothing reqM || reqM == Just [])
          && (isNothing rescan)

sendMerkleGetData :: SPVNode s m => RemoteHost -> [BlockHash] -> SPVHandle m ()
sendMerkleGetData remote hs = do
    sendMessage remote $ MGetData $ GetData $ 
        map ((InvVector InvMerkleBlock) . fromIntegral) hs
    -- Send a ping to have a recognizable end message for the last
    -- merkle block download
    -- TODO: Compute a random nonce for the ping
    sendMessage remote $ MPing $ Ping 0

-- Header height = network height
blockHeadersSynced :: SPVNode s m => SPVHandle m Bool
blockHeadersSynced = do
    networkHeight <- getBestPeerHeight
    headerHeight <- runHeaderChain bestBlockHeaderHeight
    return $ headerHeight >= networkHeight

-- Merkle block height = network height
merkleBlocksSynced :: SPVNode s m => SPVHandle m Bool
merkleBlocksSynced = do
    networkHeight <- getBestPeerHeight
    bestBlock <- lift $ S.gets bestBlockHash
    merkleHeight <- runHeaderChain $ getBlockHeaderHeight bestBlock
    return $ merkleHeight >= networkHeight

-- Log a message if we are synced up with this peer
logPeerSynced :: SPVNode s m => RemoteHost -> Version -> SPVHandle m ()
logPeerSynced remote ver = do
    bestBlock <- lift $ S.gets bestBlockHash
    bestHeight <- runHeaderChain $ getBlockHeaderHeight bestBlock
    when (bestHeight >= startHeight ver) $
        $(logInfo) $ T.pack $ unwords
            [ "Merkle blocks are in sync with the peer. Peer height:"
            , show $ startHeight ver 
            , "Our height:"
            , show bestHeight
            , "(", show remote, ")" 
            ]

addBroadcastBlocks :: SPVNode s m => RemoteHost -> [BlockHash] -> SPVHandle m ()
addBroadcastBlocks remote hs = lift $ do
    prevMap <- S.gets peerBroadcastBlocks
    S.modify $ \s -> s{ peerBroadcastBlocks = M.unionWith (++) prevMap sMap }
  where
    sMap = M.singleton remote hs

addBlocksToDwn :: SPVNode s m 
               => [(BlockHeight, BlockHash)] -> SPVHandle m ()
addBlocksToDwn hs = lift $ do
    dwnMap <- S.gets blocksToDwn
    S.modify $ \s -> s{ blocksToDwn = M.unionWith (++) dwnMap newMap }
  where
    newMap = M.fromListWith (++) $ map (\(a,b) -> (a,[b])) hs

