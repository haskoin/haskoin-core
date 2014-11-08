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
import Data.List (nub, partition, delete, maximumBy, dropWhileEnd)
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
    , toList
    , fromList
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
    , blocksToDwn :: [BlockHash]
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
    , peerInflightMerkles :: M.Map RemoteHost [(BlockHash, Timestamp)]
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
            , blocksToDwn = []
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
    toDwn <- runHeaderChain $ blocksToDownload bestHash fc
    lift $ S.modify $ \s -> s{ blocksToDwn = toDwn }

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
                map encodeBlockHashLE toDwn
            , "]"
            ]
        -- Add the block hashes to the start of the download queue
        lift $ S.modify $ \s -> s{ blocksToDwn = toDwn ++ blocksToDwn s }
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

spvPeerMerkleBlock :: SPVNode s m 
                   => RemoteHost -> DecodedMerkleBlock -> SPVHandle m ()
spvPeerMerkleBlock remote dmb = return ()

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
        toDwn          = map nodeBlockHash $ drop (length fcBlocks) newBlocks
    unless (null fcBlocks) $ do
        bestBlock <- lift $ S.gets bestBlockHash
        bestNode  <- runHeaderChain $ getBlockHeaderNode bestBlock
        -- If there are nodes >= fastCatchup in between nodes < fastCatchup,
        -- those nodes will be downloaded and simply reported as SideBlocks.
        when (nodeChainWork fcBlock > nodeChainWork bestNode) $
            lift $ S.modify $ \s -> s{ bestBlockHash = nodeBlockHash fcBlock }

    -- Add block hashes to download at the end of the queue
    lift $ S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ toDwn }

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
processInv remote (Inv vs) = return ()

processTx :: SPVNode s m => RemoteHost -> Tx -> SPVHandle m ()
processTx remote tx = return ()

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
publishTx tx = return ()

processRescan :: SPVNode s m => Timestamp -> SPVHandle m ()
processRescan ts = return ()

heartbeatMonitor :: SPVNode s m => SPVHandle m ()
heartbeatMonitor = return ()

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

downloadBlocks :: SPVNode s m => RemoteHost -> SPVHandle m ()
downloadBlocks remote = return ()

-- Header height = network height
blockHeadersSynced :: SPVNode s m => SPVHandle m Bool
blockHeadersSynced = do
    networkHeight <- getBestPeerHeight
    ourHeight <- runHeaderChain bestBlockHeaderHeight
    return $ ourHeight >= networkHeight

-- Log a message if we are synced up with this peer
logPeerSynced :: SPVNode s m => RemoteHost -> Version -> SPVHandle m ()
logPeerSynced remote ver = do
    bestBlock <- lift $ S.gets bestBlockHash
    bestHeight <- runHeaderChain $ getBlockHeaderHeight bestBlock
    when (bestHeight >= startHeight ver) $
        $(logInfo) $ T.pack $ unwords
            [ "We are synced with peer. Peer height:"
            , show $ startHeight ver 
            , "Our height:"
            , show bestHeight
            , "(", show remote, ")" 
            ]

