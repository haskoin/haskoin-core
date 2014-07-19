{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.PeerManager
( withAsyncNode
, NodeEvent(..)
, NodeRequest(..)
) where

import System.Random

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Monad.Logger 
import Control.Monad.Trans
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import qualified Control.Monad.State as S

import Data.Maybe
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import Data.Default
import Data.Foldable (toList)
import Data.List 
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Conduit 
    ( Sink
    , awaitForever
    , ($$) 
    )
import Data.Conduit.Network 
    ( ClientSettings
    , getHost
    , getPort
    , runTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Database.LevelDB.Base as DB

import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Node.Types
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util

type ManagerHandle = S.StateT ManagerSession (LoggingT IO)
type BlockHeight = Word32

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan ManagerRequest
    , eventChan        :: TBMChan NodeEvent
    , peerMap          :: M.Map RemoteHost PeerData
    , syncPeer         :: Maybe RemoteHost
    , dbHandle         :: DB.DB
    , dbTxHandle       :: DB.DB
    , mngrBloom        :: Maybe BloomFilter
    -- We received the merkle blocks but buffer them to send them in-order to
    -- the wallet
    , receivedBlocks   :: M.Map BlockHeight DecodedMerkleBlock
    -- Blocks that were inflight when a peer disconnected
    , missingBlocks    :: [BlockHash]
    -- Stall merkle block while a GetData for a transaction is pending
    , inflightTxs      :: [TxHash]
    -- Stall solo transactions while the blockchain is syncing
    , soloTxs          :: [Tx]
    -- Transactions from users that need to be broadcasted
    , broadcastBuffer  :: [Tx]
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerSettings         :: RemoteHost
    , peerHandshake        :: Bool
    , peerHeight           :: Word32
    -- Blocks that a peer sent us but we haven't linked them yet to our chain.
    -- We use this list to update the peer height when we link those blocks.
    , peerBlocks           :: [BlockHash] 
    , peerMsgChan          :: TBMChan Message
    -- Inflight merkle block requests for this peer.
    -- TODO: What if a remote peer doesn't respond? Perhaps track timestamp
    -- and re-submit?
    , peerInflightMerkle   :: [BlockHash]
    , peerReconnectTimer   :: Int
    }

withAsyncNode :: FilePath 
              -> (TBMChan NodeEvent -> TBMChan NodeRequest -> Async () -> IO ())
              -> IO ()
withAsyncNode fp f = do
    db <- DB.open (concat [fp, "/headerchain"])
              DB.defaultOptions{ DB.createIfMissing = True
                               , DB.cacheSize       = 2048
                               }
    txdb <- DB.open (concat [fp, "/txdb"])
              DB.defaultOptions{ DB.createIfMissing = True
                               , DB.cacheSize       = 2048
                               }
    mChan <- atomically $ newTBMChan 1024
    eChan <- atomically $ newTBMChan 1024
    rChan <- atomically $ newTBMChan 1024
    vers  <- buildVersion
    let session = ManagerSession { mngrVersion      = vers
                                 , mngrChan         = mChan
                                 , eventChan        = eChan
                                 , peerMap          = M.empty
                                 , syncPeer         = Nothing
                                 , dbHandle         = db
                                 , dbTxHandle       = txdb
                                 , mngrBloom        = Nothing
                                 , receivedBlocks   = M.empty
                                 , missingBlocks    = []
                                 , inflightTxs      = []
                                 , soloTxs          = []
                                 , broadcastBuffer  = []
                                 }

    let runNode = runStdoutLoggingT $ flip S.evalStateT session $ do 

        -- Initialize the database
        runDB $ do 
            initDB 
            bestM <- getBestBlock
            -- Reset the download pointer
            when (isJust bestM) $
                putLastDownloadNode $ nodeBlockHash $ fromJust bestM

        -- Process messages
        -- TODO: Close database handle on exception with DB.close
        sourceTBMChan mChan $$ managerSink

    -- Launch node
    withAsync runNode $ \a -> 
        -- Launch thread listening to user requests
        withAsync (sourceTBMChan rChan $$ processUserRequest mChan) $ \_ -> 
            f eChan rChan a

processUserRequest :: TBMChan ManagerRequest -> Sink NodeRequest IO ()
processUserRequest mChan = awaitForever $ \r -> 
    lift $ atomically $ writeTBMChan mChan $ UserRequest r

buildVersion :: IO Version
buildVersion = do
    -- TODO: Get our correct IP here
    let zeroAddr = (0x00, 0xffff00000000)
        add      = NetworkAddress 1 zeroAddr 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: Sink ManagerRequest ManagerHandle ()
managerSink = awaitForever $ \req -> lift $ do
    -- Discard messages from peers that are gone
    -- TODO: Is this always the correct behavior ?
    case req of
        StartPeer c -> processStartPeer c
        PeerHandshake remote v -> do
            exists <- peerExists remote
            when exists $ processPeerHandshake remote v
        PeerDisconnect remote -> do 
            exists <- peerExists remote
            when exists $ processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> do
            exists <- peerExists remote
            when exists $ processMerkleBlock remote dmb
        PeerMessage remote msg -> do
            exists <- peerExists remote
            when exists $ case msg of
                MHeaders headers -> processHeaders remote headers
                MInv inv         -> processInv remote inv
                MTx tx           -> processTx remote tx
                _                -> return () -- Ignore them for now
        UserRequest r -> case r of
            ConnectNode h p     -> processStartPeer $ RemoteHost h p
            BloomFilterUpdate b -> processBloomFilter b
            PublishTx tx        -> processPublishTx tx
            FastCatchupTime t   -> processFastCatchupTime t
            _                   -> return () -- Ignore them for now

processStartPeer :: RemoteHost -> ManagerHandle ()
processStartPeer remote = do
    $(logInfo) $ T.pack $ unwords
        [ "Starting peer"
        , "(", show remote, ")" 
        ] 
    vers  <- S.gets mngrVersion
    mChan <- S.gets mngrChan
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 1024
        writeTBMChan c $ MVersion vers
        return c

    -- Update/Insert peer data
    exists <- peerExists remote
    if exists
        -- TODO: Should we reuse the existing pChan or create a new one?
        then modifyPeerData remote $ \d -> d{ peerMsgChan = pChan }
        else do
            let peerData = PeerData { peerHandshake      = False
                                    , peerMsgChan        = pChan
                                    , peerHeight         = 0
                                    , peerBlocks         = []
                                    , peerInflightMerkle = []
                                    , peerReconnectTimer = 1
                                    }
            S.modify $ \s -> s{ peerMap = M.insert remote peerData (peerMap s) }

    -- Start peer thread
    let cs = clientSettings (remotePort remote) (stringToBS $ remoteHost remote) 
    void $ liftIO $ forkFinally 
        (runTCPClient cs $ peer pChan mChan remote) $ \_ -> 
            atomically $ writeTBMChan mChan $ PeerDisconnect remote

-- TODO: Do something about the inflight merkle blocks of this peer
processPeerDisconnect :: RemoteHost -> ManagerHandle ()
processPeerDisconnect remote = do
    $(logInfo) $ T.pack $ unwords
        [ "Peer disconnected"
        , "(", show remote, ")" 
        ]
    dat <- getPeerData remote
    -- TODO: Check if the peer had pending work in pChan
    liftIO $ atomically $ closeTBMChan $ peerMsgChan dat

    -- Store inflight merkle blocks of this peer
    S.modify $ \s -> 
        s{ missingBlocks = missingBlocks s ++ peerInflightMerkle dat }

    -- TODO: Do something about peerInflightMerkle
    let reconnect = peerReconnectTimer dat
    modifyPeerData remote $ \d -> d{ peerHandshake      = False 
                                   , peerInflightMerkle = []
                                   , peerBlocks         = []
                                   , peerHeight         = 0
                                   , peerReconnectTimer = reconnect*2
                                   }
    -- Handle syncPeer
    syn <- S.gets syncPeer
    when (syn == Just remote) $ do
        S.modify $ \s -> s{ syncPeer = Nothing }
        m <- S.gets peerMap
        -- Choose only peers that have finished the connection handshake
        let vals = filter (\(_,d) -> peerHandshake d) $ M.assocs m
        -- Send a GetHeaders message to all peers. The fastest will become
        -- the new syncPeer
        forM_ vals $ \(t,_) -> sendGetHeaders t True 0x00

    -- Handle reconnection
    mChan <- S.gets mngrChan
    void $ liftIO $ forkIO $ do
        -- TODO: Put this value in a config file
        let maxDelay = 1000000 * 900 -- 15 minutes
        -- reconnect is in microseconds
        threadDelay $ min maxDelay (1000000 * reconnect) 
        atomically $ writeTBMChan mChan $ StartPeer remote

processPeerHandshake :: RemoteHost -> Version -> ManagerHandle ()
processPeerHandshake remote remoteVer = do
    $(logDebug) $ T.pack $ unwords
        [ "Peer connected"
        , "(", show remote, ")" 
        ]
    modifyPeerData remote $ \d -> 
        d{ peerHandshake      = True 
         , peerHeight         = startHeight remoteVer
         , peerReconnectTimer = 1
         }
    -- Set the bloom filter for this connection
    bloom <- S.gets mngrBloom
    when (isJust bloom) $
        sendMessage remote $ MFilterLoad $ FilterLoad $ fromJust bloom

    -- Send pending transactions to broadcast
    -- TODO: Is it enough just to broadcast to 1 peer ?
    pendingTxs <- S.gets broadcastBuffer
    forM_ pendingTxs $ \tx -> sendMessage remote $ MTx tx
    S.modify $ \s -> s{ broadcastBuffer = [] }

    -- Send a GetHeaders regardless if there is already a peerSync. This peer
    -- could still be faster and become the new peerSync.
    sendGetHeaders remote True 0x00

    -- Download more block if some are pending
    downloadBlocks remote

    bestM <- runDB bestBlockHeight
    when (isJust bestM && (fromJust bestM) >= startHeight remoteVer) $
        $(logInfo) $ T.pack $ unwords
            [ "Node is synced with peer. Peer height:"
            , show $ startHeight remoteVer 
            , "Our height:"
            , show $ fromJust bestM
            , "(", show remote, ")" 
            ]

processBloomFilter :: BloomFilter -> ManagerHandle ()
processBloomFilter bloom = do
    prevBloom <- S.gets mngrBloom
    -- Don't load an empty bloom filter
    when (prevBloom /= Just bloom && (not $ isBloomEmpty bloom)) $ do
        $(logDebug) "Loading new bloom filter"
        S.modify $ \s -> s{ mngrBloom = Just bloom }
        m <- S.gets peerMap 
        forM_ (M.keys m) $ \remote -> do
            dat <- getPeerData remote
            when (peerHandshake dat) $ 
                sendMessage remote $ MFilterLoad $ FilterLoad bloom
            downloadBlocks remote

processPublishTx :: Tx -> ManagerHandle ()
processPublishTx tx = do
    $(logDebug) $ T.pack $ unwords
        [ "Broadcasting transaction to the network:"
        , encodeTxHashLE $ txHash tx
        ]
    m <- S.gets peerMap 
    flags <- forM (M.keys m) $ \remote -> do
        dat <- getPeerData remote
        if (peerHandshake dat) 
            then do
                sendMessage remote $ MTx tx
                return True
            else return False

    -- If no peers are connected, we buffer the transaction and try to send
    -- it later.
    unless (or flags) $ 
        S.modify $ \s -> s{ broadcastBuffer = tx : broadcastBuffer s }

processFastCatchupTime :: Word32 -> ManagerHandle ()
processFastCatchupTime t = do
    hasFastCatchup <- runDB getFastCatchup
    when (isNothing hasFastCatchup) $ do
        $(logDebug) $ T.pack $ unwords
            [ "Setting fast catchup time:"
            , show t
            ]
        runDB $ setFastCatchup t
        -- Trigger download if the node is idle
        remotes <- M.keys <$> S.gets peerMap 
        forM_ remotes downloadBlocks

processHeaders :: RemoteHost -> Headers -> ManagerHandle ()
processHeaders remote (Headers h) = do
    -- TODO: The time here is incorrect. It should be a median of all peers.
    adjustedTime <- liftIO getPOSIXTime
    -- TODO: If a block headers can't be added, update DOS status
    (newBest, newToDwnM) <- runDB $ do
        before <- bestHeaderHeight
        -- TODO: Handle errors in addBlockHeader. We don't do anything
        -- in case of RejectHeader
        newToDwn <- forM (map fst h) $ \x -> do
            res <- addBlockHeader x $ round adjustedTime
            case res of
                -- Return the data that will be inserted at the end of
                -- the download queue
                (AcceptHeader n) -> 
                    return $ Just (nodeHeaderHeight n, nodeBlockHash n)
                _ -> return Nothing
        after  <- bestHeaderHeight
        return $ (after > before, newToDwn)

    let newToDwn  = catMaybes newToDwnM
    remotes <- M.keys <$> S.gets peerMap 

    -- Adjust the height of peers that sent us INV messages for these headers
    forM newToDwn $ \(h, bid) -> do
        forM remotes $ \r -> do
            dat <- getPeerData r
            let (xs, ys) = partition (== bid) $ peerBlocks dat
            unless (null xs) $ do
                putPeerData r dat{ peerBlocks = ys
                                 , peerHeight = max (peerHeight dat) h
                                 }
                when (h > peerHeight dat) $
                    $(logDebug) $ T.pack $ unwords
                        [ "Adjusting peer height to"
                        , show h
                        , "(", show r, ")"
                        ]

    -- Continue syncing from this node only if it made some progress.
    -- Otherwise, another peer is probably faster/ahead already.
    when newBest $ do
        -- Adjust height of this thread
        let m = fst $ last $ newToDwn
        dat <- getPeerData remote
        when (m > peerHeight dat) $ do
            modifyPeerData remote $ \d -> d{ peerHeight = m }
            $(logDebug) $ T.pack $ unwords
                [ "Adjusting peer height to"
                , show m
                , "(", show remote, ")" 
                ]

        -- Update the sync peer 
        dwnSynced <- downloadSynced
        S.modify $ \s -> 
            s{ syncPeer = if dwnSynced then Nothing else Just remote }
        $(logInfo) $ T.pack $ unwords
            [ "New best header height:"
            , show $ fst $ last newToDwn
            ]

        -- Requesting more headers
        sendGetHeaders remote False 0x00

    -- Request block downloads for all peers that are currently idling
    forM_ remotes downloadBlocks

processMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> ManagerHandle ()
processMerkleBlock remote dmb = do
    -- TODO: We read the node both here and in addMerkleBlock. It's duplication
    -- of work. Can we avoid it?
    nodeM <- runDB $ getBlockHeaderNode bid
    let node = fromJust nodeM

    -- Ignore unsolicited merkle blocks
    when (isJust nodeM) $ do
        -- TODO: Handle this error better
        when (decodedRoot dmb /= (merkleRoot $ nodeHeader node)) $
            error "Invalid partial merkle tree received"

        blockMap <- S.gets receivedBlocks
        let height = nodeHeaderHeight node
        -- Add this merkle block to the received list
        S.modify $ \s -> s{ receivedBlocks = M.insert height dmb blockMap }

        -- Mark transactions inside the merkle block as received
        forM (merkleTxs dmb) $ putDbTx . txHash

        -- Import the merkle block in-order into the user app
        importMerkleBlocks

        -- Decrement the peer request counter
        requests <- peerInflightMerkle <$> getPeerData remote
        let newRequests = delete (nodeBlockHash node) requests
        modifyPeerData remote $ \d -> d{ peerInflightMerkle = newRequests }

        -- If this peer is done, get more merkle blocks to download
        when (null newRequests) $ downloadBlocks remote
  where
    bid = headerHash $ merkleHeader $ decodedMerkle dmb

-- This function will make sure that the merkle blocks are imported in-order
-- as they may be received out-of-order from the network (concurrent download)
importMerkleBlocks :: ManagerHandle ()
importMerkleBlocks = do
    heightM     <- runDB bestBlockHeight
    blockMap    <- S.gets receivedBlocks
    dwnTxs      <- S.gets inflightTxs
    dwnFinished <- runDB isDownloadFinished
    let ascList   = M.toAscList blockMap
        toImport  = go (fromJust heightM) ascList
        toKeep    = drop (length toImport) ascList
        -- We stall merkle block imports when transactions are inflight. This
        -- is to prevent this race condition where tx1 would miss it's
        -- confirmation:
        -- INV tx1 -> GetData tx1 -> MerkleBlock (all tx except tx1) -> Tx1
        canImport = isJust heightM && null dwnTxs && (not $ null toImport)

    when canImport $ do
        S.modify $ \s -> s{ receivedBlocks = M.fromList toKeep }
        eChan <- S.gets eventChan

        res <- forM toImport $ \(h, dmb) -> do
            -- Import in blockchain
            node <- runDB $ addMerkleBlock $ decodedMerkle dmb
            -- If solo transactions belong to this merkle block, we have
            -- to import them and remove them from the solo list.
            solo  <- S.gets soloTxs
            let f x                 = txHash x `elem` expectedTxs dmb
                (soloAdd, soloKeep) = partition f solo
                txImport            = nub $ merkleTxs dmb ++ soloAdd
            S.modify $ \s -> s{ soloTxs = soloKeep }

            liftIO $ atomically $ do
            -- Send transactions to the wallet
                forM_ txImport (writeTBMChan eChan . TxEvent)
            -- Send merkle blocks to the wallet 
                writeTBMChan eChan $ MerkleBlockEvent node (expectedTxs dmb)

            case node of
                BestBlock b      -> return $ Just b
                BlockReorg _ _ n -> return $ Just $ last n
                _                -> return Nothing

        let bestM = foldl (<|>) Nothing $ reverse res

        synced <- nodeSynced
        if synced 
            then do
                -- If we are synced, send solo transactions to the wallet
                solo <- S.gets soloTxs
                S.modify $ \s -> s{ soloTxs = [] }
                liftIO $ atomically $ forM_ solo (writeTBMChan eChan . TxEvent)
                when (isJust bestM) $ $(logDebug) $ T.pack $ unwords
                    [ "We are in sync with the network: block height"
                    , show $ nodeHeaderHeight $ fromJust $ bestM
                    ]
            else do
                let h = nodeHeaderHeight $ fromJust bestM
                -- Only display a new height every 100 blocks
                -- TODO: modulo 100 doesn't work if we import more than 1 
                -- block.
                when (isJust bestM && h `mod` 100 == 0) $ 
                    $(logDebug) $ T.pack $ unwords
                        [ "Best block height:", show h ]

  where
    go prevHeight ((currHeight, x):xs) 
        | currHeight == prevHeight + 1 = (currHeight, x) : go currHeight xs
        | otherwise = []
    go _ [] = []

processInv :: RemoteHost -> Inv -> ManagerHandle ()
processInv remote (Inv vs) = do

    -- Request transactions that we have not seen yet
    notHaveTxs <- filterM ((not <$>) . existsDbTx) txlist
    -- Nub because we may have duplicates (same Tx from many peers)
    S.modify $ \s -> s{ inflightTxs = nub $ inflightTxs s ++ notHaveTxs }
    let getData = GetData $ map (InvVector InvTx . fromIntegral) notHaveTxs
    sendMessage remote $ MGetData getData

    let f (a, b) h = getBlockHeaderNode h >>= \resM -> return $ case resM of
            Just r -> (r:a, b)
            _      -> (a, h:b)

    -- Partition blocks that we know and don't know
    (have, notHave) <- runDB $ foldM f ([],[]) blocklist

    -- Update the peer height and unknown block list
    let m = foldl max 0 $ map nodeHeaderHeight have
    dat <- getPeerData remote
    putPeerData remote dat{ peerBlocks = peerBlocks dat ++ notHave 
                          , peerHeight = max m (peerHeight dat)
                          } 

    when (m > peerHeight dat) $ do
        $(logDebug) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show m
            , "(", show remote, ")" 
            ]

    -- Request headers for blocks we don't have
    forM_ notHave $ \b -> sendGetHeaders remote True b

  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs

-- These are solo transactions not linked to a merkle block (yet)
processTx :: RemoteHost -> Tx -> ManagerHandle ()
processTx remote tx = do
    -- Only send the transaction to the wallet if we have not seen it yet, to
    -- avoid sending duplicates. This can happen when multiple peers get an INV
    -- for this transaction at the same time.
    seenTx <- existsDbTx txhash
    when (not seenTx) $ do
        putDbTx txhash

        -- Only send to wallet if we are in sync
        synced <- nodeSynced
        if synced 
            then do
                eChan <- S.gets eventChan
                liftIO $ atomically $ writeTBMChan eChan $ TxEvent tx
            else S.modify $ \s -> s{ soloTxs = tx : soloTxs s } 

    -- Remove the inflight transaction from the inflight list
    S.modify $ \s -> s{ inflightTxs = delete txhash $ inflightTxs s }
        
    -- If no more transactions are inflight, trigger the download of
    -- the merkle blocks again
    dwnTxs <- S.gets inflightTxs
    when (null dwnTxs) $ importMerkleBlocks 
  where
    txhash = txHash tx

-- Block height = network height
nodeSynced :: ManagerHandle Bool
nodeSynced = do
    pDats <- M.elems <$> S.gets peerMap 
    let netHeight = foldl max 0 $ map peerHeight pDats
    ourHeight <- runDB bestBlockHeight
    return $ ourHeight == Just netHeight

-- Header height = network height
downloadSynced :: ManagerHandle Bool
downloadSynced = do
    pDats <- M.elems <$> S.gets peerMap 
    let netHeight = foldl max 0 $ map peerHeight pDats
    ourHeight <- runDB bestHeaderHeight
    return $ ourHeight == netHeight

-- Send out a GetHeaders request for a given peer
sendGetHeaders :: RemoteHost -> Bool -> BlockHash -> ManagerHandle ()
sendGetHeaders remote full hstop = do
    loc <- runDB $ if full then blockLocator else do
        h <- getBestHeader
        return [nodeBlockHash h]
    sendMessage remote $ MGetHeaders $ GetHeaders 0x01 loc hstop
    $(logDebug) $ T.pack $ unwords 
        [ "Requesting more block headers"
        , "(", show remote, ")" 
        ]

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: RemoteHost -> ManagerHandle ()
downloadBlocks remote = do
    -- Request block downloads if some are pending
    peerData   <- getPeerData remote
    bloom      <- S.gets mngrBloom
    sync       <- S.gets syncPeer
    hasCatchup <- runDB getFastCatchup
    let remoteHeight = peerHeight peerData
        requests     = peerInflightMerkle peerData
        canDownload  =  (sync /= Just remote)
                     && (isJust bloom)
                     && (isJust hasCatchup)
                     && (peerHandshake peerData)
                     && (null requests)
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    when canDownload $ do
        -- Get blocks missing from other peers that have disconnected
        (missing, rest) <- splitAt 500 <$> S.gets missingBlocks
        S.modify $ \s -> s{ missingBlocks = rest }
        -- Get more blocks to download from the database
        xs <- runDB $ nextDownloadRange (500 - length missing) remoteHeight
        let toDwn = missing ++ xs
        unless (null toDwn) $ do
            $(logDebug) $ T.pack $ unwords 
                [ "Requesting more merkle blocks:"
                , "(", show remote, ")" 
                ]
            -- Store the work count for this peer
            modifyPeerData remote $ \d -> 
                d{ peerInflightMerkle = toDwn }
            sendMessage remote $ MGetData $ GetData $ 
                map ((InvVector InvMerkleBlock) . fromIntegral) toDwn
            -- Send a ping to have a recognizable end message for the last
            -- merkle block download
            -- TODO: Compute a random nonce for the ping
            sendMessage remote $ MPing $ Ping 0

getPeerData :: RemoteHost -> ManagerHandle PeerData
getPeerData remote = do
    m <- S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup remote m

putPeerData :: RemoteHost -> PeerData -> ManagerHandle ()
putPeerData remote d = 
    S.modify $ \s -> s{ peerMap = M.insert remote d (peerMap s) }

modifyPeerData :: RemoteHost -> (PeerData -> PeerData) -> ManagerHandle ()
modifyPeerData remote f = do
    d <- getPeerData remote
    putPeerData remote $ f d

deletePeerData :: RemoteHost -> ManagerHandle ()
deletePeerData remote = 
    S.modify $ \s -> s{ peerMap = M.delete remote $ peerMap s }

peerExists :: RemoteHost -> ManagerHandle Bool
peerExists remote = do
    m <- S.gets peerMap
    return $ M.member remote m

sendMessage :: RemoteHost -> Message -> ManagerHandle ()
sendMessage remote msg = do
    d <- getPeerData remote
    -- The message is discarded if the channel is closed.
    liftIO . atomically $ writeTBMChan (peerMsgChan d) msg

runDB :: DBHandle a -> ManagerHandle a
runDB m = do
    db <- S.gets dbHandle
    liftIO $ S.evalStateT m $ LevelSession db

existsDbTx :: TxHash -> ManagerHandle Bool
existsDbTx txhash = do
    db <- S.gets dbTxHandle
    isJust <$> (liftIO $ DB.get db def $ encode' txhash)

putDbTx :: TxHash -> ManagerHandle ()
putDbTx txhash = do
    db <- S.gets dbTxHandle
    liftIO $ DB.put db def (encode' $ txhash) BS.empty

