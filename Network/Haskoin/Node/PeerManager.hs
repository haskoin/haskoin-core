{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.PeerManager
( startNode
, NodeEvent(..)
, NodeRequest(..)
) where

import System.Random

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkIO, forkFinally, ThreadId, myThreadId)
import Control.Monad.Logger 
import Control.Monad.Trans
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
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
    , runTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Database.LevelDB.Base as DB

import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util

type ManagerHandle = S.StateT ManagerSession (LoggingT IO)
type BlockHeight = Word32

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan (ThreadId, ManagerRequest)
    , eventChan        :: TBMChan NodeEvent
    , peerMap          :: M.Map ThreadId PeerData
    , syncPeer         :: Maybe ThreadId
    , dbHandle         :: DB.DB
    , dbTxHandle       :: DB.DB
    , mngrBloom        :: Maybe BloomFilter
    -- We received the merkle blocks but buffer them to send them in-order to
    -- the wallet
    , receivedBlocks   :: M.Map BlockHeight DecodedMerkleBlock
    -- Stall merkle block while a GetData for a transaction is pending
    , inflightTxs      :: [TxHash]
    -- Stall solo transactions while the blockchain is syncing
    , soloTxs          :: [Tx]
    -- Transactions from users that need to be broadcasted
    , broadcastBuffer  :: [Tx]
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerHandshake        :: Bool
    , peerHeight           :: Word32
    -- Blocks that a peer sent us but we haven't linked them yet to our chain.
    -- We use this list to update the peer height when we link those blocks.
    , peerBlocks           :: [BlockHash] 
    , peerMsgChan          :: TBMChan Message
    -- Inflight merkle block requests for this peer.
    -- TODO: What if a remote peer doesn't respond? Perhaps track timestamp
    -- and re-submit?
    , peerInflightMerkle   :: [BlockHash]
    }

data NodeEvent 
    = MerkleBlockEvent BlockChainAction [TxHash]
    | TxEvent Tx
    deriving (Eq, Show)

data NodeRequest
    = BloomFilterUpdate BloomFilter 
    | PublishTx Tx
    | FastCatchupTime Word32
    deriving (Eq, Show)

startNode :: FilePath -> IO (TBMChan NodeEvent, TBMChan NodeRequest)
startNode fp = do
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
                                 , inflightTxs      = []
                                 , soloTxs          = []
                                 , broadcastBuffer  = []
                                 }
    -- Launch thread listening to user requests
    -- TODO: Should we catch exception here?
    _ <- forkIO $ sourceTBMChan rChan $$ processUserRequest mChan

    -- TODO: Catch exceptions here?
    _ <- forkIO $ runStdoutLoggingT $ flip S.evalStateT session $ do 

        -- Initialize the database
        runDB initDB 

        -- Spin up some peer threads
        -- TODO: Put the peers in a config file or write peer discovery
        startPeer $ clientSettings 8333 "localhost"
        startPeer $ clientSettings 8333 "haskoin.com"
        startPeer $ clientSettings 8333 "95.215.47.133"

        -- Process messages
        -- TODO: Close database handle on exception with DB.close
        sourceTBMChan mChan $$ managerSink

    return (eChan, rChan)

processUserRequest :: TBMChan (ThreadId, ManagerRequest) 
                   -> Sink NodeRequest IO ()
processUserRequest mChan = awaitForever $ \r -> case r of
    BloomFilterUpdate b -> do
        tid <- lift myThreadId
        lift $ atomically $ writeTBMChan mChan (tid, UserBloomFilter b)
    PublishTx tx -> do
        tid <- lift myThreadId
        lift $ atomically $ writeTBMChan mChan (tid, UserPublishTx tx)
    FastCatchupTime t -> do
        tid <- lift myThreadId
        lift $ atomically $ writeTBMChan mChan (tid, UserFastCatchup t)

startPeer :: ClientSettings -> ManagerHandle ()
startPeer remote = do
    vers  <- S.gets mngrVersion
    mChan <- S.gets mngrChan
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 1024
        writeTBMChan c $ MVersion vers
        return c
    tid   <- liftIO $ forkFinally 
        (runTCPClient remote $ peer pChan mChan) $ \ret -> do
        -- Thread cleanup
        tid <- myThreadId
        -- TODO: If the peer had some inflight blocks, move them to the 
        -- download queue.
        -- TODO: Check if the peer had pending work in pChan
        atomically $ do
            closeTBMChan pChan
            writeTBMChan mChan (tid, PeerDisconnect)

    let peerData = PeerData { peerHandshake        = False
                            , peerMsgChan          = pChan
                            , peerHeight           = 0
                            , peerBlocks           = []
                            , peerInflightMerkle = []
                            }
    S.modify $ \s -> s{ peerMap = M.insert tid peerData (peerMap s) }

buildVersion :: IO Version
buildVersion = do
    -- TODO: Get our correct IP here
    let zeroAddr = (0x00, 0xffff00000000)
        add      = NetworkAddress 1 zeroAddr 0
        ua       = VarString $ stringToBS haskoinUserAgent
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

managerSink :: Sink (ThreadId, ManagerRequest) ManagerHandle ()
managerSink = awaitForever $ \(tid,req) -> lift $ do
    case req of
        UserBloomFilter b -> processBloomFilter b
        UserPublishTx tx  -> processPublishTx tx
        UserFastCatchup t -> processFastCatchupTime t
        _   -> do
            exists <- peerExists tid
            -- Discard messages from peers that are gone
            -- TODO: Is this always the correct behavior ?
            when exists $ case req of
                PeerHandshake v     -> processPeerHandshake tid v
                PeerDisconnect      -> processPeerDisconnect tid
                PeerMerkleBlock dmb -> processMerkleBlock tid dmb
                PeerMessage msg -> case msg of
                    MHeaders headers -> processHeaders tid headers
                    MInv inv         -> processInv tid inv
                    MTx tx           -> processTx tid tx
                    _                -> return () -- Ignore them for now

-- TODO: When a peer disconnects, try to reconnect using exponential backoff
-- We should do this if we have a static list of peers in a config file.
processPeerDisconnect :: ThreadId -> ManagerHandle ()
processPeerDisconnect tid = do
    $(logDebug) $ T.pack $ unwords
        [ "Peer disconnected"
        , "(", show tid, ")"
        ]
    deletePeerData tid
    syn <- S.gets syncPeer
    when (syn == Just tid) $ do
        S.modify $ \s -> s{ syncPeer = Nothing }
        m <- S.gets peerMap
        -- Choose only peers that have finished the connection handshake
        let vals = filter (\(_,d) -> peerHandshake d) $ M.assocs m
        -- Send a GetHeaders message to all peers. The fastest will become
        -- the new syncPeer
        forM_ vals $ \(t,_) -> sendGetHeaders t True 0x00
    -- TODO: Do something about the inflight merkle blocks of this peer

processPeerHandshake :: ThreadId -> Version -> ManagerHandle ()
processPeerHandshake tid remoteVer = do
    $(logDebug) $ T.pack $ unwords
        [ "Peer connected"
        , "(", show tid, ")"
        ]
    modifyPeerData tid $ \d -> d{ peerHandshake = True 
                                , peerHeight    = startHeight remoteVer
                                }
    -- Set the bloom filter for this connection
    bloom <- S.gets mngrBloom
    when (isJust bloom) $
        sendMessage tid $ MFilterLoad $ FilterLoad $ fromJust bloom

    -- Send pending transactions to broadcast
    -- TODO: Is it enough just to broadcast to 1 peer ?
    pendingTxs <- S.gets broadcastBuffer
    forM_ pendingTxs $ \tx -> sendMessage tid $ MTx tx
    S.modify $ \s -> s{ broadcastBuffer = [] }

    -- Send a GetHeaders regardless if there is already a peerSync. This peer
    -- could still be faster and become the new peerSync.
    sendGetHeaders tid True 0x00

    -- Download more block if some are pending
    downloadBlocks tid

    bestM <- runDB bestBlockHeight
    when (isJust bestM && (fromJust bestM) >= startHeight remoteVer) $
        $(logInfo) $ T.pack $ unwords
            [ "Node is synced with peer. Peer height:"
            , show $ startHeight remoteVer 
            , "Our height:"
            , show $ fromJust bestM
            , "(", show tid, ")"
            ]

processBloomFilter :: BloomFilter -> ManagerHandle ()
processBloomFilter b = do
    prevBloom <- S.gets mngrBloom
    -- Don't load an empty bloom filter
    when (prevBloom /= Just bloom && (not $ bloomEmpty bloom)) $ do
        $(logDebug) "Loading new bloom filter"
        S.modify $ \s -> s{ mngrBloom = Just bloom }
        m <- S.gets peerMap 
        forM_ (M.keys m) $ \tid -> do
            dat <- getPeerData tid
            when (peerHandshake dat) $ 
                sendMessage tid $ MFilterLoad $ FilterLoad bloom
            downloadBlocks tid
  where
    bloom = bloomUpdateEmptyFull b

processPublishTx :: Tx -> ManagerHandle ()
processPublishTx tx = do
    $(logDebug) $ T.pack $ unwords
        [ "Broadcasting transaction to the network:"
        , encodeTxHashLE $ txHash tx
        ]
    m <- S.gets peerMap 
    flags <- forM (M.keys m) $ \tid -> do
        dat <- getPeerData tid
        if (peerHandshake dat) 
            then do
                sendMessage tid $ MTx tx
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
        tids <- M.keys <$> S.gets peerMap 
        forM_ tids downloadBlocks

processHeaders :: ThreadId -> Headers -> ManagerHandle ()
processHeaders tid (Headers h) = do
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
    tids <- M.keys <$> S.gets peerMap 

    -- Adjust the height of peers that sent us INV messages for these headers
    forM newToDwn $ \(h, bid) -> do
        forM tids $ \ti -> do
            dat <- getPeerData ti
            let (xs, ys) = partition (== bid) $ peerBlocks dat
            unless (null xs) $ do
                putPeerData ti dat{ peerBlocks = ys
                                  , peerHeight = max (peerHeight dat) h
                                  }
                when (h > peerHeight dat) $
                    $(logDebug) $ T.pack $ unwords
                        [ "Adjusting peer height to"
                        , show h, "(", show ti, ")"
                        ]

    -- Continue syncing from this node only if it made some progress.
    -- Otherwise, another peer is probably faster/ahead already.
    when newBest $ do
        -- Adjust height of this thread
        let m = fst $ last $ newToDwn
        dat <- getPeerData tid
        when (m > peerHeight dat) $ do
            modifyPeerData tid $ \d -> d{ peerHeight = m }
            $(logDebug) $ T.pack $ unwords
                [ "Adjusting peer height to"
                , show m
                , "(", show tid, ")"
                ]

        -- Update the sync peer 
        dwnSynced <- downloadSynced
        S.modify $ \s -> s{ syncPeer = if dwnSynced then Nothing else Just tid }
        $(logInfo) $ T.pack $ unwords
            [ "New best header height:"
            , show $ fst $ last newToDwn
            ]

        -- Requesting more headers
        sendGetHeaders tid False 0x00

    -- Request block downloads for all peers that are currently idling
    forM_ tids downloadBlocks

processMerkleBlock :: ThreadId -> DecodedMerkleBlock -> ManagerHandle ()
processMerkleBlock tid dmb = do
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
        requests <- peerInflightMerkle <$> getPeerData tid
        let newRequests = delete (nodeBlockHash node) requests
        modifyPeerData tid $ \d -> d{ peerInflightMerkle = newRequests }

        -- If this peer is done, get more merkle blocks to download
        when (null newRequests) $ downloadBlocks tid
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
                when (isJust bestM && h `mod` 100 == 0) $ 
                    $(logDebug) $ T.pack $ unwords
                        [ "Best block height:", show h ]

  where
    go prevHeight ((currHeight, x):xs) 
        | currHeight == prevHeight + 1 = (currHeight, x) : go currHeight xs
        | otherwise = []
    go _ [] = []

processInv :: ThreadId -> Inv -> ManagerHandle ()
processInv tid (Inv vs) = do

    -- Request transactions that we have not seen yet
    notHaveTxs <- filterM ((not <$>) . existsDbTx) txlist
    -- Nub because we may have duplicates (same Tx from many peers)
    S.modify $ \s -> s{ inflightTxs = nub $ inflightTxs s ++ notHaveTxs }
    let getData = GetData $ map (InvVector InvTx . fromIntegral) notHaveTxs
    sendMessage tid $ MGetData getData

    let f (a, b) h = getBlockHeaderNode h >>= \resM -> return $ case resM of
            Just r -> (r:a, b)
            _      -> (a, h:b)

    -- Partition blocks that we know and don't know
    (have, notHave) <- runDB $ foldM f ([],[]) blocklist

    -- Update the peer height and unknown block list
    let m = foldl max 0 $ map nodeHeaderHeight have
    dat <- getPeerData tid
    putPeerData tid dat{ peerBlocks = peerBlocks dat ++ notHave 
                       , peerHeight = max m (peerHeight dat)
                       } 

    when (m > peerHeight dat) $ do
        $(logDebug) $ T.pack $ unwords
            [ "Adjusting peer height to"
            , show m
            , "(", show tid, ")"
            ]

    -- Request headers for blocks we don't have
    forM_ notHave $ \b -> sendGetHeaders tid True b

  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs

-- These are solo transactions not linked to a merkle block (yet)
processTx :: ThreadId -> Tx -> ManagerHandle ()
processTx tid tx = do
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
sendGetHeaders :: ThreadId -> Bool -> BlockHash -> ManagerHandle ()
sendGetHeaders tid full hstop = do
    loc <- runDB $ if full then blockLocator else do
        h <- getBestHeader
        return [nodeBlockHash h]
    sendMessage tid $ MGetHeaders $ GetHeaders 0x01 loc hstop
    $(logDebug) $ T.pack $ unwords 
        [ "Requesting more block headers", "(", show tid, ")" ]

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: ThreadId -> ManagerHandle ()
downloadBlocks tid = do
    -- Request block downloads if some are pending
    peerData   <- getPeerData tid
    bloom      <- S.gets mngrBloom
    sync       <- S.gets syncPeer
    hasCatchup <- runDB getFastCatchup
    let remoteHeight = peerHeight peerData
        requests     = peerInflightMerkle peerData
        canDownload  =  (sync /= Just tid)
                     && (isJust bloom)
                     && (isJust hasCatchup)
                     && (peerHandshake peerData)
                     && (null requests)
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    when canDownload $ do
        toDwn <- runDB $ nextDownloadRange 500 remoteHeight
        unless (null toDwn) $ do
            $(logDebug) $ T.pack $ unwords 
                [ "Requesting more merkle blocks:", "(", show tid, ")" ]
            -- Store the work count for this peer
            modifyPeerData tid $ \d -> 
                d{ peerInflightMerkle = toDwn }
            sendMessage tid $ MGetData $ GetData $ 
                map ((InvVector InvMerkleBlock) . fromIntegral) toDwn
            -- Send a ping to have a recognizable end message for the last
            -- merkle block download
            -- TODO: Compute a random nonce for the ping
            sendMessage tid $ MPing $ Ping 0

getPeerData :: ThreadId -> ManagerHandle PeerData
getPeerData tid = do
    m <- S.gets peerMap 
    -- TODO: can fromJust fail in some cases?
    return $ fromJust $ M.lookup tid m

putPeerData :: ThreadId -> PeerData -> ManagerHandle ()
putPeerData tid d = S.modify $ \s -> s{ peerMap = M.insert tid d (peerMap s) }

modifyPeerData :: ThreadId -> (PeerData -> PeerData) -> ManagerHandle ()
modifyPeerData tid f = do
    d <- getPeerData tid
    putPeerData tid $ f d

deletePeerData :: ThreadId -> ManagerHandle ()
deletePeerData tid = S.modify $ \s -> s{ peerMap = M.delete tid $ peerMap s }

peerExists :: ThreadId -> ManagerHandle Bool
peerExists tid = do
    m <- S.gets peerMap
    return $ M.member tid m

sendMessage :: ThreadId -> Message -> ManagerHandle ()
sendMessage tid msg = do
    d <- getPeerData tid
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

