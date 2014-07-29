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
import Network.Haskoin.Constants

type ManagerHandle = S.StateT ManagerSession (LoggingT IO)

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan ManagerRequest
    , eventChan        :: TBMChan NodeEvent
    , peerMap          :: M.Map RemoteHost PeerData
    , syncPeer         :: Maybe RemoteHost
    , dbHandle         :: DB.DB
    , dataHandle       :: DB.DB
    , mngrBloom        :: Maybe BloomFilter
    -- Missing blocks that need to be re-downloaded
    , blocksToDwn      :: [BlockHash]
    -- Received merkle blocks pending to be sent to the user (wallet)
    , receivedMerkle   :: M.Map BlockHeight [DecodedMerkleBlock]
    -- Transactions not included in a merkle block. We stall them until the
    -- chain is synced.
    , soloTxs          :: [Tx]
    -- Transactions from users that need to be broadcasted
    , broadcastBuffer  :: [Tx]
    , pendingRescan    :: Maybe Word32
    -- How many block should be batched together for download/import
    , blockBatch       :: Int
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerHandshake      :: Bool
    , peerHeight         :: BlockHeight
    -- Blocks that a peer sent us but we haven't linked them yet to our chain.
    -- We use this list to update the peer height when we link those blocks.
    , peerBlocks         :: [BlockHash] 
    , peerMsgChan        :: TBMChan Message
    -- Inflight merkleblock requests for this peer.
    , inflightMerkle     :: M.Map BlockHash Timestamp
    -- Transaction pending the GetData response. Used to stall merkle blocks.
    , inflightTxs        :: [(TxHash, Timestamp)]
    , peerReconnectTimer :: Int
    }

withAsyncNode :: FilePath -> Int
              -> (TBMChan NodeEvent -> TBMChan NodeRequest -> Async () -> IO ())
              -> IO ()
withAsyncNode fp batch f = do
    db <- DB.open (concat [fp, "/headerchain"])
              DB.defaultOptions{ DB.createIfMissing = True
                               , DB.cacheSize       = 2048
                               }
    dataDb <- DB.open (concat [fp, "/datadb"])
                DB.defaultOptions{ DB.createIfMissing = True
                                , DB.cacheSize       = 2048
                                }
    mChan <- atomically $ newTBMChan 10000
    eChan <- atomically $ newTBMChan 10000
    rChan <- atomically $ newTBMChan 10000
    vers  <- buildVersion
    let session = ManagerSession { mngrVersion      = vers
                                 , mngrChan         = mChan
                                 , eventChan        = eChan
                                 , peerMap          = M.empty
                                 , syncPeer         = Nothing
                                 , dbHandle         = db
                                 , dataHandle       = dataDb
                                 , mngrBloom        = Nothing
                                 , blocksToDwn      = []
                                 , receivedMerkle   = M.empty
                                 , soloTxs          = []
                                 , broadcastBuffer  = []
                                 , pendingRescan    = Nothing
                                 , blockBatch       = batch
                                 }

    let runNode = runStdoutLoggingT $ flip S.evalStateT session $ do 

        -- Initialize the database
        runDB initDB

        -- Make sure the genesis block is marked as sent
        putData $ fromIntegral $ headerHash genesisHeader

        -- Find the position of the best block/download pointer
        let go n = do
                exists <- existsData $ fromIntegral $ nodeBlockHash n
                if exists then return n else do
                    par <- runDB $ getParent n
                    go par
        best <- go =<< runDB getBestBlock
        runDB $ do
            putBestBlock $ nodeBlockHash best
            putLastDownload $ nodeBlockHash best

        $(logInfo) $ T.pack $ unwords
            [ "Setting best block to:"
            , encodeBlockHashLE $ nodeBlockHash best
            , "height:"
            , show $ nodeHeaderHeight best
            ]

        -- Process messages
        -- TODO: Close database handle on exception with DB.close
        sourceTBMChan mChan $$ managerSink 

    -- Launch node
    withAsync runNode $ \a -> 
        -- Triggers the monitoring of pending GetData requests
        withAsync (heartBeat mChan) $ \_ ->
            -- Launch thread listening to user requests
            withAsync (sourceTBMChan rChan $$ processUserRequest mChan) $ \_ -> 
                f eChan rChan a

heartBeat :: TBMChan ManagerRequest -> IO ()
heartBeat mChan = forever $ do
    threadDelay $ 1000000 * 120 -- Sleep for 2 minutes
    atomically $ writeTBMChan mChan MonitorRequests

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
    case req of
        StartPeer c -> processStartPeer c
        MonitorRequests -> processMonitor
        PeerHandshake remote v     -> processPeerHandshake remote v
        PeerDisconnect remote      -> processPeerDisconnect remote
        PeerMerkleBlock remote dmb -> processMerkleBlock remote dmb
        PeerMessage remote msg -> case msg of
            MHeaders headers -> processHeaders remote headers
            MInv inv         -> processInv remote inv
            MTx tx           -> processTx remote tx
            MNotFound nf     -> processNotFound remote nf
            _                -> return () -- Ignore them for now
        UserRequest r -> case r of
            ConnectNode h p     -> do
                let remote = RemoteHost h p
                exists <- peerExists remote
                -- Do not allow a user to start multiple times the same peer
                unless exists $ processStartPeer remote
            BloomFilterUpdate b -> processBloomFilter b
            PublishTx tx        -> processPublishTx tx
            FastCatchupTime t   -> processFastCatchupTime t

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
                                    , inflightMerkle     = M.empty
                                    , inflightTxs        = []
                                    , peerReconnectTimer = 1
                                    }
            S.modify $ \s -> s{ peerMap = M.insert remote peerData (peerMap s) }

    -- Start peer thread
    let cs = clientSettings (remotePort remote) (stringToBS $ remoteHost remote) 
    void $ liftIO $ forkFinally 
        (runTCPClient cs $ peer pChan mChan remote) $ \_ -> atomically $ do
            closeTBMChan pChan
            writeTBMChan mChan $ PeerDisconnect remote

-- TODO: Add Ping?
-- TODO: Should we also check for stalled header downloads?
processMonitor :: ManagerHandle ()
processMonitor = do
    $(logDebug) "Monitor heartbeat"

    -- Check stalled merkle blocks
    remotes <- M.keys <$> S.gets peerMap 
    forM_ remotes $ \remote -> do
        now <- round <$> liftIO getPOSIXTime
        dat <- getPeerData remote
        let f t = t + 120 < now -- Stalled for over 2 minutes
            (stalled, rest) = M.partition f $ inflightMerkle dat
        unless (M.null stalled) $ do
            $(logWarn) $ T.pack $ unwords
                [ "Resubmitting stalled merkle blocks:"
                , "["
                , unwords $ map encodeBlockHashLE $ M.keys stalled
                , "]"
                ]
            modifyPeerData remote $ \d -> d{ inflightMerkle = rest }
            let toDwn = M.keys stalled
            S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ toDwn }
            -- Reissue merkle blocks download
            let reorderRemotes = (delete remote remotes) ++ [remote]
            forM_ reorderRemotes downloadBlocks

    -- Check stalled transactions
    forM_ remotes $ \remote -> do
        now <- round <$> liftIO getPOSIXTime
        txs <- inflightTxs <$> getPeerData remote
        let f (_,t)         = t + 120 < now
            (stalled, rest) = partition f txs
        unless (null stalled) $ do
            $(logWarn) $ T.pack $ unwords
                [ "Resubmitting stalled transactions:"
                , "["
                , unwords $ map encodeTxHashLE $ map fst stalled
                , "]"
                ]
            let newTxs  = map (\x -> (fst x,now)) stalled
            modifyPeerData remote $ \d -> d{ inflightTxs = rest ++ newTxs }
            -- Request the transactions again
            let vs      = map (InvVector InvTx . fromIntegral) $ map fst stalled
                getData = GetData vs
            sendMessage remote $ MGetData getData

processPeerDisconnect :: RemoteHost -> ManagerHandle ()
processPeerDisconnect remote = do
    dat <- getPeerData remote
    let reconnect = peerReconnectTimer dat
        -- TODO: Put this value in a config file
        maxDelay = 1000000 * 900 -- 15 minutes
    $(logInfo) $ T.pack $ unwords
        [ "Peer disconnected. Reconnecting in"
        , show $ min (maxDelay `div` 1000000) reconnect
        , "second(s)"
        , "(", show remote, ")" 
        ]

    let toDwn = M.keys $ inflightMerkle dat

    modifyPeerData remote $ \d -> d{ peerHandshake      = False 
                                   , inflightMerkle     = M.empty
                                   , peerBlocks         = []
                                   , peerHeight         = 0
                                   , peerReconnectTimer = reconnect*2
                                   }

    remotes <- M.keys <$> S.gets peerMap 

    -- Handle the inflight merkle blocks of this peer
    unless (null toDwn) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Peer had inflight merkle blocks. Adding them to download list:"
            , "[", unwords $ 
                map encodeBlockHashLE toDwn
            , "]"
            ]
        S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ toDwn }
        -- Request new merkle block downloads
        forM_ remotes downloadBlocks

    -- Handle syncPeer
    syn <- S.gets syncPeer
    when (syn == Just remote) $ do
        $(logInfo) "Finding a new block header syncing peer"
        S.modify $ \s -> s{ syncPeer = Nothing }
        -- Send a GetHeaders message to all peers. The fastest will become
        -- the new syncPeer
        forM_ remotes $ \r -> sendGetHeaders r True 0x00

    -- Handle reconnection
    mChan <- S.gets mngrChan
    void $ liftIO $ forkIO $ do
        -- reconnect is in microseconds
        threadDelay $ min maxDelay (1000000 * reconnect) 
        atomically $ writeTBMChan mChan $ StartPeer remote

processPeerHandshake :: RemoteHost -> Version -> ManagerHandle ()
processPeerHandshake remote remoteVer = do
    $(logInfo) $ T.pack $ unwords
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

    best         <- runDB bestBlockHeight
    fastCatchupM <- runDB getFastCatchup
    when (isJust fastCatchupM && best >= startHeight remoteVer) $
        $(logInfo) $ T.pack $ unwords
            [ "We are synced with peer. Peer height:"
            , show $ startHeight remoteVer 
            , "Our height:"
            , show best
            , "(", show remote, ")" 
            ]

processBloomFilter :: BloomFilter -> ManagerHandle ()
processBloomFilter bloom = do
    prevBloom <- S.gets mngrBloom
    -- Don't load an empty bloom filter
    when (prevBloom /= Just bloom && (not $ isBloomEmpty bloom)) $ do
        $(logInfo) "Loading new bloom filter"
        S.modify $ \s -> s{ mngrBloom = Just bloom }
        m <- S.gets peerMap 
        forM_ (M.keys m) $ \remote -> do
            dat <- getPeerData remote
            when (peerHandshake dat) $ 
                sendMessage remote $ MFilterLoad $ FilterLoad bloom
            downloadBlocks remote

processPublishTx :: Tx -> ManagerHandle ()
processPublishTx tx = do
    $(logInfo) $ T.pack $ unwords
        [ "Broadcasting transaction to the network:"
        , encodeTxHashLE $ txHash tx
        ]
    m <- S.gets peerMap 
    flags <- forM (M.keys m) $ \remote -> do
        dat <- getPeerData remote
        if (peerHandshake dat) 
            then do
                -- TODO: Should we send the transaction like this or through
                -- an INV?
                sendMessage remote $ MTx tx
                return True
            else return False

    -- If no peers are connected, we buffer the transaction and try to send
    -- it later.
    unless (or flags) $ 
        S.modify $ \s -> s{ broadcastBuffer = tx : broadcastBuffer s }

processFastCatchupTime :: Word32 -> ManagerHandle ()
processFastCatchupTime t = do
    remotes <- M.keys <$> S.gets peerMap 
    pendingMerkles <- forM remotes ((inflightMerkle <$>) . getPeerData)
    if (and $ map M.null pendingMerkles)
        then do
            $(logInfo) $ T.pack $ unwords
                [ "Setting fast catchup time:"
                , show t
                ]
            runDB $ setFastCatchup t
            clearData
            -- Don't remember old requests
            S.modify $ \s -> s{ blocksToDwn    = [] 
                              , pendingRescan  = Nothing
                              , receivedMerkle = M.empty
                              }
            -- Trigger downloads
            remotes <- M.keys <$> S.gets peerMap 
            forM_ remotes downloadBlocks
        else do
            $(logInfo) $ T.pack $ unwords
                [ "Waiting for pending merkle blocks to finish" ]
            S.modify $ \s -> s{ pendingRescan = Just t }

processHeaders :: RemoteHost -> Headers -> ManagerHandle ()
processHeaders remote (Headers h) = do
    -- TODO: The time here is incorrect. It should be a median of all peers.
    adjustedTime <- liftIO getPOSIXTime
    -- TODO: If a block headers can't be added, update DOS status
    before      <- runDB bestHeaderHeight
    -- TODO: Handle errors in addBlockHeader. We don't do anything
    -- in case of RejectHeader
    newBlocksM <- forM (map fst h) $ \x -> do
        res <- runDB $ addBlockHeader x (round adjustedTime)
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
    after  <- runDB bestHeaderHeight

    let newBlocks = catMaybes newBlocksM
        newBest  = after > before
    remotes <- M.keys <$> S.gets peerMap 

    -- Adjust the height of peers that sent us INV messages for these headers
    forM newBlocks $ \n -> do
        let h = nodeHeaderHeight n
        forM remotes $ \r -> do
            dat <- getPeerData r
            let (xs, ys) = partition (== nodeBlockHash n) $ peerBlocks dat
            unless (null xs) $ do
                putPeerData r dat{ peerBlocks = ys
                                 , peerHeight = max (peerHeight dat) h
                                 }
                when (h > peerHeight dat) $
                    $(logInfo) $ T.pack $ unwords
                        [ "Adjusting peer height to"
                        , show h
                        , "(", show r, ")"
                        ]

    -- Continue syncing from this node only if it made some progress.
    -- Otherwise, another peer is probably faster/ahead already.
    when newBest $ do
        -- Adjust height of this thread
        let m = nodeHeaderHeight $ last $ newBlocks
        dat <- getPeerData remote
        when (m > peerHeight dat) $ do
            modifyPeerData remote $ \d -> d{ peerHeight = m }
            $(logInfo) $ T.pack $ unwords
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
            , show $ nodeHeaderHeight $ last newBlocks
            ]

        -- Requesting more headers
        sendGetHeaders remote False 0x00

    -- Request block downloads for all peers that are currently idling
    forM_ remotes downloadBlocks

processMerkleBlock :: RemoteHost -> DecodedMerkleBlock -> ManagerHandle ()
processMerkleBlock remote dmb = do
    nodeM <- runDB $ getBlockHeaderNode bid
    let node = fromJust nodeM

    -- Ignore unsolicited merkle blocks
    when (isJust nodeM) $ do

        -- TODO: Handle this error better
        when (decodedRoot dmb /= (merkleRoot $ nodeHeader node)) $
            error "Invalid partial merkle tree received"

        -- Remove merkle blocks from the inflight list
        modifyPeerData remote $ \d -> 
            let newMap = M.delete (nodeBlockHash node) $ inflightMerkle d
            in d{ inflightMerkle = newMap }

        rescan   <- S.gets pendingRescan
        inflight <- inflightMerkle <$> getPeerData remote

        -- When a rescan is pending, don't store the merkle blocks
        when (isNothing rescan) $ do
            S.modify $ \s -> 
                let k      = nodeHeaderHeight node
                    prevM  = M.lookup k $ receivedMerkle s
                    newVal = case prevM of
                        Just prev -> prev ++ [dmb]
                        Nothing   -> [dmb]
                in s{ receivedMerkle = M.insert k newVal $ receivedMerkle s }

            when (M.null inflight) $ do
                importMerkleBlocks 
                downloadBlocks remote

        -- Try to launch the rescan if one is pending
        when (isJust rescan && M.null inflight) $ 
            processFastCatchupTime $ fromJust rescan
  where
    bid = headerHash $ merkleHeader $ decodedMerkle dmb

-- This function will make sure that the merkle blocks are imported in-order
-- as they may be received out-of-order from the network (concurrent download)
importMerkleBlocks :: ManagerHandle ()
importMerkleBlocks = do
    -- Find all inflight transactions
    remotes <- M.keys <$> S.gets peerMap 
    dwnTxs  <- concat <$> forM remotes ((inflightTxs <$>) . getPeerData)
    -- If we are pending a rescan, do not import anything
    rescan  <- S.gets pendingRescan
    -- We stall merkle block imports when transactions are inflight. This
    -- is to prevent this race condition where tx1 would miss it's
    -- confirmation:
    -- INV tx1 -> GetData tx1 -> MerkleBlock (all tx except tx1) -> Tx1
    when (null dwnTxs && isNothing rescan) $ do
        eChan    <- S.gets eventChan
        toImport <- concat . M.elems <$> S.gets receivedMerkle
        res      <- catMaybes <$> forM toImport importMerkleBlock

        -- Send data to the user
        unless (null res) $ do

            let merkles  = map (\x -> (fst3 x, snd3 x)) res
                txGroups = map lst3 res
            liftIO $ atomically $ do
                forM_ txGroups $ \gs -> unless (null gs) $
                    writeTBMChan eChan $ TxEvent gs
                writeTBMChan eChan $ MerkleBlockEvent merkles

            unless (null txGroups) $ $(logInfo) $ T.pack $ unwords
                [ "Merkle block import: sending"
                , show $ length $ concat txGroups
                , "transactions to the user"
                ]

            $(logInfo) $ T.pack $ unwords
                [ "New block height:"
                , show $ maximum $ catMaybes $ map (newHeight . fst3) res
                ]

        synced   <- nodeSynced
        when synced $ do
            -- If we are synced, send solo transactions to the wallet
            solo    <- S.gets soloTxs
            notHave <- filterM filterTx solo
            liftIO $ atomically $ writeTBMChan eChan $ TxEvent notHave
            forM notHave $ putData . fromIntegral . txHash
            S.modify $ \s -> s{ soloTxs = [] }
            height <- runDB bestBlockHeight
            unless (null res) $ $(logInfo) $ T.pack $ unwords
                [ "We are in sync with the network at height"
                , show height
                ]

        -- Try to import more merkle blocks if some were imported this round
        unless (null res) importMerkleBlocks
  where
    filterTx = ((not <$>) . existsData . fromIntegral . txHash)
    newHeight (BlockReorg _ _ n) = Just $ nodeHeaderHeight $ last n
    newHeight (BestBlock n)      = Just $ nodeHeaderHeight n
    newHeight (SideBlock _)      = Nothing

-- Import a single merkle block if its parent has already been imported
importMerkleBlock :: DecodedMerkleBlock 
                  -> ManagerHandle (Maybe (BlockChainAction, [TxHash], [Tx]))
importMerkleBlock dmb = do
    let prevHash = prevBlock $ merkleHeader $ decodedMerkle dmb
    prevNode <- fromJust <$> (runDB $ getBlockHeaderNode prevHash)
    fastCatchupM <- runDB getFastCatchup
    let fastCatchup   = fromJust fastCatchupM
        beforeCatchup = 
            isJust fastCatchupM &&
            blockTimestamp (nodeHeader prevNode) < fastCatchup
    havePrev <- existsData $ fromIntegral prevHash

    -- We must have sent the previous merkle to the user (wallet) to import
    -- this one. Or the previous block can be becore the fast catchup time.
    if not (havePrev || beforeCatchup) then return Nothing else do
        -- Import into the blockchain
        action <- runDB $ addMerkleBlock $ decodedMerkle dmb

        -- Remove this block from the receive list
        S.modify $ \s -> 
            let k      = nodeHeaderHeight $ getActionNode action
                prevM  = M.lookup k $ receivedMerkle s
                newMap = case prevM of
                    Just [dmb] -> M.delete k $ receivedMerkle s
                    Just xs    -> M.insert k (delete dmb xs) $ receivedMerkle s
                    Nothing    -> receivedMerkle s
            in s{ receivedMerkle = newMap }

        -- If solo transactions belong to this merkle block, we have
        -- to import them and remove them from the solo list.
        solo <- S.gets soloTxs
        let f x                 = txHash x `elem` expectedTxs dmb
            (soloAdd, soloKeep) = partition f solo
            allTxs              = nub $ merkleTxs dmb ++ soloAdd
        S.modify $ \s -> s{ soloTxs = soloKeep }

        let bHash = nodeBlockHash $ getActionNode action
        haveNode <- existsData $ fromIntegral bHash 

        if haveNode then return Nothing else do

            -- Never send twice the same data to the user
            let filterTx = ((not <$>) . existsData . fromIntegral . txHash)
            txToImport <- filterM filterTx allTxs

            case action of
                BestBlock b -> return ()
                BlockReorg _ o n -> $(logInfo) $ T.pack $ unwords
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

            -- Save the fact that we sent the data to the user
            forM txToImport $ putData . fromIntegral . txHash
            putData $ fromIntegral bHash
            return $ Just (action, expectedTxs dmb, txToImport)

processInv :: RemoteHost -> Inv -> ManagerHandle ()
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
        -- Request transactions that we have not sent to the user yet
        notHaveTxs <- filterM ((not <$>) . existsData . fromIntegral) txlist
        unless (null notHaveTxs) $ do
            now <- round <$> liftIO getPOSIXTime
            let addInflight = map (\x -> (x,now)) notHaveTxs
            modifyPeerData remote $ \d -> 
                d{ inflightTxs = inflightTxs d ++ addInflight }
            let vs      = map (InvVector InvTx . fromIntegral) notHaveTxs
                getData = GetData vs
            sendMessage remote $ MGetData getData

    -- Process blocks
    unless (null blocklist) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Got block inv"
            , "["
            , unwords $ map encodeBlockHashLE blocklist
            , "]"
            , "(", show remote, ")" 
            ]

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
            $(logInfo) $ T.pack $ unwords
                [ "Adjusting peer height to"
                , show m
                , "(", show remote, ")" 
                ]

        -- Request headers for blocks we don't have. 
        -- TODO: Filter duplicate requests
        forM_ notHave $ \b -> sendGetHeaders remote True b

  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs

-- These are solo transactions not linked to a merkle block (yet)
processTx :: RemoteHost -> Tx -> ManagerHandle ()
processTx remote tx = do
    -- Only process the transaction if we have not sent it to the user yet.
    alreadySentTx <- existsData $ fromIntegral txhash
    when (not alreadySentTx) $ do
        -- Only send to wallet if we are in sync
        synced <- nodeSynced
        if synced 
            then do
                $(logInfo) $ T.pack $ unwords 
                    [ "Got solo tx"
                    , encodeTxHashLE txhash 
                    , "Sending to user"
                    ]
                eChan <- S.gets eventChan
                liftIO $ atomically $ writeTBMChan eChan $ TxEvent [tx]
                putData $ fromIntegral txhash
            else do
                $(logInfo) $ T.pack $ unwords 
                    [ "Got solo tx"
                    , encodeTxHashLE txhash 
                    , "We are not synced. Buffering it."
                    ]
                S.modify $ \s -> s{ soloTxs = nub $ tx : soloTxs s } 

    -- Remove the inflight transaction from all remote inflight lists
    remotes <- M.keys <$> S.gets peerMap 
    forM_ remotes $ \r -> modifyPeerData r $ \d ->
        d{ inflightTxs = filter ((/= txhash) . fst) $ inflightTxs d }
        
    -- If no more transactions are inflight, trigger the download of
    -- the merkle blocks again
    importMerkleBlocks 
  where
    txhash = txHash tx

-- Merkle block requests can return NotFound when a fork is underway. A peer
-- would appear to have the correct height requirement for a block, but
-- could have another fork than the node that synced the headers. 
processNotFound :: RemoteHost -> NotFound -> ManagerHandle ()
processNotFound remote (NotFound vs) = do
    $(logWarn) $ T.pack $ unwords
        [ "Not found:"
        , show vs
        , "(", show remote, ")" 
        ]

    forM_ blocklist $ \bhash -> do
        dat <- getPeerData remote
        when (M.member bhash $ inflightMerkle dat) $ do
            $(logInfo) $ T.pack $ unwords
                [ "Block not found: adding"
                , encodeBlockHashLE bhash
                , "to the block download list"
                ]

            -- remove this block from the inflight list of the peer
            modifyPeerData remote $ \d ->
                let newMap = M.delete bhash $ inflightMerkle d
                in d{ inflightMerkle = newMap }

            -- Add the block to the download list
            S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ [bhash] }

    -- Trigger block downloads, but put this peer at the end
    remotes <- M.keys <$> S.gets peerMap 
    let reorderRemotes = (delete remote remotes) ++ [remote]
    forM_ reorderRemotes downloadBlocks
  where
    -- TODO: Should we handle NotFound for transactions?
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvMerkleBlock) . invType) vs

-- Block height = network height
nodeSynced :: ManagerHandle Bool
nodeSynced = do
    pDats <- M.elems <$> S.gets peerMap 
    let netHeight = foldl max 0 $ map peerHeight pDats
    ourHeight <- runDB bestBlockHeight
    return $ ourHeight == netHeight

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
    handshake <- peerHandshake <$> getPeerData remote
    -- Only peers that have finished the connection handshake
    when handshake $ do
        loc <- runDB $ if full then blockLocator else do
            h <- getBestHeader
            return [nodeBlockHash h]
        $(logInfo) $ T.pack $ unwords 
            [ "Requesting more block headers"
            , "(", show remote, ")" 
            ]
        sendMessage remote $ MGetHeaders $ GetHeaders 0x01 loc hstop

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: RemoteHost -> ManagerHandle ()
downloadBlocks remote = do
    -- Request block downloads if some are pending
    peerData   <- getPeerData remote
    bloom      <- S.gets mngrBloom
    sync       <- S.gets syncPeer
    rescan     <- S.gets pendingRescan
    let remoteHeight = peerHeight peerData
        requests     = inflightMerkle peerData
        canDownload  =  (sync /= Just remote)
                     && (isJust bloom)
                     && (peerHandshake peerData)
                     && (M.null requests)
                     && (isNothing rescan)
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    when canDownload $ do
        batch <- S.gets blockBatch
        -- Get blocks missing from other peers that have disconnected
        (missing, rest) <- splitAt batch <$> S.gets blocksToDwn
        S.modify $ \s -> s{ blocksToDwn = rest }
        -- Get more blocks to download from the database
        xs <- runDB $ getDownloads (batch - length missing) remoteHeight
        let toDwn = missing ++ xs
        unless (null toDwn) $ do
            $(logInfo) $ T.pack $ unwords 
                [ "Requesting more merkle block(s)"
                , "["
                , if length toDwn == 1
                    then encodeBlockHashLE $ head toDwn
                    else unwords [show $ length toDwn, "block(s)"]
                , "]"
                , "(", show remote, ")" 
                ]
            -- Store the inflight blocks
            now <- round <$> liftIO getPOSIXTime
            modifyPeerData remote $ \d -> 
                let newMap = M.fromList $ map (\x -> (x,now)) toDwn
                in d{ inflightMerkle = newMap }
            sendMerkleGetData remote toDwn

sendMerkleGetData :: RemoteHost -> [BlockHash] -> ManagerHandle ()
sendMerkleGetData remote hs = do
    sendMessage remote $ MGetData $ GetData $ 
        map ((InvVector InvMerkleBlock) . fromIntegral) hs
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
    dat <- getPeerData remote
    when (peerHandshake dat) $ do
        -- The message is discarded if the channel is closed.
        liftIO . atomically $ writeTBMChan (peerMsgChan dat) msg

runDB :: DBHandle a -> ManagerHandle a
runDB m = do
    db <- S.gets dbHandle
    liftIO $ S.evalStateT m $ LevelSession db

existsData :: Word256 -> ManagerHandle Bool
existsData h = do
    db <- S.gets dataHandle
    isJust <$> (liftIO $ DB.get db def $ encode' h)

putData :: Word256 -> ManagerHandle ()
putData h = do
    db <- S.gets dataHandle
    liftIO $ DB.put db def (encode' h) BS.empty

clearData :: ManagerHandle ()
clearData = do
    db <- S.gets dataHandle
    DB.withIter db def $ \iter -> do
        DB.iterFirst iter
        keys <- DB.iterKeys iter
        forM_ keys $ DB.delete db def
    putData $ fromIntegral $ headerHash genesisHeader

