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
type BlockHeight = Word32

data ManagerSession = ManagerSession
    { mngrVersion      :: Version
    , mngrChan         :: TBMChan ManagerRequest
    , eventChan        :: TBMChan NodeEvent
    , peerMap          :: M.Map RemoteHost PeerData
    , syncPeer         :: Maybe RemoteHost
    , dbHandle         :: DB.DB
    , dataHandle       :: DB.DB
    , mngrBloom        :: Maybe BloomFilter
    -- Merkle blocks that need to be downloaded
    , blocksToDwn      :: [(BlockHeight, BlockHash)]
    -- Received merkle blocks pending to be sent to the user (wallet)
    , receivedMerkle   :: [DecodedMerkleBlock]
    -- Transaction pending the GetData response. Used to stall merkle blocks.
    , inflightTxs      :: [TxHash]
    -- Transactions not included in a merkle block. We stall them until the
    -- chain is synced.
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
    , peerInflightMerkle   :: [((BlockHeight, BlockHash), Word32)]
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
    dataDb <- DB.open (concat [fp, "/datadb"])
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
                                 , dataHandle       = dataDb
                                 , mngrBloom        = Nothing
                                 , blocksToDwn      = []
                                 , receivedMerkle   = []
                                 , inflightTxs      = []
                                 , soloTxs          = []
                                 , broadcastBuffer  = []
                                 }

    let runNode = runStdoutLoggingT $ flip S.evalStateT session $ do 

        -- Initialize the database
        toDwn <- runDB $ initDB >> getBlocksToDownload

        -- Add merkle blocks that need to be downloaded
        S.modify $ \s -> s{ blocksToDwn = toDwn }

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
heartBeat mChan = do
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
        (runTCPClient cs $ peer pChan mChan remote) $ \_ -> atomically $ do
            closeTBMChan pChan
            writeTBMChan mChan $ PeerDisconnect remote

processMonitor :: ManagerHandle ()
processMonitor = do
    $(logDebug) "Monitor heartbeat"
    now <- round <$> liftIO getPOSIXTime

    -- Check merkle block timings
    remotes <- M.keys <$> S.gets peerMap 
    forM_ remotes $ \remote -> do
        dat <- getPeerData remote
        let f (_,t) = t + 120 < now -- Stalled for over 2 minutes
            (stalled, rest) = partition f $ peerInflightMerkle dat
        unless (null stalled) $ do
            $(logWarn) $ T.pack $ unwords
                [ "Resubmitting stalled merkle blocks:"
                , "["
                , unwords $ map (bsToHex . BS.reverse . encode') $ 
                    map (snd . fst) stalled
                , "]"
                ]
            modifyPeerData remote $ \d -> d{ peerInflightMerkle = rest }
            let g a b = fst a `compare` fst b
            S.modify $ \s -> 
                s{ blocksToDwn = sortBy g $ blocksToDwn s ++ map fst stalled }
            -- Reissue merkle blocks download
            let reorderRemotes = (delete remote remotes) ++ [remote]
            forM_ reorderRemotes downloadBlocks

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

    let toDwn = map fst $ peerInflightMerkle dat

    modifyPeerData remote $ \d -> d{ peerHandshake      = False 
                                   , peerInflightMerkle = []
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
                map (bsToHex . BS.reverse . encode' . snd) toDwn
            , "]"
            ]
        let f a b = fst a `compare` fst b
        S.modify $ \s -> s{ blocksToDwn = sortBy f $ blocksToDwn s ++ toDwn }
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
    hasFastCatchup <- runDB getFastCatchup
    when (isNothing hasFastCatchup) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Setting fast catchup time:"
            , show t
            ]
        toDwn <- runDB $ do
            setFastCatchup t
            getBlocksToDownload
        S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ toDwn }
        -- Trigger download if the node is idle
        remotes <- M.keys <$> S.gets peerMap 
        forM_ remotes downloadBlocks

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
                    , bsToHex $ BS.reverse $ encode' $ nodeBlockHash n
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

    -- Adding new merkle blocks to download
    fastCatchupM <- runDB getFastCatchup
    let fastCatchup = fromJust fastCatchupM
        toDwn       = map (\n -> (nodeHeaderHeight n, nodeBlockHash n)) $ 
                        filter f newBlocks
        f n         = blockTimestamp (nodeHeader n) >= fastCatchup
    when (isJust fastCatchupM) $
        S.modify $ \s -> s{ blocksToDwn = blocksToDwn s ++ toDwn }

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

        -- Put the block first as most of the time, blocks in here will
        -- be orphaned waiting for another block to be processed first.
        S.modify $ \s -> s{ receivedMerkle = dmb : receivedMerkle s }

        -- Decrement the peer request counter
        requests <- peerInflightMerkle <$> getPeerData remote
        let f ((_,x),_) = x /= nodeBlockHash node
            rest        = filter f requests
        modifyPeerData remote $ \d -> d{ peerInflightMerkle = rest }

        importMerkleBlocks 

        -- If this peer is done, get more merkle blocks to download
        downloadBlocks remote
  where
    bid = headerHash $ merkleHeader $ decodedMerkle dmb

-- This function will make sure that the merkle blocks are imported in-order
-- as they may be received out-of-order from the network (concurrent download)
importMerkleBlocks :: ManagerHandle ()
importMerkleBlocks = do
    dwnTxs <- S.gets inflightTxs
    -- We stall merkle block imports when transactions are inflight. This
    -- is to prevent this race condition where tx1 would miss it's
    -- confirmation:
    -- INV tx1 -> GetData tx1 -> MerkleBlock (all tx except tx1) -> Tx1
    when (null dwnTxs) $ do
        toImport <- S.gets receivedMerkle
        res      <- catMaybes <$> forM toImport importMerkleBlock
        synced   <- nodeSynced
        when synced $ do
            -- If we are synced, send solo transactions to the wallet
            solo    <- S.gets soloTxs
            notHave <- filterM filterTx solo
            eChan   <- S.gets eventChan
            liftIO $ atomically $ forM_ notHave (writeTBMChan eChan . TxEvent)
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

-- Import a single merkle block if its parent has already been imported
importMerkleBlock :: DecodedMerkleBlock 
                  -> ManagerHandle (Maybe BlockChainAction)
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

        -- If solo transactions belong to this merkle block, we have
        -- to import them and remove them from the solo list.
        solo <- S.gets soloTxs
        let f x                 = txHash x `elem` expectedTxs dmb
            (soloAdd, soloKeep) = partition f solo
            allTxs              = nub $ merkleTxs dmb ++ soloAdd
        S.modify $ \s -> s{ soloTxs = soloKeep }

        -- Never send twice the same data to the user
        let filterTx = ((not <$>) . existsData . fromIntegral . txHash)
        txToImport <- filterM filterTx allTxs

        unless (null txToImport) $ $(logInfo) $ T.pack $ unwords
            [ "Merkle block import: sending"
            , show $ length txToImport
            , "transactions to the user"
            ]

        unless (null soloAdd) $ $(logInfo) $ T.pack $ unwords
            [ "Importing these solo txs in the merkle block"
            , "["
            , unwords $ map (bsToHex . BS.reverse . encode') soloAdd
            , "]"
            ]

        let bHash = nodeBlockHash $ getActionNode action
        haveNode <- existsData $ fromIntegral bHash 

        unless haveNode $ do
            eChan <- S.gets eventChan
            liftIO $ atomically $ do
                -- Send transactions to the wallet
                forM_ txToImport (writeTBMChan eChan . TxEvent)
                -- Send merkle blocks to the wallet 
                writeTBMChan eChan $ MerkleBlockEvent action (expectedTxs dmb)
            -- Save the fact that we sent the data to the user
            forM txToImport $ putData . fromIntegral . txHash
            putData $ fromIntegral bHash

        -- Remove this block from the receive list
        S.modify $ \s -> s{ receivedMerkle = delete dmb $ receivedMerkle s }

        let enc = bsToHex . BS.reverse . encode'
        case action of
            BestBlock b -> $(logInfo) $ T.pack $ unwords
                [ "Best block at height"
                , show $ nodeHeaderHeight b, ":"
                , enc $ nodeBlockHash b
                ]
            BlockReorg _ o n -> $(logInfo) $ T.pack $ unwords
                [ "Block reorg. Orphaned blocks:"
                , "[", unwords $ map (enc . nodeBlockHash) o ,"]"
                , "New blocks:"
                , "[", unwords $ map (enc . nodeBlockHash) n ,"]"
                , "New height:"
                , show $ nodeHeaderHeight $ last n
                ]
            SideBlock b -> $(logInfo) $ T.pack $ unwords
                [ "Side block at height"
                , show $ nodeHeaderHeight b, ":"
                , enc $ nodeBlockHash b
                ]
        return $ Just action


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
            -- Nub because we may have duplicates (same Tx from many peers)
            S.modify $ \s -> 
                s{ inflightTxs = nub $ inflightTxs s ++ notHaveTxs }
            let vs      = map (InvVector InvTx . fromIntegral) notHaveTxs
                getData = GetData vs
            sendMessage remote $ MGetData getData

    -- Process blocks
    unless (null blocklist) $ do
        $(logInfo) $ T.pack $ unwords
            [ "Got block inv"
            , "["
            , unwords $ map (bsToHex . BS.reverse . encode') blocklist
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
                liftIO $ atomically $ writeTBMChan eChan $ TxEvent tx
                putData $ fromIntegral txhash
            else do
                $(logInfo) $ T.pack $ unwords 
                    [ "Got solo tx"
                    , encodeTxHashLE txhash 
                    , "We are not synced. Buffering it."
                    ]
                S.modify $ \s -> s{ soloTxs = nub $ tx : soloTxs s } 

    -- Remove the inflight transaction from the inflight list
    S.modify $ \s -> s{ inflightTxs = delete txhash $ inflightTxs s }
        
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
        let (match, rest) = partition g $ peerInflightMerkle dat
            g             = (== bhash) . snd . fst
        unless (null match) $ do
            $(logInfo) $ T.pack $ unwords
                [ "Block not found: adding"
                , bsToHex $ BS.reverse $ encode' bhash
                , "to the block download list"
                ]

            -- remove this block from the inflight list of the peer
            modifyPeerData remote $ \d -> d{ peerInflightMerkle = rest }

            -- Add the block to the download list
            let f a b = fst a `compare` fst b
            S.modify $ \s -> 
                s{ blocksToDwn = sortBy f $ blocksToDwn s ++ map fst match }

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
    let remoteHeight = peerHeight peerData
        requests     = peerInflightMerkle peerData
        canDownload  =  (sync /= Just remote)
                     && (isJust bloom)
                     && (peerHandshake peerData)
                     && (null requests)
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    when canDownload $ do
        xs <- S.gets blocksToDwn
        let (valid, rest) = span (\(h,_) -> h <= remoteHeight) xs
            toDwn         = take 500 valid
        S.modify $ \s -> s{ blocksToDwn = drop (length toDwn) valid ++ rest }
        unless (null toDwn) $ do
            $(logInfo) $ T.pack $ unwords 
                [ "Requesting more merkle block(s)"
                , "["
                , if length toDwn == 1
                    then bsToHex $ BS.reverse $ encode' $ snd $ head toDwn
                    else unwords [show $ length toDwn, "block(s)"]
                , "]"
                , "(", show remote, ")" 
                ]
            -- Store the inflight blocks
            now <- round <$> liftIO getPOSIXTime
            modifyPeerData remote $ \d -> 
                d{ peerInflightMerkle = map (\x -> (x,now)) toDwn }
            sendMerkleGetData remote $ map snd toDwn

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
    d <- getPeerData remote
    -- The message is discarded if the channel is closed.
    liftIO . atomically $ writeTBMChan (peerMsgChan d) msg

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

