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
    , blocksToDwn      :: [(Word32, BlockHash)]
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
                                 , receivedBlocks   = M.empty
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
    case req of
        StartPeer c -> processStartPeer c
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

    ns <- runDB $ forM (peerInflightMerkle dat) getBlockHeaderNode
    let toDwn = map (\n -> (nodeHeaderHeight n, nodeBlockHash n)) $ catMaybes ns

    -- Store inflight merkle blocks of this peer
    let f a b = fst a `compare` fst b
    S.modify $ \s -> s{ blocksToDwn = sortBy f $ blocksToDwn s ++ toDwn }

    unless (null $ peerInflightMerkle dat) $ $(logInfo) $ T.pack $ unwords
        [ "Peer had inflight merkle blocks. Adding them to download list:"
        , "[", unwords $ 
            map (bsToHex . BS.reverse . encode') $ peerInflightMerkle dat 
        , "]"
        ]

    modifyPeerData remote $ \d -> d{ peerHandshake      = False 
                                   , peerInflightMerkle = []
                                   , peerBlocks         = []
                                   , peerHeight         = 0
                                   , peerReconnectTimer = reconnect*2
                                   }
    -- Handle syncPeer
    syn <- S.gets syncPeer
    when (syn == Just remote) $ do
        $(logInfo) "Finding a new block header syncing peer"
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
    height      <- runDB bestBlockHeight
    blockMap    <- S.gets receivedBlocks
    dwnTxs      <- S.gets inflightTxs
    let ascList  = M.toAscList blockMap
        go prevHeight ((currHeight, x):xs) 
            | currHeight == prevHeight + 1 = (currHeight, x) : go currHeight xs
            | otherwise = []
        go _ [] = []
        toImport = go height ascList
        toKeep   = drop (length toImport) ascList
        -- We stall merkle block imports when transactions are inflight. This
        -- is to prevent this race condition where tx1 would miss it's
        -- confirmation:
        -- INV tx1 -> GetData tx1 -> MerkleBlock (all tx except tx1) -> Tx1
        canImport = null dwnTxs && (not $ null toImport)

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

            unless (null txImport) $ $(logInfo) $ T.pack $ unwords
                [ "Merkle block import: sending"
                , show $ length txImport
                , "transactions to the user"
                ]

            unless (null soloAdd) $ $(logInfo) $ T.pack $ unwords
                [ "Importing these solo txs in the merkle block"
                , "["
                , unwords $ map (bsToHex . BS.reverse . encode') soloAdd
                , "]"
                ]

            -- Never send twice the same data to the user
            txUniqueImport <- filterM filterTx txImport
            alreadySentNode <- existsData $ fromIntegral $ 
                nodeBlockHash $ getActionNode node

            liftIO $ atomically $ do
                -- Send transactions to the wallet
                forM_ txUniqueImport (writeTBMChan eChan . TxEvent)
                -- Send merkle blocks to the wallet 
                unless alreadySentNode $ 
                    writeTBMChan eChan $ MerkleBlockEvent node (expectedTxs dmb)

            -- Save the fact that we sent the data to the user
            forM txUniqueImport $ \tx -> putData $ fromIntegral $ txHash tx
            putData $ fromIntegral $ nodeBlockHash $ getActionNode node

            case node of
                BestBlock b -> do
                    $(logInfo) $ T.pack $ unwords
                        [ "Best block at height"
                        , show $ nodeHeaderHeight b, ":"
                        , enc $ nodeBlockHash b
                        ]
                    return $ Just b
                BlockReorg _ o n -> do
                    $(logInfo) $ T.pack $ unwords
                        [ "Block reorg. Orphaned blocks:"
                        , "[", unwords $ map (enc . nodeBlockHash) o ,"]"
                        , "New blocks:"
                        , "[", unwords $ map (enc . nodeBlockHash) n ,"]"
                        , "New height:"
                        , show $ nodeHeaderHeight $ last n
                        ]
                    return $ Just $ last n
                SideBlock b -> do
                    $(logInfo) $ T.pack $ unwords
                        [ "Side block at height"
                        , show $ nodeHeaderHeight b, ":"
                        , enc $ nodeBlockHash b
                        ]
                    return Nothing

        let bestM = foldl (<|>) Nothing $ reverse res

        synced <- nodeSynced
        when synced $ do
            -- If we are synced, send solo transactions to the wallet
            solo       <- S.gets soloTxs
            uniqueSolo <- filterM filterTx solo
            liftIO $ atomically $ 
                forM_ uniqueSolo (writeTBMChan eChan . TxEvent)
            forM uniqueSolo $ \tx -> putData $ fromIntegral $ txHash tx
            S.modify $ \s -> s{ soloTxs = [] }
            when (isJust bestM) $ $(logInfo) $ T.pack $ unwords
                [ "We are in sync with the network at height"
                , show $ nodeHeaderHeight $ fromJust $ bestM
                ]
  where
    filterTx = ((not <$>) . existsData . fromIntegral . txHash)
    enc = bsToHex . BS.reverse . encode'

processInv :: RemoteHost -> Inv -> ManagerHandle ()
processInv remote (Inv vs) = do

    -- Request transactions that we have not sent to the user yet
    notHaveTxs <- filterM ((not <$>) . existsData . fromIntegral) txlist
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
    dwnTxs <- S.gets inflightTxs
    when (null dwnTxs) $ importMerkleBlocks 
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
        nodeM <- runDB $ getBlockHeaderNode bhash
        when (isJust nodeM && bhash `elem` peerInflightMerkle dat) $ do
            $(logInfo) $ T.pack $ unwords
                [ "Block not found: adding"
                , bsToHex $ BS.reverse $ encode' bhash
                , "to the block download list"
                ]
            -- remove this block from the inflight list of the peer
            modifyPeerData remote $ \d -> 
                d{ peerInflightMerkle = delete bhash $ peerInflightMerkle d }
            -- Add the block to the download list
            let h     = nodeHeaderHeight $ fromJust nodeM
                f a b = fst a `compare` fst b
            S.modify $ \s -> 
                s{ blocksToDwn = sortBy f $ (h,bhash) : blocksToDwn s }

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
            toDwn         = map snd $ take 500 valid
        S.modify $ \s -> s{ blocksToDwn = drop (length toDwn) valid ++ rest }
        unless (null toDwn) $ do
            $(logInfo) $ T.pack $ unwords 
                [ "Requesting more merkle block(s)"
                , "["
                , if length toDwn == 1
                    then bsToHex $ BS.reverse $ encode' $ head toDwn
                    else unwords [show $ length toDwn, "block(s)"]
                , "]"
                , "(", show remote, ")" 
                ]
            -- Store the inflight blocks
            modifyPeerData remote $ \d -> d{ peerInflightMerkle = toDwn }
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

