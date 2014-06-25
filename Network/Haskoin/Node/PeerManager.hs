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
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as Q
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
    , mngrBloom        :: Maybe BloomFilter
    -- We got the headers but still need to download the merkle blocks
    , blocksToDownload :: Q.Seq (Word32, BlockHash)
    -- We received the merkle blocks but buffer them to send them in-order to
    -- the user app
    , receivedBlocks   :: M.Map BlockHeight DecodedMerkleBlock
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerHandshake        :: Bool
    , peerHeight           :: Word32
    , peerMsgChan          :: TBMChan Message
    , peerInflightRequests :: Int
    }

data NodeEvent 
    = MerkleBlockEvent [(BlockChainAction, [TxHash])]
    | TxEvent Tx
    deriving (Eq, Show)

data NodeRequest
    = BloomFilterUpdate BloomFilter
    deriving (Eq, Show)

startNode :: FilePath -> IO (TBMChan NodeEvent, TBMChan NodeRequest)
startNode fp = do
    db <- DB.open fp
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
                                 , mngrBloom        = Nothing
                                 , blocksToDownload = Q.empty
                                 , receivedBlocks   = M.empty
                                 }
    -- Launch thread listening to user requests
    -- TODO: Should we catch exception here?
    _ <- forkIO $ sourceTBMChan rChan $$ processUserRequest mChan

    -- TODO: Catch exceptions here?
    _ <- forkIO $ runStdoutLoggingT $ flip S.evalStateT session $ do 

        $(logDebug) "Building list of merkle blocks to download..."

        -- Initialize the database
        toDwn <- runDB $ initDB >> getBlocksToDownload
        -- Put the missing blocks into the download queue
        S.modify $ \s -> s{ blocksToDownload = Q.fromList toDwn }

        -- Spin up some peer threads
        startPeer $ clientSettings 8333 "localhost"
        startPeer $ clientSettings 8333 "haskoin.com"

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

startPeer :: ClientSettings -> ManagerHandle ()
startPeer remote = do
    vers  <- S.gets mngrVersion
    mChan <- S.gets mngrChan
    pChan <- liftIO . atomically $ do
        c <- newTBMChan 1024
        writeTBMChan c $ MVersion vers
        return c
    $(logDebug) "Starting peer ..."
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
                            , peerInflightRequests = 0
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
    exists <- peerExists tid
    -- Discard messages from peers that are gone
    -- TODO: Is this always the correct behavior ?
    when exists $ case req of
        PeerHandshake v     -> processPeerHandshake tid v
        PeerDisconnect      -> processPeerDisconnect tid
        PeerMerkleBlock dmb -> processMerkleBlock tid dmb
        UserBloomFilter b   -> processBloomFilter b
        PeerMessage msg -> case msg of
            MHeaders headers -> processHeaders tid headers
            MTx tx           -> processTx tid tx
            _                -> return () -- Ignore them for now

processPeerDisconnect :: ThreadId -> ManagerHandle ()
processPeerDisconnect tid = do
    $(logDebug) "Peer disconnected"
    deletePeerData tid
    syn <- S.gets syncPeer
    -- TODO: Implement a smarter algorithm for choosing the download peer
    -- Here, when the download peer dies, we just select the next one in
    -- the list of peers that has a remote peer version.
    when (syn == Just tid) $ do
        m <- S.gets peerMap
        -- Choose only peers that have finished the connection handshake
        let vals = filter (\(_,d) -> peerHandshake d) $ M.assocs m
            tidM = fst <$> listToMaybe vals
        S.modify $ \s -> s{ syncPeer = tidM }
        -- Continue the header download if we have a new peer
        when (isJust tidM) $ sendGetHeaders $ fromJust tidM
    -- TODO: Do something about the inflight merkle blocks of this peer

processPeerHandshake :: ThreadId -> Version -> ManagerHandle ()
processPeerHandshake tid remoteVer = do
    $(logDebug) "Peer connected"
    modifyPeerData tid $ \d -> d{ peerHandshake = True 
                                , peerHeight    = startHeight remoteVer
                                }
    -- Set the bloom filter for this connection
    -- TODO: Something fishy is going on when the filter is empty
    bloom <- S.gets mngrBloom
    when (isJust bloom) $
        sendMessage tid $ MFilterLoad $ FilterLoad $ fromJust bloom

    -- TODO: Implement a smarter algorithm for choosing download peer. Here,
    -- we just select the first node that completed the remote peer handshake.
    syn <- S.gets syncPeer
    when (isNothing syn) $ do
        S.modify $ \s -> s{ syncPeer = Just tid }
        sendGetHeaders tid
    -- Download more block if some are pending
    downloadBlocks tid

processBloomFilter :: BloomFilter -> ManagerHandle ()
processBloomFilter bloom = do
    prevBloom <- S.gets mngrBloom
    when (prevBloom /= Just bloom) $ do
        S.modify $ \s -> s{ mngrBloom = Just bloom }
        m <- S.gets peerMap 
        forM_ (M.keys m) $ \tid -> do
            dat <- getPeerData tid
            when (peerHandshake dat) $ 
                sendMessage tid $ MFilterLoad $ FilterLoad bloom

processHeaders :: ThreadId -> Headers -> ManagerHandle ()
processHeaders tid (Headers h) = do
    -- TODO: The time here is incorrect. It should be a median of all peers.
    adjustedTime <- liftIO getPOSIXTime
    -- TODO: If a block headers can't be added, update DOS status
    (newBest, newToDwn) <- runDB $ do
        before <- bestHeaderHeight
        -- TODO: Handle errors in addBlockHeader. We only don't do anything
        -- in case of RejectHeader
        newToDwn <- forM (map fst h) $ \x -> do
            res <- addBlockHeader x $ round adjustedTime
            case res of
                -- Return the data that will be inserted at the end of
                -- the download queue
                (AcceptHeader n) -> 
                    return $ Just (nodeHeaderHeight n, nodeBlockHash n)
                -- TODO: Remove? This is for debug only
                x -> do
                    liftIO $ print x
                    return Nothing
        after  <- bestHeaderHeight
        return $ (after > before, newToDwn)

    -- Add the blocks to download to the end of the queue
    -- TODO: Should we only download blocks if the headers is 
    -- a best header? (i.e. not a side block). Probably not but we
    -- need to consider the case when an old fork may become the
    -- new head.
    let newToDwnQ = Q.fromList $ catMaybes newToDwn
    S.modify $ \s -> s{ blocksToDownload = (blocksToDownload s) Q.>< newToDwnQ }

    -- Continue syncing from this node
    -- TODO: If this is another node than the sync node, we could have issues
    when newBest $ sendGetHeaders tid

    -- Request block downloads for all peers that are currently idling
    -- TODO: Even for the peer that is syncing the headers?
    tids <- M.keys <$> S.gets peerMap 
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

        -- Import the merkle block in-order into the user app
        bestHeight <- nodeHeaderHeight <$> runDB getBestBlock
        importMerkleBlocks bestHeight

        -- Decrement the peer request counter
        requests <- peerInflightRequests <$> getPeerData tid
        let newRequests = max 0 (requests - 1)
        modifyPeerData tid $ \d -> d{ peerInflightRequests = newRequests }

        -- If this peer is done, get more merkle blocks to download
        when (newRequests == 0) $ downloadBlocks tid
  where
    bid = headerHash $ merkleHeader $ decodedMerkle dmb

-- This function will make sure that the merkle blocks are imported in-order
-- as they may be received out-of-order from the network (concurrent download)
importMerkleBlocks :: BlockHeight -> ManagerHandle ()
importMerkleBlocks height = do
    dwnQueue <- S.gets blocksToDownload
    blockMap <- S.gets receivedBlocks
    let ascList  = M.toAscList blockMap
        toImport = go height ascList
        toKeep   = drop (length toImport) ascList
    -- Send blocks in batches
    when ( length toImport >= 500 || (Q.null dwnQueue && null toKeep) ) $ do
        S.modify $ \s -> s{ receivedBlocks = M.fromList toKeep }
        eChan <- S.gets eventChan
        -- TODO: Import in the wallet
        -- TODO: Deal with reorgs 
        -- TODO: Stall solo transactions until we have synced the chain. This is
        -- to prevent missing a transaction that is in our wallet but we have
        -- not generated the address yet
        pairs <- forM toImport $ \(h, dmb) -> do
            -- Import in blockchain
            node <- runDB $ addMerkleBlock $ decodedMerkle dmb
            liftIO $ atomically $ forM_ (merkleTxs dmb) $ \t ->
                writeTBMChan eChan $ TxEvent t
            return (node, expectedTxs dmb)
        liftIO $ atomically $ writeTBMChan eChan $ MerkleBlockEvent pairs
  where
    go prevHeight ((currHeight, x):xs) 
        | currHeight == prevHeight + 1 = (currHeight, x) : go currHeight xs
        | otherwise = []
    go _ [] = []

processTx :: ThreadId -> Tx -> ManagerHandle ()
processTx tid tx = do
    eChan <- S.gets eventChan
    liftIO $ atomically $ writeTBMChan eChan $ TxEvent tx

-- Send out a GetHeaders request for a given peer
sendGetHeaders :: ThreadId -> ManagerHandle ()
sendGetHeaders tid = do
    d    <- getPeerData tid
    -- TODO: height is only for the log. Maybe remove it?
    -- TODO: Only build a block locator the first time. Then send the last
    -- known hash from the previous headers message
    (loc,height) <- runDB $ (,) <$> blockLocator <*> bestHeaderHeight
    sendMessage tid $ MGetHeaders $ GetHeaders 0x01 loc 0x00
    $(logInfo) $ T.pack $ unwords
        [ "Requesting block headers:"
        -- TODO: This can be negative if the remote note got a new block. More
        -- generally, we need to correctly track peerHeight when a remote peer
        -- receives new blocks
        , show $ (peerHeight d) - fromIntegral height 
        , "left to download."
        , show tid
        ]

-- Look at the block download queue and request a peer to download more blocks
-- if the peer is connected, idling and meets the block height requirements.
downloadBlocks :: ThreadId -> ManagerHandle ()
downloadBlocks tid = do
    -- Request block downloads if some are pending
    queue         <- S.gets blocksToDownload
    peerData      <- getPeerData tid
    let remoteHeight  = peerHeight peerData
        handshake     = peerHandshake peerData
        requests      = peerInflightRequests peerData
    -- Only download blocks from peers that have a completed handshake
    -- and that are not already requesting blocks
    -- TODO: The peer downloading the headers should not also download
    -- merkle blocks
    -- TODO: Only download blocks after the wallet first key timestamp
    -- TODO: Should we allow download if, say, requests < x ?
    when ((not $ Q.null queue) && handshake && requests == 0) $ do
        let firstHeight   = fst $ Q.index queue 0
            (toDwn, rest) = Q.spanl f queue
            -- First 500 blocks that match the peer height requirements
            f (i,_) = i <= remoteHeight && i < firstHeight + 500
        when (not $ Q.null toDwn) $ do
            height <- runDB bestBlockHeight
            $(logInfo) $ T.pack $ unwords
                [ "Requesting merkle blocks:"
                -- TODO: This can be negative if the remote note got a new
                -- block. More generally, we need to correctly track peerHeight
                -- when a remote peer receives new blocks
                , show $ (peerHeight peerData) - fromIntegral height 
                , "left to download."
                , show tid
                ]
            S.modify $ \s -> s{ blocksToDownload = rest }
            -- Store the work count for this peer
            modifyPeerData tid $ \d -> 
                d{ peerInflightRequests = Q.length toDwn }
            sendMessage tid $ MGetData $ GetData $ 
                map ((InvVector InvMerkleBlock) . fromIntegral . snd) $ 
                    toList toDwn
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

-- TODO: Remove this if not needed
splitIn :: Int -> [a] -> [[a]]
splitIn i xs 
    | i < 1 = error "Split count must be greater than 0"
    | otherwise = go i xs
  where
    len = length xs `div` i
    go 1 ls = [ls]
    go n ls = (take len ls) : (go (n-1) (drop len ls))
    
