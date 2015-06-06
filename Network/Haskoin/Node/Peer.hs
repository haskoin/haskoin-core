{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Peer 
( startPeer
, newPeerSession
, PeerSession(peerId, peerChan, msgsChan)
) where

import System.Random (randomIO)

import Control.Applicative ((<$>))
import Control.Monad (when, unless, liftM)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (async, waitAnyCatchCancel)
import Control.Monad.Logger 
    ( MonadLogger
    , logInfo, logError, logWarn, logDebug
    )

import Data.Word (Word32)
import Data.List (delete)
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Conduit (Conduit, Sink, yield, awaitForever, await, ($$), ($=))
import Data.Conduit.Network (AppData, appSink, appSource)
import Data.Unique (newUnique, hashUnique)
import Data.Conduit.TMChan 
    ( TBMChan
    , newTBMChan
    , sourceTBMChan
    , writeTBMChan
    , unGetTBMChan
    )
import qualified Data.Text as T (Text, pack)
import qualified Data.Conduit.Binary as CB (take)
import qualified Data.ByteString as BS (ByteString, null, append)

import Network.Socket
    ( SockAddr (SockAddrInet)
    , PortNumber (PortNum)
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Chan

-- TODO: Move constants elsewhere ?
minProtocolVersion :: Word32 
minProtocolVersion = 60001

data PeerSession = PeerSession
    { peerId         :: !PeerId
    , mngrChan       :: !(TBMChan ManagerMessage)
    , msgsChan       :: !(TBMChan Message)
    , peerChan       :: !(TBMChan PeerMessage)
    , bkchChan       :: !(TBMChan BlockChainMessage)
    , mempChan       :: !(TBMChan MempoolMessage)
    , peerVersion    :: !(Maybe Version)
    -- Current Job that the peer is working on
    , currentJob     :: !(Maybe Job)
    -- Aggregate all the transaction of a merkle block before sending
    -- them up to the manager
    , inflightMerkle :: !(Maybe DecodedMerkleBlock)
    -- Buffer merkle blocks of a job and send them when the job is done
    , merkleBuffer   :: ![(BlockHash, DecodedMerkleBlock)]
    -- Save the order in which we must send the merkle blocks to the blockchain
    , merkleOrder    :: ![BlockHash]
    } 

-- | Create the session data for a new Peer given a Peer type and a manager
-- channel.
newPeerSession :: (MonadIO m, MonadLogger m)
               => TBMChan ManagerMessage 
               -> TBMChan BlockChainMessage
               -> TBMChan MempoolMessage
               -> m PeerSession
newPeerSession mngrChan bkchChan mempChan = do 
    -- Generate a new Peer unique ID
    peerId   <- liftIO newUnique
    -- Initialize the main Peer message channel
    peerChan <- liftIO $ atomically $ newTBMChan 10000
    -- Initialize the channel to send messages to the remote host
    msgsChan <- liftIO $ atomically $ newTBMChan 10000
    $(logDebug) $ format peerId "Creating a new peer session"
    return $ PeerSession{..}
  where
    peerVersion     = Nothing
    inflightMerkle  = Nothing
    merkleBuffer    = []
    merkleOrder     = []
    currentJob      = Nothing

-- | Start a new Peer application. This function is meant to be used with
-- runTCPClient or runTCPServer.
startPeer :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) 
          => PeerSession -> AppData -> m ()
startPeer session ad = do
    -- Spin up thread for receiving messages from the remote host
    a1 <- async $ flip evalStateT session $ 
        (appSource ad) $$ decodeMessage 
    $(logDebug) $ format pid "Message receiving thread started"
    -- Spin up thread for sending messages to the remote host
    let chan = msgsChan session
    a2 <- async $ flip evalStateT session $ 
        (sourceTBMChan chan) $= encodeMessage $$ (appSink ad)
    $(logDebug) $ format pid "Message sending thread started"
    -- Main peer message processing loop
    a3 <- async $ flip evalStateT session $ do
        vers <- buildVersion
        sendMessage $ MVersion vers
        (sourceTBMChan $ peerChan session) $$ processPeerMessage
    $(logDebug) $ format pid "Peer thread started"
    -- Wait for threads
    (_, resE) <- waitAnyCatchCancel [a1, a2, a3]
    -- Some logging
    case resE of
        Left e -> $(logError) $ format pid $ unwords
            [ "Peer thread stopped with exception:", show e ]
        Right () -> $(logDebug) $ format pid "Peer thread stopped"
  where
    pid = peerId session

-- | Build our Version message
buildVersion :: MonadIO m => m Version
buildVersion = do
    -- TODO: Get our correct IP here
    let add = NetworkAddress 1 $ SockAddrInet (PortNum 0) 0
        ua  = VarString $ stringToBS haskoinUserAgent
    time <- liftIO getPOSIXTime
    rdmn <- liftIO randomIO -- nonce
    return $ Version 70001 1 (floor time) add add rdmn ua 0 False

-- | Main Peer message dispatch function
processPeerMessage :: (MonadLogger m, MonadIO m)
                   => Sink PeerMessage (StateT PeerSession m) ()
processPeerMessage = await >>= \m -> do
    pid <- gets peerId
    case m of
        Just (AssignJob job) -> do
            lift $ do
                currJobM <- gets currentJob
                when (isJust currJobM) $ $(logError) $ format pid 
                    "Scheduling error. Peer received a job while busy."
                modify $ \s -> s{ currentJob = Just job }
                processJob
            processPeerMessage
        Just RetryJob -> do
            $(logDebug) $ format pid "Retrying current active job"
            lift processJob
            processPeerMessage
        Just (MsgFromRemote msg) -> do
            lift $ processRemoteMessage msg 
            processPeerMessage
        Just (ClosePeer) -> 
            $(logDebug) $ format pid "Received a shutdown request"
        Nothing -> return ()

processJob :: (MonadLogger m, MonadIO m) => StateT PeerSession m ()
processJob = do
    pid <- gets peerId
    gets currentJob >>= \jobM -> case jobPayload <$> jobM of 
        Just (JobSendBloomFilter bloom) -> do
            $(logDebug) $ format pid "Processing SendBloomFilter job"
            sendMessage $ MFilterLoad $ FilterLoad bloom
            jobDone
        -- TODO: Warning if the transaction is considered dust
        -- TODO: Handle rebroadcasting if the transaction is not confirmed
        -- TODO: Should we send the transaction through an INV message first?
        Just (JobSendTx tx) -> do
            $(logDebug) $ format pid "Processing SendTx job"
            sendMessage $ MTx tx
            jobDone
        Just (JobHeaderSync loc hstopM) -> do
            $(logDebug) $ format pid "Processing HeaderSync job"
            sendMessage $ MGetHeaders $ GetHeaders 0x01 loc $ fromMaybe 0 hstopM
        Just (JobDwnTxs tids) -> do
            if null tids then jobDone else do
                $(logDebug) $ format pid "Processing DwnTxs job"
                let vs = map (InvVector InvTx . fromIntegral) tids
                sendMessage $ MGetData $ GetData vs
        Just (JobDwnBlocks bids) -> do
            if null bids then jobDone else do
                $(logDebug) $ format pid $ unwords $
                    "Processing DwnBlocks job" : case bids of
                        (s:_:_) -> [ "Start:", encodeBlockHashLE s
                                   , "End:"  , encodeBlockHashLE $ last bids
                                   ]
                        (s:_)   -> [ "Hash:", encodeBlockHashLE s ]
                        _       -> []
                let vs = map (InvVector InvBlock . fromIntegral) bids
                sendMessage $ MGetData $ GetData vs
        Just (JobDwnMerkles did bids) -> do
            $(logDebug) $ format pid $ unwords $
                "Processing DwnMerkles job" : case bids of
                    (s:_:_) -> [ "Start:", encodeBlockHashLE s
                               , "End:"  , encodeBlockHashLE $ last bids
                               ]
                    (s:_)   -> [ "Hash:", encodeBlockHashLE s ]
                    _       -> []
            if null bids 
                then do
                    sendBlockChain $ IncMerkleBlocks did []
                    jobDone 
                else do
                    order <- gets merkleOrder
                    -- Update the order if it is empty only. Otherwise, a retry
                    -- is triggered and we want to save the complete order
                    -- before the retry.
                    when (null order) $ modify $ \s -> s{ merkleOrder = bids }
                    let vs = map (InvVector InvMerkleBlock . fromIntegral) bids
                    sendMessage $ MGetData $ GetData vs
                    -- Send a ping to have a recognizable end message for
                    -- the last merkle block download.
                    -- TODO: Compute a random nonce for the ping
                    sendMessage $ MPing $ Ping 0
        Nothing -> $(logError) $ format pid "No job available to process"

jobDone :: (MonadLogger m, MonadIO m) => StateT PeerSession m ()
jobDone = do
    jobM <- gets currentJob
    case jobM of
        Just (Job jid _ _ pJob) -> do
            pid <- gets peerId
            $(logDebug) $ format pid $ unwords
                [ "Finished job", show $ hashUnique jid 
                , "of type", showJob pJob
                ]
            sendManager $ PeerJobDone pid jid
            modify $ \s -> s{ currentJob = Nothing }
        _ -> return ()

-- | Process incomming messages from the remote peer
processRemoteMessage :: (MonadLogger m, MonadIO m) 
                     => Message -> StateT PeerSession m ()
processRemoteMessage msg = checkInitVersion >>= \valid -> when valid $ do
    -- After a merkle block we expect to receive transactions related to
    -- that merkle block. As soon as we get a different message than a
    -- transaction, we know that we are done processing the merkle block.
    merkleM <- gets inflightMerkle
    when (isJust merkleM && isNotTx msg) $ endMerkleBlock $ fromJust merkleM
    -- Dispatch the message to the right actor for handling
    pid <- gets peerId
    case msg of
        MVersion v            -> processVersion v
        MVerAck               -> processVerAck 
        MMerkleBlock m        -> processMerkleBlock m
        MTx t                 -> processTx t
        MHeaders hs           -> processHeaders hs
        MBlock b              -> processBlock b 
        MInv inv              -> processInvMessage inv
        MNotFound nf          -> processNotFound nf
        MPing (Ping n) -> do
            $(logDebug) $ format pid $ unwords
                [ "Sending pong in reply to ping with nonce", show n ]
            sendMessage $ MPong $ Pong n
        _ -> do
            $(logDebug) $ format pid "Ignoring this message"
            
  where
    isNotTx (MTx _) = False
    isNotTx _       = True
    isVersion (MVersion _) = True
    isVersion _            = False
    -- Make sure that the first message we receive is a Version message
    checkInitVersion = do
        verM <- gets peerVersion
        let valid = isVersion msg || isJust verM
        unless valid $ do
            pid <- gets peerId
            sendManager $ PeerMisbehaving pid minorDoS
                "Got a non-version message before a version message"
        return valid

processInvMessage :: (MonadLogger m, MonadIO m) 
                  => Inv -> StateT PeerSession m ()
processInvMessage (Inv vs) 
    -- Single blockhash INV is a tickle
    | null txlist && length blocklist == 1 = do
        pid <- gets peerId
        $(logDebug) $ format pid $ unwords
            [ "Received block tickle", encodeBlockHashLE $ head blocklist ]
        sendBlockChain $ BlockTickle pid $ head blocklist
    | otherwise = do
        pid <- gets peerId
        unless (null txlist) $ do
            $(logDebug) $ format pid "Received tx INV"
            sendMempool $ MempoolTxInv pid txlist
        unless (null blocklist) $ do
            $(logDebug) $ format pid "Received block INV"
            sendBlockChain $ BlockInv pid blocklist
  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs

processNotFound :: (MonadLogger m, MonadIO m) 
                => NotFound -> StateT PeerSession m ()
processNotFound (NotFound vs) = do 
    pid <- gets peerId
    unless (null txlist) $ do
        $(logDebug) $ format pid $ unwords $
            [ "Peer does not have the following txs:" ] ++
            (map encodeTxHashLE txlist)
    unless (null blocklist) $ do
        $(logDebug) $ format pid $ unwords $
            [ "Peer does not have the following blocks:" ] ++
            (map encodeBlockHashLE blocklist)
    unless (null merklelist) $ do
        $(logDebug) $ format pid $ unwords $
            [ "Peer does not have the following merkle blocks:" ] ++
            (map encodeBlockHashLE merklelist)
  where
    txlist = map (fromIntegral . invHash) $ 
        filter ((== InvTx) . invType) vs
    blocklist = map (fromIntegral . invHash) $ 
        filter ((== InvBlock) . invType) vs
    merklelist = map (fromIntegral . invHash) $ 
        filter ((== InvMerkleBlock) . invType) vs

-- | Process a Version message sent from the remote host
processVersion :: (MonadLogger m, MonadIO m) 
               => Version -> StateT PeerSession m ()
processVersion remoteVer = go =<< get
  where
    go session
        | isJust $ peerVersion session = do
            sendMessage $ MReject $ 
                reject MCVersion RejectDuplicate "Duplicate version message"
            pid <- gets peerId
            sendManager $ PeerMisbehaving pid minorDoS
                "Remote peer sent a duplicate version message"
        | version remoteVer < minProtocolVersion = do
            pid <- gets peerId
            $(logWarn) $ format pid $ unwords 
                [ "Connected to a peer speaking protocol version"
                , show $ version $ fromJust $ peerVersion session
                , "but we require at least" 
                , show $ minProtocolVersion
                ]
            closePeer
        | otherwise = do
            pid <- gets peerId
            $(logInfo) $ format pid $ unlines
                [ unwords [ "Connected to remote host"
                          , show $ naAddress $ addrSend remoteVer 
                          ]
                , unwords [ "  version  :", show $ version remoteVer ]
                , unwords [ "  subVer   :", show $ userAgent remoteVer ] 
                , unwords [ "  services :", show $ services remoteVer ]
                , unwords [ "  time     :", show $ timestamp remoteVer ]
                , unwords [ "  blocks   :", show $ startHeight remoteVer ]
                ]
            modify $ \s -> s{ peerVersion = Just remoteVer }
            sendMessage MVerAck
            -- Notify the manager that the handshake was succesfull
            sendManager $ PeerConnected pid remoteVer

-- | Process a VerAck message sent from the remote host
processVerAck :: MonadLogger m => StateT PeerSession m ()
processVerAck = do
    pid <- gets peerId
    $(logInfo) $ format pid "Received a version ack."

-- | Process a MerkleBlock sent from the remote host
processMerkleBlock :: (MonadLogger m, MonadIO m) 
                   => MerkleBlock -> StateT PeerSession m ()
processMerkleBlock decodedMerkle@(MerkleBlock bh ntx hs fs) = do
    pid <- gets peerId
    -- Check that we are expecting this merkle bock. Otherwise, the remote
    -- peer is misbehaving by sending us unsolicited junk.
    gets currentJob >>= \jobM -> case jobM of
        Just (Job _ _ _ (JobDwnMerkles _ bids)) ->
            if bid `elem` bids
                then do
                    $(logDebug) $ format pid $ unwords
                        [ "Received merkle block", encodeBlockHashLE bid ]
                    go pid 
                else $(logDebug) $ format pid $ unwords
                    [ "Received an unsolicited merkle block"
                    , encodeBlockHashLE bid
                    ]
        _ -> $(logDebug) $ format pid $ unwords
            [ "Received an unsolicited merkle block"
            , encodeBlockHashLE bid
            ]
  where
    bid = headerHash bh
    go pid = case extractMatches fs hs (fromIntegral ntx) of
        Left err -> sendManager $ PeerMisbehaving pid severeDoS $ unwords 
            [ "Received an invalid merkle block:", err ]
        Right (decodedRoot, expectedTxs) -> if decodedRoot == merkleRoot bh
            then let merkleTxs = []
                     dmb       = DecodedMerkleBlock{..}
                 in if null expectedTxs
                     then endMerkleBlock dmb
                     else do
                        $(logDebug) $ format pid $ unwords
                            [ "Setting inflight merkle block"
                            , encodeBlockHashLE bid
                            ]
                        modify $ \s -> s{ inflightMerkle = Just dmb }
            else sendManager $ PeerMisbehaving pid severeDoS
                "Received a merkle block with an invalid merkle root."

-- | Process a transaction sent from the remote host
processTx :: (MonadLogger m, MonadIO m) => Tx -> StateT PeerSession m ()
processTx tx = do
    pid <- gets peerId
    -- If the transaction is part of a merkle block, buffer it. We will send
    -- everything to the manager together.
    merkleM <- gets inflightMerkle
    case merkleM of
        Just dmb@(DecodedMerkleBlock _ _ match txs) ->
            if tid `elem` match
                then do
                    $(logDebug) $ format pid $ unwords
                        [ "Received merkle block transaction"
                        , encodeTxHashLE tid 
                        ]
                    modify $ \s -> 
                        s{ inflightMerkle = Just dmb{ merkleTxs = tx : txs } }
                else do
                    $(logDebug) $ format pid $ unwords
                        [ "Received transaction" , encodeTxHashLE tid ]
                    endMerkleBlock dmb
                    sendMempool $ MempoolTx tx
        Nothing -> sendMempool $ MempoolTx tx

    -- Check if the transaction is part of a Job
    currJobM <- gets currentJob
    case currJobM of
        Just job@(Job _ _ _ (JobDwnTxs tids)) -> case delete tid tids of
            []    -> jobDone
            tids' -> modify $ \s -> 
                s{ currentJob = Just job{ jobPayload = JobDwnTxs tids' } }
        _ -> return ()
  where
    tid = txHash tx

processHeaders :: (MonadLogger m, MonadIO m) 
               => Headers -> StateT PeerSession m ()
processHeaders (Headers hs) = do
    pid <- gets peerId
    $(logDebug) $ format pid $ unwords
        [ "Received", show $ length hs, "headers" ]
    sendBlockChain $ IncHeaders pid $ map fst hs
    currJobM <- gets currentJob
    case jobPayload <$> currJobM of
        Just (JobHeaderSync _ _) -> jobDone
        _ -> return ()

processBlock :: (MonadLogger m, MonadIO m) => Block -> StateT PeerSession m ()
processBlock block = do
    pid <- gets peerId
    $(logDebug) $ format pid $ unwords
        [ "Received block", encodeBlockHashLE bid ]
    sendBlockChain $ IncBlock block
    -- Check if the block is part of a job
    gets currentJob >>= \jobM -> case jobM of
        Just job@(Job _ _ _(JobDwnBlocks hs)) -> case delete bid hs of
            []  -> jobDone
            hs' -> modify $ \s -> 
                s{ currentJob = Just $ job{ jobPayload = JobDwnBlocks hs' } }
        _ -> return ()
  where
    bid = headerHash $ blockHeader block

-- | When the download of transactions related to a merkle block is finished,
-- this function is called to buffer the merkle block and clean up the data.
endMerkleBlock :: (MonadLogger m, MonadIO m) 
               => DecodedMerkleBlock -> StateT PeerSession m ()
endMerkleBlock dmb@(DecodedMerkleBlock mb _ match txs) = do
    pid <- gets peerId
    $(logDebug) $ format pid $ unwords
        [ "Merkle block", encodeBlockHashLE bid
        , "containing", show $ length txs, "txs is complete."
        ]
    -- Add the merkle block to the buffer
    buffer <- liftM ((bid, orderedDmb) :) $ gets merkleBuffer
    -- Clear the inflight merkle 
    modify $ \s -> s{ inflightMerkle = Nothing }
    -- Get the current job and delete the block hash from the job
    gets currentJob >>= \jobM -> case jobM of
        Just job@(Job _ _ _ (JobDwnMerkles did bids)) ->
            case delete bid bids of
                -- We are done with this merkle job
                [] -> do
                    order <- gets merkleOrder
                    -- Reorder the buffer to match the order of the job
                    let g (a,_) b = a == b
                        orderedBuff = catMaybes $ matchTemplate buffer order g
                    -- Reverse the buffer due to how we built it
                    sendBlockChain $ IncMerkleBlocks did $ map snd orderedBuff
                    modify $ \s -> s{ merkleBuffer = [] 
                                    , merkleOrder  = []
                                    }
                    jobDone
                -- We have more merkles to download in this job
                bids' -> do
                    $(logDebug) $ format pid $ unwords
                        [ "Expecting", show $ length bids'
                        , "more merkle blocks."
                        ]
                    -- Save the new job and merkle buffer
                    let newJob = job{ jobPayload = JobDwnMerkles did bids' }
                    modify $ \s -> s{ currentJob   = Just newJob 
                                    , merkleBuffer = buffer
                                    }
        _ -> return ()
  where
    bid = headerHash $ merkleHeader mb
    -- Keep the same transaction order as in the merkle block
    orderedTxs = catMaybes $ matchTemplate txs match f
    f a b      = txHash a == b
    orderedDmb = dmb{ merkleTxs = orderedTxs }

-- | Decode messages sent from the remote host and send them to the peers main
-- message queue for processing. If we receive invalid messages, this function
-- will also notify the PeerManager about a misbehaving remote host.
decodeMessage :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
              => Sink BS.ByteString (StateT PeerSession m) ()
decodeMessage = do
    pid <- lift $ gets peerId
    -- Message header is always 24 bytes
    headerBytes <- toStrictBS <$> CB.take 24
    -- If headerBytes is empty, the conduit has disconnected and we need to
    -- exit (not recurse). Otherwise, we go into an infinite loop here.
    unless (BS.null headerBytes) $ do
        -- Introspection required to know the length of the payload
        let headerE = decodeToEither headerBytes
        case headerE of
            Left err -> lift $ sendManager $
                PeerMisbehaving pid moderateDoS $ unwords
                    [ "Could not decode message header:"
                    , err
                    , "Bytes:"
                    , bsToHex headerBytes
                    ]
            Right (MessageHeader _ cmd len _) -> do
                $(logDebug) $ format pid $ unwords
                    [ "Received message header of type", show cmd ]
                payloadBytes <- toStrictBS <$> (CB.take $ fromIntegral len)
                case decodeToEither $ headerBytes `BS.append` payloadBytes of
                    Left err -> lift $ sendManager $
                        PeerMisbehaving pid moderateDoS $ unwords
                            [ "Could not decode message payload:", err ]
                    Right res -> do
                        pChan <- lift $ gets peerChan
                        liftIO . atomically $ 
                            writeTBMChan pChan $ MsgFromRemote res
        decodeMessage

-- | Encode message that are being sent to the remote host.
encodeMessage :: Monad m => Conduit Message (StateT PeerSession m) BS.ByteString
encodeMessage = awaitForever $ yield . encode'

{- Helpers -}

-- | Send a message to the remote host connected to this peer
sendMessage :: (MonadLogger m, MonadIO m) => Message -> StateT PeerSession m ()
sendMessage msg = do
    versM <- gets peerVersion
    -- Only send non-version messages when the handshake is complete
    if isJust versM || isVersion msg
        then do
            chan <- gets msgsChan
            liftIO . atomically $ writeTBMChan chan msg
        else $(logWarn) err
  where
    err = "Trying to send a non-version message before the handshake is done"
    isVersion (MVersion _) = True
    isVersion _            = False

sendManager :: MonadIO m => ManagerMessage -> StateT PeerSession m ()
sendManager msg = do
    chan <- gets mngrChan
    liftIO . atomically $ writeTBMChan chan msg

sendBlockChain :: MonadIO m => BlockChainMessage -> StateT PeerSession m ()
sendBlockChain msg = do
    chan <- gets bkchChan
    liftIO . atomically $ writeTBMChan chan msg

sendMempool :: MonadIO m => MempoolMessage -> StateT PeerSession m ()
sendMempool msg = do
    chan <- gets mempChan
    liftIO . atomically $ writeTBMChan chan msg

closePeer :: MonadIO m => StateT PeerSession m ()
closePeer = do
    chan <- gets peerChan
    -- Push the ClosePeer message to the top of the channel
    liftIO . atomically $ unGetTBMChan chan ClosePeer

format :: PeerId -> String -> T.Text
format pid str = T.pack $ concat [ "[Peer ", show $ hashUnique pid, "] ", str ]

