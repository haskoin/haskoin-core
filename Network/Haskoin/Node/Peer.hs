{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Peer where

import System.Random (randomIO)

import Control.Monad (liftM, when, unless, join, forM_, forever)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug, logError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Concurrent (threadDelay, killThread, myThreadId)
import qualified Control.Concurrent.STM.Lock as Lock (with)
import Control.Exception.Lifted (fromException, finally, throwIO, throw)
import Control.Concurrent.STM.TBMChan
    ( TBMChan, writeTBMChan, closeTBMChan, newTBMChan )
import Control.Concurrent.Async.Lifted
    ( race, withAsync, waitAnyCancel, link, waitCatch )
import Control.Concurrent.STM
    ( STM, atomically, readTVar, modifyTVar', swapTVar, retry, newTVarIO )

import Data.List (nub, sort, sortBy)
import Data.Text (Text, pack)
import Data.Word (Word32)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Unique (hashUnique, newUnique)
import Data.Conduit (Conduit, Sink, awaitForever, yield, ($$), ($=))
import Data.Conduit.TMChan (sourceTBMChan)
import qualified Data.Conduit.Binary as CB (take)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString as BS (ByteString, null, append)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Map as M
    ( keys , lookup, assocs, elems, fromList, unionWith )
import Data.Conduit.Network
    ( runGeneralTCPClient, appSink, appSource, clientSettings )

import qualified Database.LevelDB.Base as L (DB, withDB)

import Network.Socket (SockAddr (SockAddrInet))

import Network.Haskoin.Node
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Util

-- TODO: Move constants elsewhere ?
minProtocolVersion :: Word32
minProtocolVersion = 70001

-- Start a reconnecting peer that will idle once the connection is established
-- and the handshake is performed.
startIdlePeer :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
              => PeerHost
              -> NodeT m ()
startIdlePeer ph@PeerHost{..} = withPeerReconnect ph (const $ return ())

-- Start a peer that will try to reconnect when the connection is closed. This
-- function takes a callback that will be called with the PeerId every time
-- a new connection is establishes with the peer host. With reconnect using
-- an exponential backoff. This function blocks until the peer cannot reconnect
-- (either the peer is banned or we already have a peer connected to the
-- given peer host).
withPeerReconnect :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
                  => PeerHost
                  -> (PeerId -> NodeT m ())
                  -> NodeT m ()
withPeerReconnect ph@PeerHost{..} f = do
    -- Create a new unique ID for this peer
    pid <- liftIO newUnique
    -- Wait if there is a reconnection timeout
    maybeWaitReconnect pid
    -- Launch the peer
    withAsync (startPeer pid ph $ f pid) $ \a -> do
        resE <- liftIO $ waitCatch a
        reconnect <- case resE of
            Left se -> do
                $(logError) $ formatPid pid ph $ unwords
                    [ "Peer thread stopped with exception:", show se ]
                return $ case fromException se of
                    Just NodeExceptionBanned    -> False
                    Just NodeExceptionConnected -> False
                    _ -> True
            Right _ -> do
                $(logDebug) $ formatPid pid ph "Peer thread stopped"
                return True
        -- Try to reconnect
        when reconnect $ withPeerReconnect ph f
  where
    maybeWaitReconnect pid = do
        reconnect <- atomicallyNodeT $ do
            sessM <- getHostSession ph
            case sessM of
                Just PeerHostSession{..} -> do
                    -- Compute the new reconnection time (max 15 minutes)
                    let reconnect = min 900 $ 2 * peerHostSessionReconnect
                    -- Save the reconnection time
                    modifyHostSession ph $ \s ->
                        s{ peerHostSessionReconnect = reconnect }
                    return reconnect
                _ -> return 0

        when (reconnect > 0) $ do
            $(logInfo) $ formatPid pid ph $ unwords
                [ "Reconnecting peer in", show reconnect, "seconds" ]
            -- Wait for some time before calling a reconnection
            liftIO $ threadDelay $ reconnect * 1000000

-- Start a peer with with the given peer host/peer id and initiate the
-- network protocol handshake. This function will block until the peer
-- connection is closed or an exception is raised.
startPeer :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
          => PeerId
          -> PeerHost
          -> NodeT m ()
          -> NodeT m ()
startPeer pid ph@PeerHost{..} action = do
    -- Check if the peer host is banned
    banned <- atomicallyNodeT $ isPeerHostBanned ph
    when banned $ do
        $(logWarn) $ formatPid pid ph "Failed to start banned host"
        liftIO $ throwIO NodeExceptionBanned

    -- Check if the peer host is already connected
    connected <- atomicallyNodeT $ isPeerHostConnected ph
    when connected $ do
        $(logWarn) $ formatPid pid ph "This host is already connected"
        liftIO $ throwIO NodeExceptionConnected

    tid     <- liftIO myThreadId
    chan    <- liftIO . atomically $ newTBMChan 1024
    mChan   <- liftIO . atomically $ newTBMChan 1024
    pings   <- liftIO $ newTVarIO []
    atomicallyNodeT $ do
        newPeerSession pid $ PeerSession
            { peerSessionConnected  = False
            , peerSessionVersion    = Nothing
            , peerSessionHeight     = 0
            , peerSessionChan       = chan
            , peerSessionHost       = ph
            , peerSessionThreadId   = tid
            , peerSessionMerkleChan = mChan
            , peerSessionPings      = pings
            , peerSessionScore      = Nothing
            }
        newHostSession ph $ PeerHostSession
            { peerHostSessionScore     = 0
            , peerHostSessionReconnect = 1
            }

    $(logDebug) $ formatPid pid ph "Starting a new client TCP connection"

    -- Start the client TCP connection
    let cs  = clientSettings peerPort $ C.pack peerHost
    runGeneralTCPClient cs (peerTCPClient chan) `finally` cleanupPeer
    return ()
  where
    peerTCPClient chan ad = do
            -- Conduit for receiving messages from the remote host
        let recvMsg = (appSource ad) $$ decodeMessage pid ph
            -- Conduit for sending messages to the remote host
            sendMsg = (sourceTBMChan chan) $= encodeMessage $$ (appSink ad)

        withAsync (evalStateT recvMsg Nothing) $ \a1 -> link a1 >> do
            $(logDebug) $ formatPid pid ph
                "Receiving message thread started..."
            withAsync sendMsg $ \a2 -> link a2 >> do
                $(logDebug) $ formatPid pid ph
                    "Sending message thread started..."
                -- Perform the peer handshake before we continue
                peerHandshake pid ph chan
                -- Send the bloom filter if we have one
                $(logDebug) $ formatPid pid ph
                    "Sending the bloom filter if we have one"
                atomicallyNodeT $ do
                    readTVarS sharedBloomFilter >>= \bloomM -> case bloomM of
                        Just bloom ->
                            sendMessage pid $ MFilterLoad $ FilterLoad bloom
                        _ -> return ()
                withAsync (peerPing pid ph) $ \a3 -> link a3 >> do
                    $(logDebug) $ formatPid pid ph "Ping thread started"
                    action -- Run the user supplied function
                    _ <- liftIO $ waitAnyCancel [a1, a2, a3]
                    return ()

    cleanupPeer = do
        $(logWarn) $ formatPid pid ph "Peer is closing. Running cleanup..."
        atomicallyNodeT $ do
            -- Remove the header syncing peer if necessary
            hPidM <- readTVarS sharedHeaderPeer
            when (hPidM == Just pid) $ writeTVarS sharedHeaderPeer Nothing
            -- Remove the merkle syncing peer if necessary
            mPidM <- readTVarS sharedMerklePeer
            when (mPidM == Just pid) $ writeTVarS sharedMerklePeer Nothing
            -- Remove the session and close the channels
            sessM <- removePeerSession pid
            case sessM of
                Just PeerSession{..} -> lift $ do
                    closeTBMChan peerSessionChan
                    closeTBMChan peerSessionMerkleChan
                _ -> return ()
            -- Update the network height
            updateNetworkHeight

-- Return True if the PeerHost is banned
isPeerHostBanned :: PeerHost -> NodeT STM Bool
isPeerHostBanned ph = do
    hostMap <- readTVarS sharedHostMap
    case M.lookup ph hostMap of
        Just sessTVar -> do
            sess <- lift $ readTVar sessTVar
            return $ isHostScoreBanned $ peerHostSessionScore sess
        _ -> return False

-- Returns True if we have a peer connected to that PeerHost already
isPeerHostConnected :: PeerHost -> NodeT STM Bool
isPeerHostConnected ph = do
    peerMap <- readTVarS sharedPeerMap
    sess <- lift $ mapM readTVar $ M.elems peerMap
    return $ ph `elem` (map peerSessionHost sess)

-- | Decode messages sent from the remote host and send them to the peers main
-- message queue for processing. If we receive invalid messages, this function
-- will also notify the PeerManager about a misbehaving remote host.
decodeMessage
    :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
    => PeerId
    -> PeerHost
    -> Sink BS.ByteString (StateT (Maybe (MerkleBlock, MerkleTxs)) (NodeT m)) ()
decodeMessage pid ph = do
    -- Message header is always 24 bytes
    headerBytes <- BL.toStrict <$> CB.take 24
    -- If headerBytes is empty, the conduit has disconnected and we need to
    -- exit (not recurse). Otherwise, we go into an infinite loop here.
    unless (BS.null headerBytes) $ do
        -- Introspection required to know the length of the payload
        case decodeToEither headerBytes of
            Left err -> lift . lift $ misbehaving pid ph moderateDoS $ unwords
                [ "Could not decode message header:", err
                , "Bytes:", bsToHex headerBytes
                ]
            Right (MessageHeader _ cmd len _) -> do
                $(logDebug) $ formatPid pid ph $ unwords
                    [ "Received message header of type", show cmd ]
                payloadBytes <- BL.toStrict <$> (CB.take $ fromIntegral len)
                case decodeToEither $ headerBytes `BS.append` payloadBytes of
                    Left err -> lift . lift $ misbehaving pid ph moderateDoS $
                        unwords [ "Could not decode message payload:", err ]
                    Right msg -> lift $ processMessage pid ph msg
        decodeMessage pid ph

-- Handle a message from a peer
processMessage :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
               => PeerId
               -> PeerHost
               -> Message
               -> StateT (Maybe (MerkleBlock, MerkleTxs)) (NodeT m) ()
processMessage pid ph msg = checkMerkleEnd >> case msg of
    MVersion v -> lift $ do
        $(logDebug) $ formatPid pid ph "Processing MVersion message"
        join . atomicallyNodeT $ do
            oldVerM <- liftM peerSessionVersion $ getPeerSession pid
            case oldVerM of
                Just _ -> do
                    _ <- trySendMessage pid $ MReject $ reject
                        MCVersion RejectDuplicate "Duplicate version message"
                    return $
                        misbehaving pid ph minorDoS "Duplicate version message"
                Nothing -> do
                    modifyPeerSession pid $ \s ->
                        s{ peerSessionVersion = Just v }
                    return $ return ()
        $(logDebug) $ formatPid pid ph "Done processing MVersion message"
    MPing (Ping nonce) -> lift $ do
        $(logDebug) $ formatPid pid ph "Processing MPing message"
        -- Just reply to the Ping with a Pong message
        _ <- atomicallyNodeT $ trySendMessage pid $ MPong $ Pong nonce
        return ()
    MPong (Pong nonce) -> lift $ do
        $(logDebug) $ formatPid pid ph "Processing MPong message"
        atomicallyNodeT $ do
            PeerSession{..} <- getPeerSession pid
            -- Add the Pong response time
            lift $ modifyTVar' peerSessionPings (++ [nonce])
    MHeaders h -> lift $ do
        $(logDebug) $ formatPid pid ph "Processing MHeaders message"
        _ <- atomicallyNodeT $ tryPutTMVarS sharedHeaders (pid, h)
        return ()
    MInv inv -> lift $ do
        $(logDebug) $ formatPid pid ph "Processing MInv message"
        processInvMessage pid ph inv
    MGetData (GetData inv) -> do
        $(logDebug) $ formatPid pid ph "Processing MGetData message"
        let txlist = filter ((== InvTx) . invType) inv
            txids  = nub $ map (fromIntegral . invHash) txlist
        $(logDebug) $ formatPid pid ph $ unlines $
            "Received GetData request for transactions"
            : map (("  " ++) . encodeTxHashLE) txids
        -- Add the txids to the GetData request map
        mapTVar <- asks sharedTxGetData
        liftIO . atomically $ modifyTVar' mapTVar $ \datMap ->
            let newMap = M.fromList $ map (\tid -> (tid, [(pid, ph)])) txids
            in  M.unionWith (\x -> nub . (x ++)) newMap datMap
    MTx tx -> do
        $(logDebug) $ formatPid pid ph "Processing MTx message"
        PeerSession{..} <- lift . atomicallyNodeT $ getPeerSession pid
        txChan <- lift $ asks sharedTxChan
        get >>= \merkleM -> case merkleM of
            Just (_, mTxs) -> if txHash tx `elem` mTxs
                then do
                    $(logDebug) $ formatPid pid ph $ unwords
                        [ "Received merkle tx", encodeTxHashLE $ txHash tx ]
                    liftIO . atomically $
                        writeTBMChan peerSessionMerkleChan $ Right tx
                else do
                    $(logDebug) $ formatPid pid ph $ unwords
                        [ "Received tx broadcast (ending a merkle block)"
                        , encodeTxHashLE $ txHash tx
                        ]
                    endMerkle
                    liftIO . atomically $ writeTBMChan txChan (pid, ph, tx)
            _ -> do
                $(logDebug) $ formatPid pid ph $ unwords
                    [ "Received tx broadcast", encodeTxHashLE $ txHash tx ]
                liftIO . atomically $ writeTBMChan txChan (pid, ph, tx)
    MMerkleBlock mb@(MerkleBlock mHead ntx hs fs) -> do
        $(logDebug) $ formatPid pid ph "Processing MMerkleBlock message"
        case extractMatches fs hs (fromIntegral ntx) of
            Left err -> lift $ misbehaving pid ph severeDoS $ unwords
                [ "Received an invalid merkle block:", err ]
            Right (decodedRoot, mTxs) ->
                -- Make sure that the merkle roots match
                if decodedRoot == merkleRoot mHead
                    then do
                        $(logDebug) $ formatPid pid ph $ unwords
                            [ "Received valid merkle block"
                            , encodeBlockHashLE $ headerHash mHead
                            ]
                        if null mTxs
                            -- Deliver the merkle block
                            then lift . atomicallyNodeT $ do
                                PeerSession{..} <- getPeerSession pid
                                lift $ writeTBMChan peerSessionMerkleChan $
                                    Left (mb, [])
                            -- Buffer the merkle block until we received all txs
                            else put $! Just (mb, mTxs)
                    else lift $ misbehaving pid ph severeDoS
                        "Received a merkle block with an invalid merkle root"
    _ -> return () -- Ignore other requests
  where
    checkMerkleEnd = unless (isTxMsg msg) endMerkle
    endMerkle = get >>= \merkleM -> case merkleM of
        Just (mb, mTxs) -> do
            lift . atomicallyNodeT $ do
                PeerSession{..} <- getPeerSession pid
                lift $ writeTBMChan peerSessionMerkleChan $ Left (mb, mTxs)
            put Nothing
        _ -> return ()
    isTxMsg (MTx _) = True
    isTxMsg _       = False

processInvMessage :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
                  => PeerId
                  -> PeerHost
                  -> Inv
                  -> NodeT m ()
processInvMessage pid ph (Inv vs) = case tickleM of
    Just tickle -> do
        $(logDebug) $ formatPid pid ph $ unwords
            [ "Received block tickle", encodeBlockHashLE tickle ]
        tickleChan <- asks sharedTickleChan
        liftIO $ atomically $ writeTBMChan tickleChan (pid, ph, tickle)
    _ -> do
        unless (null txlist) $ do
            forM_ txlist $ \tid -> $(logDebug) $ formatPid pid ph $ unwords
                [ "Received transaction INV", encodeTxHashLE tid ]
            -- We simply request the transactions.
            -- TODO: Should we do something more elaborate here?
            atomicallyNodeT $ sendMessage pid $ MGetData $ GetData $
                map (InvVector InvTx . fromIntegral) txlist
        unless (null blocklist) $ do
            $(logDebug) $ formatPid pid ph $ unlines $
                "Received block INV"
                : map (("  " ++) . encodeBlockHashLE) blocklist
            -- We ignore block INVs as we do headers-first sync
            return ()
  where
    -- Single blockhash INV is a tickle
    tickleM = case blocklist of
        (h:[]) -> if null txlist then (Just h) else Nothing
        _ -> Nothing
    txlist :: [TxHash]
    txlist = map (fromIntegral . invHash) $
        filter ((== InvTx) . invType) vs
    blocklist :: [BlockHash]
    blocklist = map (fromIntegral . invHash) $
        filter ((== InvBlock) . invType) vs

-- | Encode message that are being sent to the remote host.
encodeMessage :: (MonadIO m, MonadLogger m)
              => Conduit Message (NodeT m) BS.ByteString
encodeMessage = awaitForever $ yield . encode'

peerPing :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
         => PeerId
         -> PeerHost
         -> NodeT m ()
peerPing pid ph = forever $ do
    $(logDebug) $ formatPid pid ph
        "Waiting until the peer is available for sending pings..."
    atomicallyNodeT $ waitPeerAvailable pid

    nonce <- liftIO randomIO
    nonceTVar <- atomicallyNodeT $ do
        PeerSession{..} <- getPeerSession pid
        sendMessage pid $ MPing $ Ping nonce
        return peerSessionPings

    $(logDebug) $ formatPid pid ph $ unwords
        [ "Waiting for Ping nonce", show nonce ]
    -- Wait 60 seconds for the pong or time out
    startTime <- liftIO getCurrentTime
    resE <- raceTimeout 60 (killPeer nonce) (waitPong nonce nonceTVar)
    case resE of
        Right _ -> do
            endTime <- liftIO getCurrentTime
            (diff, score) <- atomicallyNodeT $ do
                PeerSession{..} <- getPeerSession pid
                -- Compute the ping time and the new score
                let diff  = diffUTCTime endTime startTime
                    score = 0.5*diff + 0.5*(fromMaybe diff peerSessionScore)
                -- Save the score in the peer session
                modifyPeerSession pid $ \s -> s{ peerSessionScore = Just score }
                return (diff, score)
            $(logDebug) $ formatPid pid ph $ unwords
                [ "Got response to ping", show nonce
                , "with time", show diff, "and score", show score
                ]
        _ -> return ()

    -- Sleep 30 seconds before sending the next ping
    liftIO $ threadDelay $ 30 * 1000000
  where
    -- Wait for the Pong message of our Ping nonce to arrive
    waitPong nonce nonceTVar = do
        ns <- liftIO . atomically $ do
            ns <- swapTVar nonceTVar []
            if null ns then retry else return ns
        if nonce `elem` ns then return () else waitPong nonce nonceTVar
    killPeer nonce = do
        $(logWarn) $ formatPid pid ph $ concat
            [ "Did not receive a timely reply for Ping ", show nonce
            , ". Reconnecting the peer."
            ]
        disconnectPeer pid ph

peerHandshake :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
              => PeerId
              -> PeerHost
              -> TBMChan Message
              -> NodeT m ()
peerHandshake pid ph chan = do
    ourVer <- buildVersion
    $(logDebug) $ formatPid pid ph "Sending our version message"
    liftIO . atomically $ writeTBMChan chan $ MVersion ourVer
    -- Wait for the peer version message to arrive
    $(logDebug) $ formatPid pid ph "Waiting for the peers version message..."
    peerVer <- atomicallyNodeT $ waitPeerVersion pid
    $(logInfo) $ formatPid pid ph $ unlines
        [ unwords [ "Connected to peer host"
                  , show $ naAddress $ addrSend peerVer
                  ]
        , unwords [ "  version  :", show $ version peerVer ]
        , unwords [ "  subVer   :", show $ userAgent peerVer ]
        , unwords [ "  services :", show $ services peerVer ]
        , unwords [ "  time     :", show $ timestamp peerVer ]
        , unwords [ "  blocks   :", show $ startHeight peerVer ]
        ]
    -- Check the protocol version
    if version peerVer < minProtocolVersion
        then misbehaving pid ph severeDoS $ unwords
            [ "Connected to a peer speaking protocol version"
            , show $ version peerVer
            , "but we require at least"
            , show $ minProtocolVersion
            ]
        else do
            atomicallyNodeT $ do
                -- Save the peers height and update the network height
                modifyPeerSession pid $ \s ->
                    s{ peerSessionHeight    = startHeight peerVer
                    , peerSessionConnected = True
                    }
                updateNetworkHeight
                -- Reset the reconnection timer (exponential backoff)
                modifyHostSession ph $ \s -> s{ peerHostSessionReconnect = 1 }
                -- ACK the version message
                lift $ writeTBMChan chan MVerAck

            $(logDebug) $ formatPid pid ph "Handshake complete"
  where
    buildVersion = do
        -- TODO: Get our correct IP here
        let add = NetworkAddress 1 $ SockAddrInet 0 0
            ua  = VarString $ C.pack haskoinUserAgent
        time <- liftM floor $ liftIO getPOSIXTime
        rdmn <- liftIO randomIO -- nonce
        h    <- runHeaderTree bestBlockHeaderHeight
        return $ Version { version     = 70001
                         , services    = 1
                         , timestamp   = time
                         , addrRecv    = add
                         , addrSend    = add
                         , verNonce    = rdmn
                         , userAgent   = ua
                         , startHeight = h
                         , relay       = False
                         }

-- Wait for the version message of a peer and return it
waitPeerVersion :: PeerId -> NodeT STM Version
waitPeerVersion pid = do
    PeerSession{..} <- getPeerSession pid
    case peerSessionVersion of
        Just ver -> return ver
        _ -> lift retry

-- Delete the session of a peer and send a kill signal to the peers thread.
-- Unless the peer is banned, the peer will try to reconnect.
disconnectPeer :: (MonadIO m, MonadLogger m)
               => PeerId
               -> PeerHost
               -> NodeT m ()
disconnectPeer pid ph = do
    sessM <- atomicallyNodeT $ tryGetPeerSession pid
    case sessM of
        Just PeerSession{..} -> do
            $(logDebug) $ formatPid pid ph "Killing the peer thread"
            liftIO $ killThread peerSessionThreadId
        _ -> return ()

{- Peer utility functions -}

-- Wait until the given peer is not syncing headers or merkle blocks
waitPeerAvailable :: PeerId -> NodeT STM ()
waitPeerAvailable pid = do
    hPidM <- readTVarS sharedHeaderPeer
    mPidM <- readTVarS sharedMerklePeer
    if (Just pid) `elem` [hPidM, mPidM] then lift retry else return ()

-- Wait for a non-empty bloom filter to be available
waitBloomFilter :: NodeT STM BloomFilter
waitBloomFilter = maybe (lift retry) return =<< readTVarS sharedBloomFilter

sendBloomFilter :: BloomFilter -> NodeT STM ()
sendBloomFilter bloom = when (not $ isBloomEmpty bloom) $ do
    oldBloomM <- readTVarS sharedBloomFilter
    -- Don't do anything if the bloom filter is the same
    if oldBloomM == Just bloom then return () else do
        writeTVarS sharedBloomFilter $ Just bloom
        sendMessageAll $ MFilterLoad $ FilterLoad bloom

-- Returns the median height of all the peers
getMedianHeight :: NodeT STM BlockHeight
getMedianHeight = do
    hs <- liftM (map (peerSessionHeight . snd)) getConnectedPeers
    let (_,ms) = splitAt (length hs `div` 2) $ sort hs
    return $ fromMaybe 0 $ listToMaybe ms

-- Set the network height to the median height of all peers.
updateNetworkHeight :: NodeT STM ()
updateNetworkHeight = writeTVarS sharedNetworkHeight =<< getMedianHeight

getPeers :: NodeT STM [(PeerId, PeerSession)]
getPeers = do
    peerMap <- readTVarS sharedPeerMap
    lift $ mapM f $ M.assocs peerMap
  where
    f (pid, sess) = liftM ((,) pid) $ readTVar sess

getConnectedPeers :: NodeT STM [(PeerId, PeerSession)]
getConnectedPeers = liftM (filter (peerSessionConnected . snd)) getPeers

-- Returns a peer that is connected, at the network height and
-- with the best score.
getPeersAtNetHeight :: NodeT STM [(PeerId, PeerSession)]
getPeersAtNetHeight = do
    -- Find the current network height
    height <- readTVarS sharedNetworkHeight
    getPeersAtHeight (== height)

-- Find the best peer at the given height
getPeersAtHeight :: (BlockHeight -> Bool)
                 -> NodeT STM [(PeerId, PeerSession)]
getPeersAtHeight cmpHeight = do
    peers <- liftM (filter f) getPeers
    -- Choose the peer with the best score
    return $ sortBy s peers
  where
    f (_, p) =
        peerSessionConnected p &&       -- Only connected peers
        isJust (peerSessionScore p) &&  -- Only peers with scores
        cmpHeight (peerSessionHeight p) -- Only peers at the required height
    s (_,a) (_,b) = peerSessionScore a `compare` peerSessionScore b

-- Send a message to a peer only if it is connected. It returns True on
-- success.
trySendMessage :: PeerId -> Message -> NodeT STM Bool
trySendMessage pid msg = do
    sessM <- tryGetPeerSession pid
    lift $ case sessM of
        Just PeerSession{..} ->
            if peerSessionConnected
                then writeTBMChan peerSessionChan msg >> return True
                else return False -- The peer is not yet connected
        _ -> return False -- The peer does not exist

-- Send a message to a peer only if it is connected. It returns True on
-- success. Throws an exception if the peer does not exist or is not connected.
sendMessage :: PeerId -> Message -> NodeT STM ()
sendMessage pid msg = do
    PeerSession{..} <- getPeerSession pid
    if peerSessionConnected
        then lift $ writeTBMChan peerSessionChan msg
        else throw $ NodeExceptionPeerNotConnected pid

-- Send a message to all connected peers.
sendMessageAll :: Message -> NodeT STM ()
sendMessageAll msg = do
    peerMap <- readTVarS sharedPeerMap
    forM_ (M.keys peerMap) $ \pid -> trySendMessage pid msg

getNetworkHeight :: NodeT STM BlockHeight
getNetworkHeight = readTVarS sharedNetworkHeight

misbehaving :: (MonadIO m, MonadLogger m)
            => PeerId
            -> PeerHost
            -> (PeerHostScore -> PeerHostScore)
            -> String
            -> NodeT m ()
misbehaving pid ph f msg = do
    sessM <- atomicallyNodeT $ do
        modifyHostSession ph $ \s ->
            s{ peerHostSessionScore =  f $! peerHostSessionScore s }
        getHostSession ph
    case sessM of
        Just PeerHostSession{..} -> do
            $(logWarn) $ formatPid pid ph $ unlines
                [ "Misbehaving peer"
                , unwords [ "  Score:", show peerHostSessionScore ]
                , unwords [ "  Reason:", msg ]
                ]
            when (isHostScoreBanned peerHostSessionScore) $
                disconnectPeer pid ph
        _ -> return ()

{- LevelDB function -}

runHeaderTree :: MonadIO m => (ReaderT L.DB IO a) -> NodeT m a
runHeaderTree action = do
    lock <- asks sharedLevelDBLock
    fp   <- asks levelDBFilePath
    opts <- asks levelDBOptions
    liftIO $ Lock.with lock $ L.withDB fp opts $ runReaderT action

{- Utilities -}

raceTimeout :: (MonadIO m, MonadBaseControl IO m)
            => Int
               -- ^ Timeout value in seconds
            -> m a
               -- ^ Action to run until the time runs out
            -> m b
               -- ^ Action to run if the main action times out
            -> m (Either a b)
raceTimeout sec cleanup action = do
    resE <- race (liftIO $ threadDelay (sec * 1000000)) action
    case resE of
        Right res -> return $ Right res
        Left _ -> liftM Left cleanup

formatPid :: PeerId -> PeerHost -> String -> Text
formatPid pid ph str = pack $ concat
    [ "[Peer ", show $ hashUnique pid
    , " | ", peerHostString ph, "] ", str
    ]

