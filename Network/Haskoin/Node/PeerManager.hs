{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.PeerManager 
( withPeerManager
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless, forM_, liftM, filterM, forever)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Control 
    ( MonadBaseControl
    , StM
    , control
    , liftBaseDiscard
    )
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (Async, withAsync, link)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug, logError)
import Control.Monad.State (StateT, evalStateT, gets, modify)

import qualified Data.Text as T (Text, pack)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Map as M 
    ( Map
    , insert, delete, lookup, (!)
    , member, keys, elems, adjust
    , toList, empty, size
    , filter, insertWith, null
    , fromAscListWith, toAscList
    )
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.Network 
    ( runGeneralTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , newTBMChan
    , sourceTBMChan
    , writeTBMChan
    , unGetTBMChan
    , closeTBMChan
    , (>=<)
    )

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Chan
import Network.Haskoin.Node.Peer

data PeerState
    = PeerStateNew    -- Connection/Handshake in progress
    | PeerStateReady  -- Ready to receive work
    | PeerStateBusy   -- Busy working
    deriving (Eq, Read, Show)

data PeerType
    = PeerIncoming
    | PeerOutgoing

data ManagerSession = ManagerSession
    { mngrChan      :: !(TBMChan ManagerMessage)
    , bkchChan      :: !(TBMChan BlockChainMessage)
    , mempChan      :: !(TBMChan MempoolMessage)
    , peerMap       :: !(M.Map PeerId PeerData)
    , remoteMap     :: !(M.Map RemoteHost RemoteData)
    , mngrBloom     :: !(Maybe BloomFilter)
    , jobQueue      :: !(M.Map JobPriority [Job])
    -- Map of jobs with resource AllPeers1. We need to guarantee that at least
    -- 1 peer will broadcast the job. This map is used to decide if a job
    -- needs to be reschedule after a peer crashes.
    , broadcastJobs :: !(M.Map JobId Int) 
    } 

-- Data stored about a peer in the Manager
data PeerData = PeerData
    { peerState       :: !PeerState
    , peerType        :: !PeerType
    , peerHeight      :: !BlockHeight
    , peerDataChan    :: !(TBMChan PeerMessage)
    , peerRemote      :: !RemoteHost
    , peerJobs        :: ![Job]
    , peerJobDeadline :: !(Maybe UTCTime)
    }

-- Data stored about a remote host in the Manager
data RemoteData = RemoteData
    { remoteBehavior       :: !Behavior
    , remoteReconnectTimer :: !Int
    }

-- | Start the PeerManager. This function will spin up a new thread and
-- return the PeerManager message channel to communicate with it.
withPeerManager :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
                => TBMChan BlockChainMessage
                => TBMChan MempoolMessage
                -> (TBMChan ManagerMessage -> m ())
                -> m ()
withPeerManager bkchChan mempChan f = do
    mngrChan <- liftIO $ atomically $ newTBMChan 10000
    let peerMap       = M.empty
        remoteMap     = M.empty
        mngrBloom     = Nothing
        jobQueue      = M.empty
        broadcastJobs = M.empty
        session       = ManagerSession{..}

        -- Run the peer manager main processing loop
        run = do
            $(logDebug) $ format "Peer manager thread started"
            sourceTBMChan mngrChan $$ processManagerMessage

        -- Monitoring hearbeat
        heartbeat = do
            $(logDebug) $ format "Heartbeat thread started"
            forever $ do
                liftIO $ threadDelay $ 1000000 * 300 -- Sleep for 5 minutes
                sendManager Heartbeat

    withAsync (evalStateT run session) $ \a1 -> do
        withAsync (evalStateT heartbeat session) $ \a2 -> do
            link a1 >> link a2 >> f mngrChan

-- | Main message dispatch function for the PeerManager
processManagerMessage :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
                      => Sink ManagerMessage (StateT ManagerSession m) ()
processManagerMessage = awaitForever $ \req -> lift $ case req of
    AddRemoteHosts remotes    -> addRemoteHosts remotes
    SetBloomFilter bloom      -> processSetBloomFilter bloom
    PublishJob job res pri    -> publishJob job res pri
    PeerHeight pid h          -> processPeerHeight pid h
    PeerConnected pid ver     -> processPeerConnected pid ver
    PeerClosed pid            -> processPeerClosed pid
    PeerMisbehaving pid f msg -> peerMisbehaving pid f msg
    PeerJobDone pid jid       -> peerJobDone pid jid
    ConnectToRemote remote    -> connectToRemoteHost remote
    Heartbeat                 -> processHeartbeat

-- | Let the PeerManager know about new remote peers to connect to. The
-- PeerManager will add them to its session data and try to connect to
-- them.
addRemoteHosts :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) 
               => [RemoteHost] -> StateT ManagerSession m ()
addRemoteHosts remotes = do
    -- Only add remote peers that we don't know of yet.
    newRemotes <- filterM (fmap not . existsRemote) remotes 

    forM_ newRemotes $ \remote -> do
        $(logDebug) $ format $ unwords
            [ "Adding remote host", showRemoteHost remote ]

        -- Add new RemoteData
        putRemoteData remote $ RemoteData GoodBehavior 1

        -- Connect to the new remote peer
        connectToRemoteHost remote

-- | Connect to a remote peer. This function will spin up a new Peer actor
-- to manage the connection with the remote peer.
connectToRemoteHost :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) 
                    => RemoteHost -> StateT ManagerSession m ()
connectToRemoteHost remote@(RemoteHost host port) = do
    -- Find all peers handling this remote host (should only be 0 or 1)
    peers <- M.filter ((== remote) . peerRemote) <$> gets peerMap
    -- Check if the remote host is banned
    banned <- isRemoteBanned remote
    when banned $ $(logWarn) $ format $ unwords
        [ "Won't connect to banned host", showRemoteHost remote]
    -- Only spin up a new peer if there is not already an active peer and if
    -- the remote host is not banned.
    when (M.null peers && not banned) $ do

        -- Start the peer
        mChan   <- gets mngrChan
        bChan   <- gets bkchChan
        oChan   <- gets mempChan
        session <- newPeerSession mChan bChan oChan

        -- Save the state of the peer
        let pid = peerId session
            peerData = PeerData { peerState       = PeerStateNew
                                , peerType        = PeerOutgoing
                                , peerHeight      = 0
                                , peerDataChan    = peerChan session
                                , peerRemote      = remote
                                , peerJobs        = []
                                , peerJobDeadline = Nothing
                                }

        modify $ \s -> s{ peerMap = M.insert pid peerData (peerMap s) }
        
        let cs        = clientSettings port $ stringToBS host
            cleanup _ = atomically $ do
                closeTBMChan $ msgsChan session
                closeTBMChan $ peerChan session
                writeTBMChan mChan $ PeerClosed pid

        $(logInfo) $ format $ unwords
            [ "Connecting to remote host", showRemoteHost remote
            , "( Peer", show $ hashUnique pid, ")"
            ] 

        -- Spin up the new Peer thread
        _ <- lift $ liftBaseDiscard (flip forkFinally cleanup) $
            runGeneralTCPClient cs $ startPeer session

        return ()

-- | When the peer completes the handshake with the remote host, this
-- function is called. We update the state of the peer and save the
-- height of the remote peers blockchain.
processPeerConnected :: (MonadLogger m, MonadIO m)
                     => PeerId -> Version -> StateT ManagerSession m ()
processPeerConnected pid remoteVer = existsPeerData pid >>= \e -> when e $ do
    remote   <- liftM peerRemote $ getPeerData pid
    oldState <- liftM peerState $ getPeerData pid
    when (oldState == PeerStateNew) $ do
        $(logDebug) $ format $ unwords 
            [ "Peer", show $ hashUnique pid, "handshake complete"
            , "(", showRemoteHost remote, ")."
            , "Updating peer state to ready."
            ]
        -- Update the state to connected and save the remote peers height
        modifyPeerData pid $ \d -> 
            d{ peerState  = PeerStateReady
             , peerHeight = startHeight remoteVer
             }
        -- Reset the remote reconnection timer (exponential backoff)
        modifyRemoteData remote $ \r -> r{ remoteReconnectTimer = 1 }

        -- Send the bloom filter if one is available
        bloomM <- gets mngrBloom
        case bloomM of
            Just bloom -> publishJob (JobSendBloomFilter bloom) (ThisPeer pid) 0
            -- Run job scheduling as we have a new Ready peer that could take
            -- up some work.
            _ -> scheduleJobs

-- | When a peer is closed, this function is called. We set the state of
-- the peer to closed and spin up a thread to reconnect to this remote
-- host after a timeout (exponential backoff).
processPeerClosed :: (MonadLogger m, MonadIO m) 
                  => PeerId -> StateT ManagerSession m ()
processPeerClosed pid = existsPeerData pid >>= \exists -> when exists $ do
    remote <- liftM peerRemote $ getPeerData pid
    $(logWarn) $ format $ unwords
        [ "Peer", show $ hashUnique pid, "closed"
        , "(", showRemoteHost remote, ")."
        , "Rescheduling pending jobs in the peer queue."
        ]
    -- Find jobs that need to be rescheduled
    jobs <- liftM peerJobs $ getPeerData pid
    forM_ jobs $ \job@(Job jid pri res _) -> case res of
        -- Reschedule AnyPeer jobs
        AnyPeer _ -> do
            $(logDebug) $ format $ unwords
                [ "Rescheduling AnyPeer job", show $ hashUnique jid 
                , "previously in peer queue", show $ hashUnique pid
                ]
            modify $ \s -> 
                s{ jobQueue = M.insertWith (flip (++)) pri [job] $ jobQueue s }
        -- AllPeers1 jobs need to be rescheduled if all peers that were 
        -- working on it failed.
        AllPeers1 _ -> do
            -- Check if other peers are working on this job
            cntM <- liftM (M.lookup jid) $ gets broadcastJobs
            case cntM of
                Just n -> if n <= 1
                    -- Only 1 peer was working on this job (us). We have to
                    -- reschedule it.
                    then do
                        $(logDebug) $ format $ unwords
                            [ "Rescheduling AllPeers1 job"
                            , show $ hashUnique jid 
                            , "previously in peer queue", show $ hashUnique pid
                            ]
                        modify $ \s ->
                            s{ jobQueue = 
                                M.insertWith (flip (++)) pri [job] $ jobQueue s 
                            , broadcastJobs = M.delete jid $ broadcastJobs s
                            }
                    -- If more than one peer is working on this job, we reduce
                    -- the counter of active peers as we just died.
                    else do
                        $(logDebug) $ format $ unwords
                            [ "Reducing number of active peers for AllPeers1 job"
                            , show $ hashUnique jid 
                            ]
                        modify $ \s -> 
                            let m = M.adjust (subtract 1) jid $ broadcastJobs s 
                            in  s{ broadcastJobs = m }
                -- If we get Nothing, then the job was succesfully completed
                -- by another peer. No rescheduling is required.
                Nothing -> return ()
        -- Other resource types do not need to be rescheduled
        _ -> return ()

    -- Remote data associated with this peer
    deletePeerData pid
    -- Reschedule jobs after we have deleted the data of this peer so it
    -- doesn't get assigned any work.
    scheduleJobs

    -- Schedule a reconnection to the remote if it is not banned
    banned <- isRemoteBanned remote
    when banned $
        $(logDebug) $ format $ unwords
            [ "Remote host", showRemoteHost remote
            , "is banned. Not scheduling a reconnection."
            ]
    unless banned $ do
        dat <- getRemoteData remote

        -- TODO: Put this value in a config file
        let maxDelay  = 900 -- 15 minutes
            -- Increase the reconnection time (exponential backoff)
            reconnect = min maxDelay $ (2 * remoteReconnectTimer dat)

        $(logInfo) $ format $ unwords
            [ "Reconnecting to remote host", showRemoteHost remote
            , "in", show reconnect, "seconds."
            ]

        -- Save the new reconnection time
        modifyRemoteData remote $ \d -> d{ remoteReconnectTimer = reconnect }

        -- Spin up a new thread to handle reconnection
        mChan <- gets mngrChan
        _ <- liftIO $ forkIO $ do
            -- Thread delay time is in microseconds
            threadDelay $ 1000000 * reconnect
            atomically $ writeTBMChan mChan $ ConnectToRemote remote
        return ()

peerMisbehaving :: (MonadLogger m, MonadIO m)
                => PeerId 
                -> BehaviorUpdate 
                -> String
                -> StateT ManagerSession m ()
peerMisbehaving pid f msg = existsPeerData pid >>= \exists -> when exists $ do
    remote <- liftM peerRemote $ getPeerData pid
    -- Compute the new behavior
    newBehavior <- liftM (f . remoteBehavior) $ getRemoteData remote
    -- Save the new behavior
    modifyRemoteData remote $ \s -> s{ remoteBehavior = newBehavior }

    $(logWarn) $ format $ unlines
        [ "Misbehaving peer"
        , unwords [ "  Host:", showRemoteHost remote ]
        , unwords [ "  Severity:", show newBehavior ]
        , unwords [ "  Reason:", msg ]
        ]

    when (newBehavior == Banned) $ do
        $(logWarn) $ format $ unwords
            [ "Peer", show $ hashUnique pid, "is being banned and closed." ] 
        -- Send a message to the peer to shutdown
        sendPeer pid ClosePeer

-- Increase the height of a peer to the given height if it is greater than
-- the existing one.
processPeerHeight :: MonadLogger m
                  => PeerId -> BlockHeight -> StateT ManagerSession m ()
processPeerHeight pid h = existsPeerData pid >>= \exists -> when exists $ do
    $(logInfo) $ format $ unwords
        [ "Adjusting height of peer", show $ hashUnique pid, "to", show h ]
    dat <- getPeerData pid
    when (h > peerHeight dat) $ modifyPeerData pid $ \s -> s{ peerHeight = h }
    
processSetBloomFilter :: (MonadLogger m, MonadIO m)
                      => BloomFilter -> StateT ManagerSession m ()
processSetBloomFilter bloom = 
    go =<< gets mngrBloom
  where
    go prevBloomM
        | prevBloomM == Just bloom = 
            $(logWarn) $ format "Trying to load an identical bloom filter"
        | isBloomEmpty bloom =
            $(logWarn) $ format "Trying to load an empty bloom filter"
        | otherwise = do
            $(logInfo) $ format "Sending new bloom filter to all peers."
            modify $ \s -> s{ mngrBloom = Just bloom }
            publishJob (JobSendBloomFilter bloom) (AllPeers 0) 0
            -- We notify the blockchain so it can start the merkle download
            -- if it was paused waiting for a valid bloom.
            sendBlockChain ValidBloom

processHeartbeat :: (MonadLogger m, MonadIO m) => StateT ManagerSession m ()
processHeartbeat = do
    $(logDebug) $ format "Monitoring heartbeat"
    now <- liftIO getCurrentTime
    peers <- liftM (M.filter $ f now) $ gets peerMap
    forM_ (M.keys peers) $ \pid -> do
        peerMisbehaving pid minorDoS "Peer did not complete his job on time"
        sendPeer pid RetryJob
    -- Schedule jobs if it hanged for any reasons
    scheduleJobs
  where
    f now dat = case peerJobDeadline dat of
        Just deadline -> now > deadline
        _ -> False

{- Job scheduling -}

publishJob :: (MonadLogger m, MonadIO m)
           => PeerJob -> JobResource -> JobPriority 
           -> StateT ManagerSession m ()
publishJob pJob res pri = do
    jid <- liftIO newUnique
    let job = Job jid pri res pJob
    $(logDebug) $ format $ unwords
        [ "Publishing job", show $ hashUnique jid
        , "of type", showJob $ jobPayload job
        ]
    modify $ \s -> 
        s{ jobQueue = M.insertWith (flip (++)) pri [job] $ jobQueue s }
    scheduleJobs
            
-- Assign jobs to peers based on priority and resource assignment 
scheduleJobs :: (MonadLogger m, MonadIO m) => StateT ManagerSession m ()
scheduleJobs = 
    go . f =<< gets jobQueue
  where
    go queue@((_,job):qs) = do
        success <- scheduleJob job
        if success 
            then go qs
            else modify $ \s -> s{ jobQueue = g queue }
    go [] = modify $ \s -> s{ jobQueue = M.empty }
    -- Turn the map into a list
    f = concat . map (\(pri, jobs) -> map (\j -> (pri, j)) jobs) . M.toAscList
    -- Turn the list back into a map
    g = M.fromAscListWith (flip (++)) . map (\(pri, job) -> (pri, [job]))

-- Schedule one job according to its resource assignment if possible
scheduleJob :: (MonadLogger m, MonadIO m) => Job -> StateT ManagerSession m Bool
scheduleJob job@(Job jid _ res _) = case res of
    -- The job can only be scheduled on a specific peer
    ThisPeer pid -> do
        exists <- existsPeerData pid
        when exists $ addJob pid >> stepPeer pid
        -- We return True even if the peer does not exist as we do not want
        -- this job to sit in the main queue.
        return True
    -- The job can be scheduled on any available peer that meets the height
    -- requirements
    AnyPeer height -> do
        let f dat = isReady dat && goodHeight height dat
        peers <- liftM (M.filter f) $ gets peerMap
        case M.keys peers of
            (pid:_) -> do
                addJob pid >> stepPeer pid 
                return True
            [] -> do
                $(logDebug) $ format $ unwords
                    [ "Could not schedule AnyPeer job", show $ hashUnique jid
                    , "because no peers available at height", show height
                    ]
                return False
    -- The job is scheduled on all peers that meet the height requirements.
    -- If no peers are available, that's ok.
    AllPeers height -> do
        let f dat = goodHeight height dat && isNotNew dat
        peers <- liftM (M.filter f) $ gets peerMap
        forM_ (M.keys peers) $ \pid -> addJob pid >> stepPeer pid
        return True
    -- The job is scheduled on all peers that meet the height requirements.
    -- If no peers are available, the job will remain in the queue to be
    -- rescheduled later.
    AllPeers1 height -> do
        let f dat = goodHeight height dat && isNotNew dat
        peers <- liftM (M.filter f) $ gets peerMap
        if M.null peers
            then do
                $(logDebug) $ format $ unwords
                    [ "Could not schedule AnyPeers1 job", show $ hashUnique jid
                    , "because no peers available at height", show height
                    ]
                return False
            else do
                forM_ (M.keys peers) $ \pid -> addJob pid >> stepPeer pid
                -- Add the number of peers that got this job in the broadcast
                -- map. This will be used in case a peer crashes.
                modify $ \s -> 
                    let newMap = M.insert jid (M.size peers) $ broadcastJobs s
                    in  s{ broadcastJobs = newMap }
                return True
  where
    isNotNew dat = peerState dat /= PeerStateNew
    isReady dat = peerState dat == PeerStateReady
    goodHeight height dat = peerHeight dat >= height
    addJob pid = modifyPeerData pid $ \s -> s{ peerJobs = peerJobs s ++ [job] }

-- If a peer is in ready state, send it the next job in its queue
stepPeer :: (MonadLogger m, MonadIO m) => PeerId -> StateT ManagerSession m ()
stepPeer pid = do
    dat <- getPeerData pid
    when (peerState dat == PeerStateReady) $ do
        case peerJobs dat of
            (job:_) -> do
                $(logDebug) $ format $ unwords
                    [ "Sending job", show $ hashUnique $ jobId job
                    , "to peer", show $ hashUnique pid
                    , "( Type:", showJob $ jobPayload job, ")"
                    ]
                -- Deadline of 2 minutes to complete the job
                deadline <- liftM (addUTCTime 120) $ liftIO getCurrentTime
                modifyPeerData pid $ \s -> 
                    s{ peerState       = PeerStateBusy 
                     , peerJobDeadline = Just deadline
                     }
                sendPeer pid $ AssignJob job
            [] -> return ()

-- When a peer is done with a job, we remove it from the queue and 
-- try to start a new job if one is available.
peerJobDone :: (MonadLogger m, MonadIO m) 
            => PeerId -> JobId -> StateT ManagerSession m ()
peerJobDone pid jid = existsPeerData pid >>= \exists -> when exists $ do
    $(logDebug) $ format $ unwords
        [ "Peer", show $ hashUnique pid, "finished his job." 
        , "Updating peer queue and state."
        ]
    queue <- liftM peerJobs $ getPeerData pid
    newQueue <- case queue of
        (Job jid' _ _ _:qs) -> do
            when (jid /= jid') $
                $(logError) $ format "Scheduling error. Removing wrong JobId."
            return qs
        [] -> do
            $(logError) $ format "Scheduling error. No jobs to remove."
            return []

    -- Set the peer state to ready and update the peers job queue
    modifyPeerData pid $ \s -> 
        s{ peerState       = PeerStateReady
         , peerJobDeadline = Nothing
         , peerJobs        = newQueue
         }

    -- Remove the job form the broadcastJobs map as at least 1 peer completed
    -- that job.
    modify $ \s -> s{ broadcastJobs = M.delete jid $ broadcastJobs s }

    stepPeer pid
    scheduleJobs

{- Helpers -}

sendPeer :: MonadIO m => PeerId -> PeerMessage -> StateT ManagerSession m ()
sendPeer pid msg = do
    pChan <- liftM peerDataChan $ getPeerData pid
    liftIO . atomically $ writeTBMChan pChan msg

sendManager :: MonadIO m => ManagerMessage -> StateT ManagerSession m ()
sendManager msg = do
    mChan <- gets mngrChan
    liftIO . atomically $ writeTBMChan mChan msg

sendBlockChain :: MonadIO m => BlockChainMessage -> StateT ManagerSession m ()
sendBlockChain msg = do
    bChan <- gets bkchChan
    liftIO . atomically $ writeTBMChan bChan msg

existsPeerData :: Monad m => PeerId -> StateT ManagerSession m Bool
existsPeerData pid = liftM (M.member pid) $ gets peerMap

getPeerData :: Monad m => PeerId -> StateT ManagerSession m PeerData
getPeerData pid = liftM (M.! pid) $ gets peerMap

putPeerData :: Monad m => PeerId -> PeerData -> StateT ManagerSession m ()
putPeerData pid d = modify $ \s -> s{ peerMap = M.insert pid d (peerMap s) }

modifyPeerData :: Monad m 
               => PeerId 
               -> (PeerData -> PeerData) 
               -> StateT ManagerSession m ()
modifyPeerData pid f = do
    d <- getPeerData pid
    putPeerData pid $ f d

deletePeerData :: Monad m => PeerId -> StateT ManagerSession m ()
deletePeerData pid = modify $ \s -> s{ peerMap = M.delete pid $ peerMap s }

findPeersAtHeight :: Monad m => BlockHeight -> StateT ManagerSession m [PeerId]
findPeersAtHeight h = do
    peers <- liftM (M.filter f) $ gets peerMap 
    return $ M.keys peers
  where
    f dat = peerHeight dat >= h

existsRemote :: Monad m => RemoteHost -> StateT ManagerSession m Bool
existsRemote remote = liftM (M.member remote) $ gets remoteMap

getRemoteData :: Monad m => RemoteHost -> StateT ManagerSession m RemoteData
getRemoteData remote = liftM (M.! remote) $ gets remoteMap

putRemoteData :: Monad m 
              => RemoteHost -> RemoteData -> StateT ManagerSession m ()
putRemoteData remote d = modify $ 
    \s -> s{ remoteMap = M.insert remote d (remoteMap s) }

modifyRemoteData :: Monad m 
                 => RemoteHost
                 -> (RemoteData -> RemoteData) 
                 -> StateT ManagerSession m ()
modifyRemoteData remote f = do
    d <- getRemoteData remote
    putRemoteData remote $ f d

deleteRemoteData :: Monad m => RemoteHost -> StateT ManagerSession m ()
deleteRemoteData remote = 
    modify $ \s -> s{ remoteMap = M.delete remote $ remoteMap s }

isRemoteBanned :: Monad m => RemoteHost -> StateT ManagerSession m Bool
isRemoteBanned remote = 
    liftM ((== Banned) . remoteBehavior) $ getRemoteData remote

format :: String -> T.Text
format str = T.pack $ unwords [ "[PeerManager]", str ]

