{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.SpvBlockChain
( withSpvBlockChain
) where

import Control.Applicative ((<$>))
import Control.Monad ( when, unless, forM, forM_, foldM, forever, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug, logError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (Async, withAsync, link)
import Control.Monad.Trans.Control 
    ( MonadBaseControl
    , StM
    , control
    , liftBaseDiscard
    )

import qualified Data.Text as T (Text, pack)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (nub, partition, delete, (\\))
import Data.List.Split (chunksOf)
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , writeTBMChan
    , newTBMChan
    , sourceTBMChan
    )
import Data.Unique (newUnique, hashUnique)
import qualified Data.Map as M 
    ( Map, member, delete, lookup, fromList, fromListWith, null
    , keys, elems, toList, toAscList, empty, map, filter, size
    , adjust, update, singleton, insertWith, insert, assocs, partition
    )

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Constants
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.Chan

data SpvSession = SpvSession
    { -- Peer manager message channel
      mngrChan :: !(TBMChan ManagerMessage)
      -- Mempool message channel
    , mempChan :: !(TBMChan MempoolMessage)
      -- Peer that is currently syncing the headers
    , syncPeer :: !(Maybe PeerId)
      -- Block hashes that a peer advertised to us but we haven't linked them
      -- yet to our chain. We use this map to update the peer height once
      -- those blocks are linked. Only the last tickle of any peer is stored.
    , peerTickles :: !(M.Map PeerId BlockHash)
      -- This value will be True if the wallet has set us a non-empty 
      -- bloom filter.
    , validBloom :: !Bool
      -- Continue downloading blocks from this point on. The merkle download
      -- process will be paused as long as this value is Nothing.
    , windowEnd :: !(Maybe BlockHash)
      -- Do not request merkle blocks with a timestamp before the
      -- fast catchup time.
    , fastCatchup :: !(Maybe Timestamp)
      -- Merkle block window. Every element in the window corresponds to a
      -- merkle block download job. Completed jobs in the window are sent
      -- to the mempool in order.
    , merkleWindow :: !(M.Map DwnId (BlockChainAction, [DecodedMerkleBlock]))
    }

data LocatorType
    = FullLocator
    | PartialLocator
    deriving (Eq, Read, Show)

-- | Start the SpvBlockChain. This function will spin up a new thread and
-- return the BlockChain message channel to communicate with it.
withSpvBlockChain 
    :: (HeaderTree m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
    => TBMChan MempoolMessage
    -> (TBMChan BlockChainMessage -> TBMChan ManagerMessage -> m ())
    -> m ()
withSpvBlockChain mempChan f = do
    bkchChan <- liftIO $ atomically $ newTBMChan 10000
    withPeerManager bkchChan mempChan $ \mngrChan -> do
        let syncPeer     = Nothing
            peerTickles  = M.empty
            windowEnd    = Nothing
            fastCatchup  = Nothing
            merkleWindow = M.empty
            validBloom   = False
            session      = SpvSession{..}
            -- Run the main blockchain message processing loop
            run = do
                $(logDebug) $ format "SPV Blockchain thread started"
                -- Initialize Header Tree
                runDB initHeaderTree
                -- Trigger the header download
                height <- runDB bestBlockHeaderHeight
                headerSync (AnyPeer height) FullLocator Nothing
                -- Process messages
                sourceTBMChan bkchChan $$ processBlockChainMessage

        withAsync (evalStateT run session) $ \a -> do
            link a >> f bkchChan mngrChan

processBlockChainMessage :: (HeaderTree m, MonadLogger m, MonadIO m) 
                         => Sink BlockChainMessage (StateT SpvSession m) ()
processBlockChainMessage = awaitForever $ \req -> lift $ case req of
    BlockTickle pid bid      -> processBlockTickle pid bid
    IncHeaders pid bhs       -> processBlockHeaders pid bhs
    IncMerkleBlocks did dmbs -> processMerkleBlocks did dmbs
    StartDownload   valE     -> processStartDownload valE
    ValidBloom               -> processValidBloom
    _ -> return () -- Ignore block invs (except tickles) and full blocks

-- | Handle block tickles from a peer. A peer can only send us one tickle
-- at a time. If we are syncing the tickle of a peer, we ignore other
-- tickles form the same peer.
processBlockTickle :: (HeaderTree m, MonadLogger m, MonadIO m) 
                   => PeerId -> BlockHash -> StateT SpvSession m ()
processBlockTickle pid bid = do
    $(logInfo) $ format $ unwords
        [ "Received block tickle", encodeBlockHashLE bid
        , "from peer", show $ hashUnique pid
        ]

    nodeM <- runDB $ getBlockHeaderNode bid
    case nodeM of
        Just node -> do
            $(logDebug) $ format "We already have this block hash."
            -- Update the height of the peer who sent us this tickle
            sendManager $ PeerHeight pid $ nodeHeaderHeight node
        Nothing -> do
            $(logDebug) $ format $ unwords
                [ "Buffering block hash tickle to update peer height when"
                , "we can connect it."
                ]
            -- Save the PeerId/BlockHash so we can update the PeerId height
            -- when we can connect the block.
            -- TODO: We could have a DoS leak here
            setPeerTickle pid bid
            -- Only request headers if no header sync is in progress
            peerM <- gets syncPeer
            when (isNothing peerM) $ do
                $(logInfo) $ format $ unwords
                    [ "The header sync peer is now", show $ hashUnique pid ]
                modify $ \s -> s{ syncPeer = Just pid }
                --Request headers so we can connect this block
                headerSync (ThisPeer pid) FullLocator $ Just bid

processBlockHeaders :: (HeaderTree m, MonadLogger m, MonadIO m) 
                    => PeerId -> [BlockHeader] -> StateT SpvSession m ()

processBlockHeaders pid [] = do
    peerM <- gets syncPeer
    -- Only the sync peer is allowed to process headers
    when (isNothing peerM || peerM == Just pid) $ do
        -- TODO: Should we check the tickles to continue the header download ?
        $(logInfo) $ format $ unwords 
            [ "Received empty headers from peer", show $ hashUnique pid ]
        -- There is no more sync peer
        $(logInfo) $ format $ "We have no more header syncing peer."
        modify $ \s -> s{ syncPeer = Nothing }
        -- Try to sync more headers from remaining tickles
        tickles <- gets peerTickles
        case M.assocs tickles of
            -- We have a tickle in the buffer. Let's try to sync it.
            ((p,bid):_) -> do
                $(logDebug) $ format $ 
                    "Syncing more headers from the tickle buffer."
                headerSync (ThisPeer p) FullLocator $ Just bid
                -- Try to download more merkle blocks
                continueMerkleDownload
            _ -> do
                continueMerkleDownload
                -- Check if merkle downloads have reached the headers
                checkSynced

processBlockHeaders pid bhs = do
    peerM <- gets syncPeer
    -- Only the sync peer is allowed to process headers
    when (isNothing peerM || peerM == Just pid) $ do
        $(logDebug) $ format $ unwords
            [ "Received", show $ length bhs, "headers" 
            , "from peer", show $ hashUnique pid
            ]
        now <- liftM round $ liftIO getPOSIXTime
        -- Connect block headers and commit them
        actionE <- runDB $ connectHeaders bhs now True
        case actionE of
            Left err -> sendManager $ PeerMisbehaving pid severeDoS err
            Right action -> case action of
                -- Ignore SideChain/duplicate headers
                SideChain _ -> do
                    $(logDebug) $ format $ unwords
                        [ "Ignoring side chain headers from peer"
                        , show $ hashUnique pid
                        ]
                    -- This peer is not the sync peer anymore.
                    $(logInfo) $ format $ "We have no more header syncing peer."
                    modify $ \s -> s{ syncPeer = Nothing }
                -- Headers extend our current best head
                _ -> do
                    let nodes  = actionNewNodes action
                        height = nodeHeaderHeight $ last nodes
                    $(logInfo) $ format $ unwords 
                        [ "Headers from peer", show $ hashUnique pid
                        , "connected successfully."
                        , "New best header height:", show height
                        ]
                    -- Adjust height of peers that sent us a tickle for
                    -- these blocks
                    forM_ nodes adjustPeerHeight
                    -- Adjust height of the node that sent us these headers
                    sendManager $ PeerHeight pid height
                    -- This peer is the sync peer
                    $(logInfo) $ format $ unwords
                        [ "The header sync peer is now", show $ hashUnique pid ]
                    modify $ \s -> s{ syncPeer = Just pid }
                    -- Continue syncing headers from the same peer
                    headerSync (ThisPeer pid) PartialLocator Nothing
                    -- Try to download more merkle blocks
                    continueMerkleDownload

-- | Request a header download job for the given peer resource
headerSync :: (HeaderTree m, MonadLogger m, MonadIO m)
           => JobResource -> LocatorType -> Maybe BlockHash
           -> StateT SpvSession m ()
headerSync resource locType hStopM = do
    $(logInfo) $ format $ unwords 
        [ "Requesting more headers with block locator type", show locType ]
    -- Build the block locator object
    loc <- runDB $ case locType of
        FullLocator    -> blockLocator 
        PartialLocator -> liftM ((:[]) . nodeBlockHash) getBestBlockHeader
    -- Send a job that can only run on the given PeerId. Priority = 2 to
    -- give priority to BloomFilters (0) and Tx broadcasts (1)
    sendManager $ PublishJob (JobHeaderSync loc hStopM) resource 2

processMerkleBlocks :: (HeaderTree m, MonadLogger m, MonadIO m)
                    => DwnId -> [DecodedMerkleBlock] -> StateT SpvSession m ()
processMerkleBlocks did [] = do
    $(logError) $ format $ "Got a completed merkle job with an empty merkle list"
    modify $ \s -> s{ merkleWindow = M.delete did $ merkleWindow s }
    continueMerkleDownload
processMerkleBlocks did dmbs = do
    $(logDebug) $ format $ unwords
        [ "Received merkle batch id", show $ hashUnique did
        , "containing", show $ length dmbs, "merkle blocks."
        ]
    win <- gets merkleWindow
    -- Find this specific job in the window
    case M.lookup did win of
        Just (action, []) -> do
            $(logDebug) $ format $ unwords
                [ "Saving merkle batch id", show $ hashUnique did
                , "in merkle block window."
                ]
            -- Add the merkles to the window
            modify $ \s -> s{ merkleWindow = M.insert did (action, dmbs) win }
            -- Try to send merkle blocks to the mempool
            dispatchMerkles
        -- This can happen if we had pending jobs when issuing a rescan. We
        -- simply ignore jobs from before the rescan.
        _ -> $(logDebug) $ format $ unwords
            [ "Ignoring merkle batch id", show $ hashUnique did ] 

-- | Look for completed jobs at the start of the window and send them to the 
-- mempool.
dispatchMerkles :: (HeaderTree m, MonadLogger m, MonadIO m) 
                => StateT SpvSession m ()
dispatchMerkles = do
    win <- gets merkleWindow
    case M.assocs win of
        -- If the first batch in the window is ready, send it to the mempool
        ((did, (action, dmbs@(d:ds))):_) -> do
            $(logDebug) $ format $ unwords
                [ "Dispatching merkle batch id", show $ hashUnique did
                , "to the mempool."
                ]
            sendMempool $ MempoolMerkle action dmbs
            modify $ \s -> s{ merkleWindow = M.delete did win }
            dispatchMerkles
        -- Try to download more merkles if there is space in the window
        _ -> do
            continueMerkleDownload
            checkSynced

-- Check if merkle blocks are in sync with block headers.
checkSynced :: (HeaderTree m, MonadLogger m, MonadIO m)
            => StateT SpvSession m ()
checkSynced = do
    newWin   <- gets merkleWindow
    bestHead <- liftM nodeBlockHash $ runDB getBestBlockHeader
    winEnd   <- gets windowEnd
    -- If the window is empty after dispatching some merkles and trying
    -- to download some more, then the merkles have catched up with
    -- the headers.
    when (M.null newWin && winEnd == Just bestHead) $ do
        $(logDebug) $ format $ 
            "Merkle blocks are in sync with the block headers."
        sendMempool MempoolSynced

-- | If space is available in the merkle block download window, request
-- more merkle block download jobs and add them to the window.
continueMerkleDownload :: (HeaderTree m, MonadLogger m, MonadIO m) 
                       => StateT SpvSession m ()
continueMerkleDownload = do
    dwnM   <- gets windowEnd
    win    <- gets merkleWindow
    vBloom <- gets validBloom
    -- Download merkle blocks if the wallet asked us to start and if there
    -- is space left in the window and the bloom filter is valid.
    when (vBloom && isJust dwnM && M.size win < 10) $ do
        let dwn = fromJust dwnM
        -- Get a batch of blocks to download
        -- TODO: Add this value to a configuration (100)
        actionM <- runDB $ getNodeWindow dwn 100
        case actionM of
            -- Nothing to download
            Nothing -> $(logDebug) $ format 
                "No more merkle blocks available to download."
            -- A batch of merkle blocks is available for download
            Just action -> do
                -- Update the windowEnd pointer
                let nodes  = actionNewNodes action
                    winEnd = Just $ nodeBlockHash $ last nodes
                modify $ \s -> s{ windowEnd = winEnd }
                fcM <- gets fastCatchup 
                let fc = fromJust fcM
                    ts = map (blockTimestamp . nodeHeader) nodes
                -- Check if the batch is after the fast catchup time
                if isNothing fcM || any (>= fc) ts
                    then do
                        did <- liftIO newUnique
                        let job    = JobDwnMerkles did $ map nodeBlockHash nodes
                            height = nodeHeaderHeight $ last nodes
                        $(logDebug) $ format $ unwords
                            [ "Requesting download of merkle batch id"
                            , show $ hashUnique did
                            , "of length", show $ length nodes
                            , "up to height", show height
                            ]
                        -- Publish the job with low priority 10
                        sendManager $ PublishJob job (AnyPeer height) 10
                        -- Extend the window
                        modify $ \s -> 
                            s{ merkleWindow = M.insert did (action, []) win 
                            -- We reached the fast catchup. Set to Nothing.
                            , fastCatchup  = Nothing
                            }
                    else $(logDebug) $ format $ unwords
                        [ "Merkle block batch is before the fast catchup time."
                        , "Not downloading it."
                        ]
                -- Recurse with the windowEnd pointer updated
                continueMerkleDownload

processStartDownload :: (HeaderTree m, MonadLogger m, MonadIO m) 
                     => Either Timestamp BlockHash -> StateT SpvSession m ()
processStartDownload valE = do
    (fc, we) <- case valE of
        -- Set a fast catchup time and search from the genesis
        Left ts -> do
            $(logInfo) $ format $ unwords
                [ "Requesting merkle block download from timestamp", show ts ]
            return (Just ts, Just $ headerHash genesisHeader)
        -- No fast catchup time. Just download from the given block.
        Right h -> do
            $(logInfo) $ format $ unwords
                [ "Requesting merkle block download from block"
                , encodeBlockHashLE h
                ]
            return (Nothing, Just h)
    modify $ \s -> 
        s{ fastCatchup = fc
         , windowEnd = we
         -- Empty the window to ignore any old pending jobs
         , merkleWindow = M.empty
         }
    -- Trigger merkle block downloads
    continueMerkleDownload

processValidBloom :: (HeaderTree m, MonadLogger m, MonadIO m) 
                     => StateT SpvSession m ()
processValidBloom = do
    valid <- gets validBloom
    unless valid $ do
        $(logDebug) $ format $ unwords
            [ "Received a valid bloom filter."
            , "Attempting to start the merkle block download."
            ]
        modify $ \s -> s{ validBloom = True }
        continueMerkleDownload
    
{- Helpers -}

-- Add a BlockHash to the PeerId tickle map
setPeerTickle :: Monad m => PeerId -> BlockHash -> StateT SpvSession m ()
setPeerTickle pid bid = modify $ \s ->
    s{ peerTickles = M.insert pid bid $ peerTickles s }

-- Adjust height of peers that sent us a tickle for these blocks
adjustPeerHeight :: (MonadLogger m, MonadIO m) 
                 => BlockHeaderNode -> StateT SpvSession m ()
adjustPeerHeight node = do
    -- Find peers that have this block as a tickle
    (m, r) <- liftM (M.partition (== bid)) $ gets peerTickles 
    forM_ (M.keys m) $ \pid -> do
        $(logDebug) $ format $ unwords
            [ "Removing buffered tickle from peer", show $ hashUnique pid ]
        -- Update the height of the peer
        sendManager $ PeerHeight pid height
    -- Update the peer tickles
    modify $ \s -> s{ peerTickles = r }
  where
    bid    = nodeBlockHash node
    height = nodeHeaderHeight node

-- Send a message to the PeerManager
sendManager :: MonadIO m => ManagerMessage -> StateT SpvSession m ()
sendManager msg = do
    chan <- gets mngrChan
    liftIO . atomically $ writeTBMChan chan msg

-- Send a message to the mempool
sendMempool :: MonadIO m => MempoolMessage -> StateT SpvSession m ()
sendMempool msg = do
    chan <- gets mempChan
    liftIO . atomically $ writeTBMChan chan msg

runDB :: HeaderTree m => m a -> StateT SpvSession m a
runDB = lift

format :: String -> T.Text
format str = T.pack $ unwords [ "[Blockchain]", str ]

