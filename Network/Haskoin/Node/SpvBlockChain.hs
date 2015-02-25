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

import qualified Data.Text as T (pack)
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
import Data.Unique (hashUnique)
import qualified Data.Map as M 
    ( Map, member, delete, lookup, fromList, fromListWith
    , keys, elems, toList, toAscList, empty, map, filter, size
    , adjust, update, singleton, insertWith, insert, assocs
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
      -- Block hashes that a peer advertised to us but we haven't linked them
      -- yet to our chain. We use this list to update the peer height once
      -- those blocks are linked.
    , peerTickles :: !(M.Map BlockHash [PeerId])
      -- Continue downloading blocks from this point on. The merkle download
      -- process will be paused as long as this value is Nothing.
    , windowEnd :: !(Maybe BlockHash)
      -- Do not request merkle blocks with a timestamp before the
      -- fast catchup time.
    , fastCatchup :: !(Maybe Timestamp)
      -- Generate Ids for identifying merkle block jobs. When a rescan is
      -- triggered and jobs are still in the window, there is a possibility
      -- to have duplicate identical jobs at the peer manager level. We want
      -- to distinguish between them.
    , dwnId :: !DwnId
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
        let peerTickles    = M.empty
            windowEnd      = Nothing
            fastCatchup    = Nothing
            dwnId          = 0
            merkleWindow   = M.empty
            session        = SpvSession{..}
            -- Run the main blockchain message processing loop
            run = do
                -- Initialize Header Tree
                runDB initHeaderTree
                -- Trigger the header download
                height <- runDB bestBlockHeaderHeight
                headerSync (AnyPeer height) FullLocator Nothing
                -- Process messages
                sourceTBMChan bkchChan $$ processBlockChainMessage

        withAsync (evalStateT run session) $ \a -> 
            link a >> f bkchChan mngrChan

processBlockChainMessage :: (HeaderTree m, MonadLogger m, MonadIO m) 
                         => Sink BlockChainMessage (StateT SpvSession m) ()
processBlockChainMessage = awaitForever $ \req -> lift $ case req of
    BlockTickle pid bid      -> processBlockTickle pid bid
    IncHeaders pid bhs       -> processBlockHeaders pid bhs
    IncMerkleBlocks did dmbs -> processMerkleBlocks did dmbs
    StartDownload   valE     -> processStartDownload valE
    _ -> return () -- Ignore block invs (except tickles) and full blocks

processBlockTickle :: (HeaderTree m, MonadLogger m, MonadIO m) 
                   => PeerId -> BlockHash -> StateT SpvSession m ()
processBlockTickle pid bid = do
    $(logInfo) $ T.pack $ unwords
        [ "Got block tickle"
        , encodeBlockHashLE bid
        , "(", "Peer", show $ hashUnique pid, ")"
        ]
    
    nodeM <- runDB $ getBlockHeaderNode bid
    case nodeM of
        Just node -> do
            -- Update the height of the peer who sent us this tickle
            sendManager $ PeerHeight pid $ nodeHeaderHeight node
        Nothing -> do
            -- Save the PeerId/BlockHash so we can update the PeerId height
            -- when we can connect the block.
            addPeerTickle pid bid
            --Request headers so we can connect this block
            headerSync (ThisPeer pid) FullLocator $ Just bid

processBlockHeaders :: (HeaderTree m, MonadLogger m, MonadIO m) 
                    => PeerId -> [BlockHeader] -> StateT SpvSession m ()

processBlockHeaders pid [] = do
    -- TODO: Should we check the tickles to continue the header download ?
    $(logInfo) 
        "Received empty block headers. Header sync with this peer is complete."
    -- Try to download more merkle blocks
    continueMerkleDownload

processBlockHeaders pid bhs = do
    now <- liftM round $ liftIO getPOSIXTime
    -- Connect block headers and commit them
    actionE <- runDB $ connectHeaders bhs now True
    case actionE of
        Left err -> sendManager $ PeerMisbehaving pid severeDoS err
        Right action -> case action of
            -- Ignore SideChain/duplicate headers
            SideChain _ -> return () 
            -- Headers extend our current best head
            _ -> do
                let nodes  = actionNewNodes action
                    height = nodeHeaderHeight $ last nodes
                $(logInfo) $ T.pack $ unwords 
                    [ "Best header height:", show height ]
                -- Adjust height of peers that sent us a tickle for
                -- these blocks
                forM_ nodes adjustPeerHeight
                -- Adjust height of the node that sent us these headers
                sendManager $ PeerHeight pid height
                -- Continue syncing headers from a peer at the right height
                -- TODO: Schedule high priority AnyPeer on good scoring peers
                headerSync (AnyPeer height) PartialLocator Nothing
                -- Try to download more merkle blocks
                continueMerkleDownload

-- | Request a header download job for the given peer resource
headerSync :: (HeaderTree m, MonadLogger m, MonadIO m)
           => JobResource -> LocatorType -> Maybe BlockHash
           -> StateT SpvSession m ()
headerSync resource locType hStopM = do
    $(logInfo) $ T.pack $ unwords 
        [ "Requesting more BlockHeaders"
        , "[", show locType, "]"
        ]
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
    $(logError) "Got a completed merkle job with an empty merkle list"
    modify $ \s -> s{ merkleWindow = M.delete did $ merkleWindow s }
    continueMerkleDownload
processMerkleBlocks did dmbs = do
    win <- gets merkleWindow
    -- Find this specific job in the window
    case M.lookup did win of
        Just (action, []) -> do
            -- Add the merkles to the window
            modify $ \s -> s{ merkleWindow = M.insert did (action, dmbs) win }
            -- Try to send merkle blocks to the mempool
            dispatchMerkles
        -- This can happen if we had pending jobs when issuing a rescan. We
        -- simply ignore jobs from before the rescan.
        _ -> return ()

-- | Look for completed jobs at the start of the window and send them to the 
-- mempool.
dispatchMerkles :: (HeaderTree m, MonadLogger m, MonadIO m) 
                => StateT SpvSession m ()
dispatchMerkles = do
    win <- gets merkleWindow
    case M.assocs win of
        -- If the first batch in the window is ready, send it to the mempool
        ((did, (action, dmbs@(d:ds))):_) -> do
            sendMempool $ MempoolMerkle action dmbs
            modify $ \s -> s{ merkleWindow = M.delete did win }
            dispatchMerkles
        -- Try to download more merkles if there is space in the window
        _ -> continueMerkleDownload

-- | If space is available in the merkle block download window, request
-- more merkle block download jobs and add them to the window.
continueMerkleDownload :: (HeaderTree m, MonadLogger m, MonadIO m) 
                       => StateT SpvSession m ()
continueMerkleDownload = do
    dwnM <- gets windowEnd
    win  <- gets merkleWindow
    -- Download merkle blocks if the wallet asked us to start and if there
    -- is space left in the window
    when (isJust dwnM && M.size win < 10) $ do
        let dwn = fromJust dwnM
        -- Get a batch of blocks to download
        -- TODO: Add this value to a configuration (100)
        actionM <- runDB $ getNodeWindow dwn 100
        case actionM of
            -- Nothing to download
            Nothing -> return ()
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
                when (isNothing fcM || any (>= fc) ts) $ do
                    did <- nextId
                    let job    = JobDwnMerkles did $ map nodeBlockHash nodes
                        height = nodeHeaderHeight $ last nodes
                    -- Publish the job with low priority 10
                    sendManager $ PublishJob job (AnyPeer height) 10
                    -- Extend the window
                    modify $ \s -> 
                        s{ merkleWindow = M.insert did (action, []) win 
                         -- We reached the fast catchup. Set to Nothing.
                         , fastCatchup  = Nothing
                         }
                -- Recurse with the windowEnd pointer updated
                continueMerkleDownload

processStartDownload :: (HeaderTree m, MonadLogger m, MonadIO m) 
                     => Either Timestamp BlockHash -> StateT SpvSession m ()
processStartDownload valE = do
    $(logInfo) $ T.pack $ unwords
        [ "Initiating merkle block download from:" , show valE ]
    let (fc, we) = case valE of
            -- Set a fast catchup time and search from the genesis
            Left ts -> (Just ts, Just $ headerHash genesisHeader)
            -- No fast catchup time. Just download from the given block.
            Right h -> (Nothing, Just h)
    modify $ \s -> 
        s{ fastCatchup = fc
         , windowEnd = we
         -- Empty the window to ignore any old pending jobs
         , merkleWindow = M.empty
         }
    -- Trigger merkle block downloads
    continueMerkleDownload
    
{- Helpers -}

-- Add a BlockHash to the PeerId tickle map
addPeerTickle :: Monad m => PeerId -> BlockHash -> StateT SpvSession m ()
addPeerTickle pid bid = modify $ \s ->
    s{ peerTickles = M.insertWith (++) bid [pid] $ peerTickles s }

-- Adjust height of peers that sent us a tickle for these blocks
adjustPeerHeight :: MonadIO m => BlockHeaderNode -> StateT SpvSession m ()
adjustPeerHeight node = do
    pidsM <- liftM (M.lookup bid) $ gets peerTickles
    case pidsM of
        Just pids -> do
            forM_ (fromJust pidsM) $ \pid -> sendManager $ PeerHeight pid h
            modify $ \s -> s{ peerTickles = M.delete bid $ peerTickles s }
        Nothing -> return ()
  where
    bid = nodeBlockHash node
    h   = nodeHeaderHeight node

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

-- Simple counter from 0
nextId :: Monad m => StateT SpvSession m DwnId
nextId = do
    id <- gets dwnId
    modify $ \s -> s{ dwnId = id + 1 } 
    return id

runDB :: HeaderTree m => m a -> StateT SpvSession m a
runDB = lift
