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
import qualified Control.Monad.State as S (gets, modify)

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
    , keys, elems, toList, toAscList, empty, map, filter
    , adjust, update, singleton, insertWith
    )

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.Chan

type SpvHandle m = StateT SpvSession (HeaderTreeT m)

data SpvSession = SpvSession
    { -- Peer manager message channel
      mngrChan :: !(TBMChan ManagerMessage)
      -- Wallet message channel
    , wletChan :: !(TBMChan WalletMessage)
      -- This flag is set if the wallet triggered a rescan.
      -- The rescan can only be executed if no merkle block are still
      -- being downloaded.
    , pendingRescan :: !(Maybe Timestamp)
      -- Do not request merkle blocks with a timestamp before the
      -- fast catchup time.
    , fastCatchup :: !Timestamp
      -- Block hashes that a peer advertised to us but we haven't linked them
      -- yet to our chain. We use this list to update the peer height once
      -- those blocks are linked.
    , peerTickles :: !(M.Map BlockHash [PeerId])
      -- Merkle block window. Every element in the window corresponds to a
      -- merkle block download job. Completed jobs in the window are sent
      -- to the wallet in order.
    , merkleWindow :: ![(BlockChainAction, [DecodedMerkleBlock])]
      -- Continue downloading blocks from this point on
    , merkleDownload :: !BlockHash
    }

data LocatorType
    = FullLocator
    | PartialLocator
    deriving (Eq, Read, Show)

-- | Start the SpvBlockChain. This function will spin up a new thread and
-- return the BlockChain message channel to communicate with it.
withSpvBlockChain :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
                  => Timestamp
                  -> BlockHash
                  -> TBMChan WalletMessage
                  -> (TBMChan BlockChainMessage -> m ())
                  -> HeaderTreeT m ()
withSpvBlockChain fastCatchup merkleDownload wletChan f = do
    bkchChan <- liftIO $ atomically $ newTBMChan 10000
    withPeerManager bkchChan wletChan $ \mngrChan -> do
        let spvSyncPeer    = Nothing
            receivedMerkle = M.empty
            pendingRescan  = Nothing
            peerTickles    = M.empty
            merkleWindow   = []
            session        = SpvSession{..}
            -- Run the main blockchain message processing loop
            run = sourceTBMChan bkchChan $$ processBlockChainMessage

        -- Initialize Header Tree
        initHeaderTree

        withAsync (evalStateT run session) $ \a -> 
            link a >> lift (f bkchChan)

processBlockChainMessage :: (MonadLogger m, MonadIO m) 
                         => Sink BlockChainMessage (SpvHandle m) ()
processBlockChainMessage = awaitForever $ \req -> lift $ case req of
    BlockTickle pid bid      -> processBlockTickle pid bid
    IncHeaders pid bhs       -> processBlockHeaders pid bhs
    IncMerkleBlocks pid dmbs -> processMerkleBlocks pid dmbs
    _ -> return () -- Ignore block invs (except tickles) and full blocks

processBlockTickle :: (MonadLogger m, MonadIO m) 
                   => PeerId -> BlockHash -> SpvHandle m ()
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

processBlockHeaders :: (MonadLogger m, MonadIO m) 
                    => PeerId -> [BlockHeader] -> SpvHandle m ()
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
                downloadMerkles

-- | Request a header download job for the given peer resource
headerSync :: (MonadLogger m, MonadIO m)
           => JobResource -> LocatorType -> Maybe BlockHash
           -> SpvHandle m ()
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

processMerkleBlocks :: (MonadLogger m, MonadIO m)
                    => PeerId -> [DecodedMerkleBlock] -> SpvHandle m ()
processMerkleBlocks pid dmbs = do
    win <- gets merkleWindow
    -- Find which job in the window these merkles belong to
    let (ls, match) = break f win
    case match of
        (action, []):rs -> do
            -- Add the merkles to the window
            let newWin = ls ++ [(action, dmbs)] ++ rs
            modify $ \s -> s{ merkleWindow = newWin }
            -- Try to send merkle blocks to the wallet
            dispatchMerkles
        [] -> return () -- This shouldn't happen
  where
    bid = headerHash $ merkleHeader $ decodedMerkle $ head dmbs
    f   = (== bid) . nodeBlockHash . head . actionNewNodes . fst

dispatchMerkles :: (MonadLogger m, MonadIO m) => SpvHandle m ()
dispatchMerkles = do
    win <- gets merkleWindow
    case win of
        -- If the first batch in the window is ready, send it to the wallet
        ((action, dmbs@(d:ds)):rest) -> do
            sendWallet $ MerkleBlockEvent action dmbs
            modify $ \s -> s{ merkleWindow = rest }
            dispatchMerkles
        -- Try to download more merkles if there is space in the window
        _ -> downloadMerkles

-- | If space is available in the merkle block download window, request
-- more merkle block download jobs and add them to the window.
downloadMerkles :: (MonadLogger m, MonadIO m) => SpvHandle m ()
downloadMerkles = gets merkleWindow >>= \win -> when (length win < 10) $ do
    dwn <- gets merkleDownload
    -- Get a batch of blocks to download
    -- TODO: Add this value to a configuration (100)
    actionM <- runDB $ getNodeWindow dwn 100
    case actionM of
        -- Nothing to download
        Nothing -> return ()
        -- A batch of merkle blocks is available for download
        Just action -> do
            -- Update the merklDownload pointer
            let nodes = actionNewNodes action
            modify $ \s -> s{ merkleDownload = nodeBlockHash $ last nodes }
            -- Check if the batch is after the fast catchup time
            fc <- gets fastCatchup
            when (any (>= fc) $ map (blockTimestamp . nodeHeader) nodes) $ do
                let job    = JobDwnMerkles $ map nodeBlockHash nodes
                    height = nodeHeaderHeight $ last nodes
                -- Publish the job with low priority 10
                sendManager $ PublishJob job (AnyPeer height) 10
                -- Extend the window
                modify $ \s -> s{ merkleWindow = win ++ [(action, [])] }
            -- Recurse with the merkleDownload pointer updated
            downloadMerkles

{- Helpers -}

-- Add a BlockHash to the PeerId tickle map
addPeerTickle :: Monad m => PeerId -> BlockHash -> SpvHandle m ()
addPeerTickle pid bid = modify $ \s ->
    s{ peerTickles = M.insertWith (++) bid [pid] $ peerTickles s }

-- Adjust height of peers that sent us a tickle for these blocks
adjustPeerHeight :: MonadIO m => BlockHeaderNode -> SpvHandle m ()
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
sendManager :: MonadIO m => ManagerMessage -> SpvHandle m ()
sendManager msg = do
    mChan <- gets mngrChan
    liftIO . atomically $ writeTBMChan mChan msg

-- Send a message to the Wallet
sendWallet :: MonadIO m => WalletMessage -> SpvHandle m ()
sendWallet msg = do
    wChan <- gets wletChan
    liftIO . atomically $ writeTBMChan wChan msg

runDB :: Monad m => HeaderTreeT m a -> SpvHandle m a
runDB = lift

