{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.STM where

import Control.Monad (liftM, (<=<))
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (MonadLogger, logDebug)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.Lock (Lock)
import Control.DeepSeq (NFData(..))
import qualified Control.Concurrent.STM.Lock as Lock (new)
import Control.Concurrent.STM.TBMChan
    (TBMChan, closeTBMChan, newTBMChan)
import Control.Exception.Lifted
    (SomeException, Exception, fromException, throw, catch)
import Control.Concurrent.STM
    ( STM, TMVar, TVar, atomically, orElse
    , readTVar, writeTVar, newTVarIO, modifyTVar', newTVar
    , tryPutTMVar, takeTMVar, newEmptyTMVarIO, isEmptyTMVar, putTMVar
    , tryReadTMVar, readTMVar
    )

import Data.Time.Clock (NominalDiffTime)
import Data.Unique (Unique, hashUnique)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import Data.Maybe (isJust)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Map.Strict as M (Map, insert, lookup, delete, empty)

import qualified Database.LevelDB.Base as L (Options(..), withDB)

import Network.Haskoin.Util
import Network.Haskoin.Node
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Constants
import Network.Haskoin.Node.HeaderTree

{- Type aliases -}

type MerkleTxs = [TxHash]
type NodeT = ReaderT SharedNodeState
type PeerId = Unique
type PeerHostScore = Word32

instance Show PeerId where
    show = show . hashUnique

getNodeState :: (MonadIO m, MonadLogger m)
             => FilePath
             -> L.Options
             -> m SharedNodeState
getNodeState levelDBFilePath levelDBOptions = do
    -- Initialize the HeaderTree
    $(logDebug) "Initializing the HeaderTree and NodeState"
    liftIO $ do
        L.withDB levelDBFilePath levelDBOptions $ runReaderT initHeaderTree
        sharedPeerMap       <- newTVarIO M.empty
        sharedHostMap       <- newTVarIO M.empty
        sharedNetworkHeight <- newTVarIO 0
        sharedHeaders       <- newEmptyTMVarIO
        sharedHeaderPeer    <- newTVarIO Nothing
        sharedMerklePeer    <- newTVarIO Nothing
        sharedSyncLock      <- atomically Lock.new
        sharedTickleChan    <- atomically $ newTBMChan 1024
        sharedTxChan        <- atomically $ newTBMChan 1024
        sharedTxGetData     <- newTVarIO M.empty
        sharedRescan        <- newEmptyTMVarIO
        sharedMempool       <- newTVarIO False
        sharedLevelDBLock   <- atomically Lock.new
        sharedBloomFilter   <- newTVarIO Nothing
        -- Find our best node in the HeaderTree
        sharedBestHeader    <- newTVarIO =<< L.withDB
                                                levelDBFilePath
                                                levelDBOptions
                                                (runReaderT getBestBlockHeader)
        sharedBestBlock     <- newTVarIO $ headerHash genesisHeader
        return SharedNodeState{..}

runNodeT :: Monad m => SharedNodeState -> NodeT m a -> m a
runNodeT state action = runReaderT action state

withNodeT :: (MonadIO m, MonadLogger m)
          => FilePath
          -> L.Options
          -> NodeT m a
          -> m a
withNodeT fp opts action = flip runNodeT action =<< getNodeState fp opts

atomicallyNodeT :: MonadIO m => NodeT STM a -> NodeT m a
atomicallyNodeT action = liftIO . atomically . runReaderT action =<< ask

{- PeerHost Session -}

data PeerHostSession = PeerHostSession
    { peerHostSessionScore     :: !PeerHostScore
    , peerHostSessionReconnect :: !Int
    }

instance NFData PeerHostSession where
    rnf PeerHostSession{..} =
        rnf peerHostSessionScore `seq`
        rnf peerHostSessionReconnect

{- Shared Peer STM Type -}

data SharedNodeState = SharedNodeState
    { sharedPeerMap :: !(TVar (M.Map PeerId (TVar PeerSession)))
      -- ^ Map of all active peers and their sessions
    , sharedHostMap :: !(TVar (M.Map PeerHost (TVar PeerHostSession)))
      -- ^ The peer that is currently syncing the block headers
    , sharedNetworkHeight :: !(TVar BlockHeight)
      -- ^ The current height of the network
    , sharedHeaders :: !(TMVar (PeerId, Headers))
      -- ^ Block headers sent from a peer
    , sharedHeaderPeer :: !(TVar (Maybe PeerId))
      -- ^ Peer currently syncing headers
    , sharedMerklePeer :: !(TVar (Maybe PeerId))
      -- ^ Peer currently downloading merkle blocks
    , sharedSyncLock :: !Lock
      -- ^ Lock on the header syncing process
    , sharedLevelDBLock :: !Lock
      -- ^ LevelDB lock
    , sharedBestHeader :: !(TVar BlockHeaderNode)
      -- ^ Our best block header
    , sharedBestBlock :: !(TVar BlockHash)
      -- ^ Our best merkle block
    , sharedTxGetData :: !(TVar (M.Map TxHash [(PeerId, PeerHost)]))
      -- ^ List of Tx GetData requests
    , sharedBloomFilter :: !(TVar (Maybe (BloomFilter, Int)))
      -- ^ Bloom filter
    , sharedTickleChan :: !(TBMChan (PeerId, PeerHost, BlockHash))
      -- ^ Channel containing all the block tickles received from peers
    , sharedTxChan :: !(TBMChan (PeerId, PeerHost, Tx))
      -- ^ Transaction channel
    , sharedRescan :: !(TMVar (Either Timestamp BlockHeight))
      -- ^ Rescan requests from a timestamp or from a block height
    , sharedMempool :: !(TVar Bool)
      -- ^ Did we do a Mempool sync ?
    , levelDBFilePath :: !FilePath
      -- ^ LevelDB FilePath
    , levelDBOptions :: !L.Options
      -- ^ LevelDB Options
    }

{- Peer Data -}

type PingNonce = Word64

-- Data stored about a peer
data PeerSession = PeerSession
    { peerSessionConnected    :: !Bool
      -- ^ True if the peer is connected (completed the handshake)
    , peerSessionVersion      :: !(Maybe Version)
      -- ^ Contains the version message that we received from the peer
    , peerSessionHeight       :: !BlockHeight
      -- ^ Current known height of the peer
    , peerSessionChan         :: !(TBMChan Message)
      -- ^ Message channel to send messages to the peer
    , peerSessionHost         :: !PeerHost
      -- ^ Host to which this peer is connected
    , peerSessionThreadId     :: !ThreadId
      -- ^ Peer ThreadId
    , peerSessionMerkleChan   :: !(TBMChan (Either (MerkleBlock, MerkleTxs) Tx))
      -- ^ Merkle block/Merkle transaction channel
    , peerSessionPings        :: !(TVar [PingNonce])
      -- ^ Time at which we requested pings
    , peerSessionScore        :: !(Maybe NominalDiffTime)
      -- ^ Ping scores for this peer (round trip times)
    }

instance NFData PeerSession where
    rnf PeerSession{..} =
        rnf peerSessionConnected `seq`
        rnf peerSessionVersion `seq`
        rnf peerSessionHeight `seq`
        peerSessionChan `seq`
        rnf peerSessionHost `seq`
        peerSessionThreadId `seq` ()

{- Peer Hosts -}

data PeerHost = PeerHost
    { peerHost :: !String
    , peerPort :: !Int
    }
    deriving (Eq, Ord)

$(deriveJSON (dropFieldLabel 4) ''PeerHost)

peerHostString :: PeerHost -> String
peerHostString PeerHost{..} = concat [ peerHost, ":", show peerPort ]

instance NFData PeerHost where
    rnf PeerHost{..} =
        rnf peerHost `seq`
        rnf peerPort

{- Node Status -}

data PeerStatus = PeerStatus
    -- Regular fields
    { peerStatusPeerId         :: !Int
    , peerStatusHost           :: !PeerHost
    , peerStatusConnected      :: !Bool
    , peerStatusHeight         :: !BlockHeight
    , peerStatusProtocol       :: !(Maybe Word32)
    , peerStatusUserAgent      :: !(Maybe String)
    , peerStatusPing           :: !(Maybe String)
    , peerStatusDoSScore       :: !(Maybe PeerHostScore)
    -- Debug fields
    , peerStatusThreadId       :: !String
    , peerStatusHaveMerkles    :: !Bool
    , peerStatusHaveMessage    :: !Bool
    , peerStatusPingNonces     :: ![PingNonce]
    , peerStatusReconnectTimer :: !(Maybe Int)
    }

$(deriveJSON (dropFieldLabel 10) ''PeerStatus)

data NodeStatus = NodeStatus
    -- Regular fields
    { nodeStatusPeers            :: ![PeerStatus]
    , nodeStatusNetworkHeight    :: !BlockHeight
    , nodeStatusBestHeader       :: !BlockHash
    , nodeStatusBestHeaderHeight :: !BlockHeight
    , nodeStatusBestBlock        :: !BlockHash
    , nodeStatusBloomSize        :: !Int
    -- Debug fields
    , nodeStatusHeaderPeer       :: !(Maybe Int)
    , nodeStatusMerklePeer       :: !(Maybe Int)
    , nodeStatusHaveHeaders      :: !Bool
    , nodeStatusHaveTickles      :: !Bool
    , nodeStatusHaveTxs          :: !Bool
    , nodeStatusGetData          :: ![TxHash]
    , nodeStatusRescan           :: !(Maybe (Either Timestamp BlockHeight))
    , nodeStatusMempool          :: !Bool
    , nodeStatusSyncLock         :: !Bool
    , nodeStatusLevelDBLock      :: !Bool
    }

$(deriveJSON (dropFieldLabel 10) ''NodeStatus)

{- Getters / Setters -}

tryGetPeerSession :: PeerId -> NodeT STM (Maybe PeerSession)
tryGetPeerSession pid = do
    peerMap <- readTVarS sharedPeerMap
    case M.lookup pid peerMap of
        Just sessTVar -> liftM Just $ lift $ readTVar sessTVar
        _ -> return Nothing

getPeerSession :: PeerId -> NodeT STM PeerSession
getPeerSession pid = do
    sessM <- tryGetPeerSession pid
    case sessM of
        Just sess -> return sess
        _ -> throw $ NodeExceptionInvalidPeer pid

newPeerSession :: PeerId -> PeerSession -> NodeT STM ()
newPeerSession pid sess = do
    peerMapTVar <- asks sharedPeerMap
    peerMap <- lift $ readTVar peerMapTVar
    case M.lookup pid peerMap of
        Just _ -> return ()
        Nothing -> do
            sessTVar <- lift $ newTVar sess
            let newMap = M.insert pid sessTVar peerMap
            lift $ writeTVar peerMapTVar $! newMap

modifyPeerSession :: PeerId -> (PeerSession -> PeerSession) -> NodeT STM ()
modifyPeerSession pid f = do
    peerMap <- readTVarS sharedPeerMap
    case M.lookup pid peerMap of
        Just sessTVar -> lift $ modifyTVar' sessTVar f
        _ -> return ()

removePeerSession :: PeerId -> NodeT STM (Maybe PeerSession)
removePeerSession pid = do
    peerMapTVar <- asks sharedPeerMap
    peerMap <- lift $ readTVar peerMapTVar
    -- Close the peer TBMChan
    sessM <- case M.lookup pid peerMap of
        Just sessTVar -> lift $ do
            sess@PeerSession{..} <- readTVar sessTVar
            closeTBMChan peerSessionChan
            return $ Just sess
        _ -> return Nothing
    -- Remove the peer from the peerMap
    let newMap = M.delete pid peerMap
    lift $ writeTVar peerMapTVar $! newMap
    return sessM

getHostSession :: PeerHost
               -> NodeT STM (Maybe PeerHostSession)
getHostSession ph = do
    hostMap <- readTVarS sharedHostMap
    lift $ case M.lookup ph hostMap of
        Just hostSessionTVar -> liftM Just $ readTVar hostSessionTVar
        _ -> return Nothing

modifyHostSession :: PeerHost
                  -> (PeerHostSession -> PeerHostSession)
                  -> NodeT STM ()
modifyHostSession ph f = do
    hostMap <- readTVarS sharedHostMap
    case M.lookup ph hostMap of
        Just hostSessionTVar -> lift $ modifyTVar' hostSessionTVar f
        _ -> newHostSession ph $!
            f PeerHostSession { peerHostSessionScore = 0
                              , peerHostSessionReconnect = 1
                              }

newHostSession :: PeerHost -> PeerHostSession -> NodeT STM ()
newHostSession ph session = do
    hostMapTVar <- asks sharedHostMap
    hostMap <- lift $ readTVar hostMapTVar
    case M.lookup ph hostMap of
        Just _ -> return ()
        Nothing -> lift $ do
            hostSessionTVar <- newTVar session
            let newHostMap = M.insert ph hostSessionTVar hostMap
            writeTVar hostMapTVar $! newHostMap

{- Host DOS Scores -}

bannedScore :: PeerHostScore
bannedScore = 100

minorDoS :: PeerHostScore -> PeerHostScore
minorDoS = (+ 1)

moderateDoS :: PeerHostScore -> PeerHostScore
moderateDoS = (+ 10)

severeDoS :: PeerHostScore -> PeerHostScore
severeDoS = (+ bannedScore)

isHostScoreBanned :: PeerHostScore -> Bool
isHostScoreBanned = (>= bannedScore)

{- STM Utilities -}

orElseNodeT :: NodeT STM a -> NodeT STM a -> NodeT STM a
orElseNodeT a b = do
    s <- ask
    lift $ (runNodeT s a) `orElse` (runNodeT s b)

{- TVar Utilities -}

readTVarS :: (SharedNodeState -> TVar a) -> NodeT STM a
readTVarS = lift . readTVar <=< asks

writeTVarS :: (SharedNodeState -> TVar a) -> a -> NodeT STM ()
writeTVarS f val = lift . (flip writeTVar val) =<< asks f

{- TMVar Utilities -}

takeTMVarS :: (SharedNodeState -> TMVar a) -> NodeT STM a
takeTMVarS = lift . takeTMVar <=< asks

readTMVarS :: (SharedNodeState -> TMVar a) -> NodeT STM a
readTMVarS = lift . readTMVar <=< asks

tryReadTMVarS :: (SharedNodeState -> TMVar a) -> NodeT STM (Maybe a)
tryReadTMVarS = lift . tryReadTMVar <=< asks

putTMVarS :: (SharedNodeState -> TMVar a) -> a -> NodeT STM ()
putTMVarS f val = lift . (flip putTMVar val) =<< asks f

tryPutTMVarS :: (SharedNodeState -> TMVar a) -> a -> NodeT STM Bool
tryPutTMVarS f val = lift . (flip tryPutTMVar val) =<< asks f

swapTMVarS :: (SharedNodeState -> TMVar a) -> a -> NodeT STM ()
swapTMVarS f val = lift . (flip putTMVar val) =<< asks f

isEmptyTMVarS :: (SharedNodeState -> TMVar a) -> NodeT STM Bool
isEmptyTMVarS f = lift . isEmptyTMVar =<< asks f

data NodeException
    = NodeExceptionBanned
    | NodeExceptionConnected
    | NodeExceptionInvalidPeer !PeerId
    | NodeExceptionPeerNotConnected !PeerId
    | NodeException !String
    deriving (Show, Typeable)

instance Exception NodeException

isNodeException :: SomeException -> Bool
isNodeException se = isJust (fromException se :: Maybe NodeException)

catchAny :: MonadBaseControl IO m
         => m a -> (SomeException -> m a) -> m a
catchAny = catch

catchAny_ :: MonadBaseControl IO m
          => m () -> m ()
catchAny_ = flip catchAny $ \_ -> return ()

