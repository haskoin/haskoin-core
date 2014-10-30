{-# LANGUAGE DeriveDataTypeable #-}
module Network.Haskoin.SPV.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)

import Data.Binary (Binary, get, put)

import Data.Typeable (Typeable)
import Data.Word (Word32)

import Network.Haskoin.SPV.LevelDBChain
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Block
import Network.Haskoin.Node

type MerkleRoot = Word256
type BlockHeight = Word32
type Timestamp   = Word32

data RemoteHost = RemoteHost
    { remoteHost :: String
    , remotePort :: Int
    } deriving (Eq, Ord, Show, Read)

instance Binary RemoteHost where
    get = RemoteHost <$> get <*> get
    put (RemoteHost h p) = put h >> put p

-- Data sent from peers to the central manager queue
data ManagerRequest
    = StartPeer RemoteHost
    | PeerHandshake RemoteHost Version
    | PeerDisconnect RemoteHost
    | PeerMerkleBlock RemoteHost DecodedMerkleBlock
    | PeerMessage RemoteHost Message   
    | UserRequest NodeRequest
    | MonitorRequests

-- User requests
data NodeRequest
    = BloomFilterUpdate BloomFilter 
    | PublishTx Tx
    | NodeRescan Timestamp
    | ConnectNode String Int
    deriving (Eq, Show)

data NodeEvent 
    = MerkleBlockEvent [(BlockChainAction, [TxHash])]
    | TxEvent [Tx]
    deriving (Eq, Show)

data DecodedMerkleBlock = DecodedMerkleBlock
    { decodedMerkle :: MerkleBlock
    , decodedRoot   :: MerkleRoot
    , expectedTxs   :: [TxHash]
    , merkleTxs     :: [Tx]
    } deriving (Eq, Read, Show)

data NodeException
    = NodeException String
    deriving (Eq, Read, Show, Typeable)

instance Exception NodeException

data BlockChainException 
    = BlockChainException String
    deriving (Eq, Read, Show, Typeable)

instance Exception BlockChainException

