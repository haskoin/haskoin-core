{-# LANGUAGE DeriveDataTypeable #-}
module Network.Haskoin.Node.Types where

import Control.Exception (Exception)

import Data.Typeable (Typeable)
import Data.Word

import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol

type MerkleRoot = Word256

data RemoteHost = RemoteHost
    { remoteHost :: String
    , remotePort :: Int
    } deriving (Eq, Ord, Show, Read)

-- Data sent from peers to the central manager queue
data ManagerRequest
    = StartPeer RemoteHost
    | PeerHandshake RemoteHost Version
    | PeerDisconnect RemoteHost
    | PeerMerkleBlock RemoteHost DecodedMerkleBlock
    | PeerMessage RemoteHost Message   
    | UserRequest NodeRequest

-- User requests
data NodeRequest
    = BloomFilterUpdate BloomFilter 
    | PublishTx Tx
    | FastCatchupTime Word32
    deriving (Eq, Show)

data NodeEvent 
    = MerkleBlockEvent BlockChainAction [TxHash]
    | TxEvent Tx
    deriving (Eq, Show)

data DecodedMerkleBlock = DecodedMerkleBlock
    { decodedMerkle :: MerkleBlock
    , decodedRoot   :: MerkleRoot
    , expectedTxs   :: [TxHash]
    , merkleTxs     :: [Tx]
    }

data NodeException
    = NodeException String
    deriving (Eq, Read, Show, Typeable)

instance Exception NodeException

data BlockChainException 
    = BlockChainException String
    deriving (Eq, Read, Show, Typeable)

instance Exception BlockChainException

