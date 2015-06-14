module Network.Haskoin.Node.Chan
( PeerMessage(..)
, ManagerMessage(..)
, BlockChainMessage(..)
, MempoolMessage(..)
, WalletMessage(..)
, NodeRequest(..)
, PeerId
, PeerJob(..)
, showJob
, JobResource(..)
, JobPriority
, Job(..)
, JobId
, DwnMerkleId
, DwnBlockId
, RemoteHost(..) 
, showRemoteHost
, DecodedMerkleBlock(..)
, Behavior(..)
, BehaviorUpdate
, minorDoS
, moderateDoS
, severeDoS
) where

import Data.Unique (Unique)

import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.Types
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto

type PeerId = Unique
type JobPriority = Int
type JobId = Unique
type DwnMerkleId = Unique
type DwnBlockId = Unique

data Job = Job
    { jobId       :: !JobId
    , jobPriority :: !JobPriority
    , jobResource :: !JobResource
    , jobPayload  :: !PeerJob
    }

data RemoteHost = RemoteHost
    { remoteHost :: !String
    , remotePort :: !Int
    } 
    deriving (Eq, Ord)

showRemoteHost :: RemoteHost -> String
showRemoteHost (RemoteHost h p) = concat [ h, ":", show p ]

data DecodedMerkleBlock = DecodedMerkleBlock
    { decodedMerkle :: !MerkleBlock
    , decodedRoot   :: !MerkleRoot
    , expectedTxs   :: ![TxHash]
    , merkleTxs     :: ![Tx]
    } deriving (Eq, Read, Show)

data JobResource
      -- Assign work to this specific peer only
    = ThisPeer !PeerId
      -- Assign work to the first available peer
    | AnyPeer !BlockHeight
      -- Assign work to all peers. 0 peers is acceptable.
    | AllPeers !BlockHeight 
      -- Assign work to all peers but at least 1 peer.
    | AllPeers1 !BlockHeight 

data PeerJob
    = JobSendBloomFilter !BloomFilter
    | JobSendTxInv !TxHash
    | JobSendTx !Tx
    | JobHeaderSync !BlockLocator !(Maybe BlockHash)
    | JobDwnTxs ![TxHash]
    | JobDwnBlocks !DwnBlockId ![BlockHash]
    | JobDwnMerkles !DwnMerkleId ![BlockHash]

showJob :: PeerJob -> String
showJob pJob = case pJob of
    JobSendBloomFilter _ -> "JobSendBloomFilter"
    JobSendTx _          -> "JobSendTx"
    JobSendTxInv _       -> "JobSendTxInv"
    JobHeaderSync _ _    -> "JobHeaderSync"
    JobDwnTxs _          -> "JobDwnTxs"
    JobDwnBlocks _ _     -> "JobDwnBlocks"
    JobDwnMerkles _ _    -> "JobDwnMerkles"

-- | Messages handled by a Peer actor
data PeerMessage
      -- Public Messages
    = AssignJob !Job
    | RetryJob
    | ClosePeer 
      -- Internal Messages
    | MsgFromRemote !Message

-- | Messages handled by the Peer manager actor
data ManagerMessage
      -- Public messages
    = AddRemoteHosts ![RemoteHost]
    | MngrBloomFilter !BloomFilter
    | PublishJob !PeerJob !JobResource !JobPriority 
    | PeerHeight !PeerId !BlockHeight
    | MngrStartDownload !(Either Timestamp BlockHash)
      -- Messages from Peers
    | PeerConnected !PeerId !Version
    | PeerClosed !PeerId 
    | PeerMisbehaving !PeerId !BehaviorUpdate !String
    | PeerJobDone !PeerId !JobId
      -- Internal Messages
    | ConnectToRemote !RemoteHost
    | MngrHeartbeat

-- | Messages handled by the Blockchain actor
data BlockChainMessage
    = BlockTickle !PeerId !BlockHash
    | BlockInv !PeerId ![BlockHash]
    | IncHeaders PeerId ![BlockHeader]
    | IncBlocks !DwnBlockId ![Block]
    | IncMerkleBlocks !DwnMerkleId ![DecodedMerkleBlock]
    | StartMerkleDownload !(Either Timestamp BlockHash)
    | StartBlockDownload !(Either Timestamp BlockHash)
    | SetBloomFilter !BloomFilter
    | NetworkHeight !BlockHeight
    | BkchHeartbeat

-- | Messages handled by the Mempool actor
data MempoolMessage
    = MempoolTxInv !PeerId ![TxHash]
    | MempoolTx !Tx
    | MempoolSendTx !Tx
    | MempoolGetTx !PeerId !TxHash
    | MempoolMerkles !BlockChainAction ![DecodedMerkleBlock]
    | MempoolBlocks !BlockChainAction ![Block]
    | MempoolStartDownload !(Either Timestamp BlockHash)
    | MempoolSynced

-- | Node events sent to the wallet
data WalletMessage
    = WalletTx !Tx
    | WalletBlocks !BlockChainAction ![Block]
    | WalletMerkles !BlockChainAction ![DecodedMerkleBlock]
    | WalletGetTx !TxHash
    | WalletSynced

-- | Requests from the wallet to the node
data NodeRequest
    = NodeBloomFilter !BloomFilter
    | NodeSendTx !Tx
    | NodePublishTx !TxHash
    | NodeStartMerkleDownload !(Either Timestamp BlockHash)
    | NodeStartBlockDownload !(Either Timestamp BlockHash)
    | NodeConnectPeers ![RemoteHost]

-- | Describes the behavior of a remote peer
data Behavior
    = GoodBehavior
    | Misbehaving !Int
    | Banned
    deriving (Eq, Show, Read)

type BehaviorUpdate = Behavior -> Behavior

misbehaving :: Int -> BehaviorUpdate
misbehaving n behavior = case behavior of
    GoodBehavior  -> if n   >= banThreshold then Banned else Misbehaving n
    Misbehaving i -> if n+i >= banThreshold then Banned else Misbehaving (n+i)
    Banned        -> Banned
  where
    banThreshold = 100

minorDoS :: BehaviorUpdate 
minorDoS = misbehaving 1

moderateDoS :: BehaviorUpdate 
moderateDoS = misbehaving 10

severeDoS :: BehaviorUpdate 
severeDoS = misbehaving 100

