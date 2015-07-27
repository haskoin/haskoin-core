{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Node.Actors.Types
( PeerMessage(..)
, ManagerMessage(..)
, BlockChainMessage(..)
, TxManagerMessage(..)
, WalletMessage(..)
, NodeRequest(..)
, PeerId
, PeerJob(..)
, showJob
, JobResource(..)
, showJobResource
, JobPriority
, Job(..)
, JobId
, DwnMerkleId
, DwnBlockId
, RemoteHost(..) 
, showRemoteHost
, MerkleTxs
, Behavior(..)
, BehaviorUpdate
, minorDoS
, moderateDoS
, severeDoS
) where

import Data.Unique (Unique, hashUnique)
import Control.DeepSeq (NFData(..))

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

instance NFData Unique where
    rnf u = u `seq` ()

data Job = Job
    { jobId       :: !JobId
    , jobPriority :: !JobPriority
    , jobResource :: !JobResource
    , jobPayload  :: !PeerJob
    }

instance NFData Job where
    rnf Job{..} = 
        rnf jobId `seq` 
        rnf jobPriority `seq`
        rnf jobResource `seq`
        rnf jobPayload

data RemoteHost = RemoteHost
    { remoteHost :: !String
    , remotePort :: !Int
    } 
    deriving (Eq, Ord)

instance NFData RemoteHost where
    rnf RemoteHost{..} = 
        rnf remoteHost `seq` 
        rnf remotePort

showRemoteHost :: RemoteHost -> String
showRemoteHost (RemoteHost h p) = concat [ h, ":", show p ]

type MerkleTxs = [TxHash]

data JobResource
      -- Assign work to this specific peer only
    = ThisPeer !PeerId
      -- Assign work to the first available peer
    | AnyPeer !BlockHeight
      -- Assign work to all peers. 0 peers is acceptable.
    | AllPeers !BlockHeight 
      -- Assign work to all peers but at least 1 peer.
    | AllPeers1 !BlockHeight 

instance NFData JobResource where
    rnf jr = case jr of
        ThisPeer pid -> rnf pid
        AnyPeer h    -> rnf h
        AllPeers h   -> rnf h
        AllPeers1 h  -> rnf h

showJobResource :: JobResource -> String
showJobResource r = case r of
    ThisPeer p  -> unwords [ "ThisPeer", show $ hashUnique p]
    AnyPeer h   -> unwords [ "AnyPeers", show h ]
    AllPeers h  -> unwords [ "AnyPeers", show h ]
    AllPeers1 h -> unwords [ "AnyPeers", show h ]

data PeerJob
    = JobSendBloomFilter !BloomFilter
    | JobSendTxInv ![TxHash]
    | JobSendTx !Tx
    | JobHeaderSync !BlockLocator !(Maybe BlockHash)
    | JobDwnTxs ![TxHash]
    | JobDwnBlocks !DwnBlockId ![BlockHash]
    | JobDwnMerkles !DwnMerkleId ![BlockHash]
    | JobMempool
    | JobStatus

instance NFData PeerJob where
    rnf pj = case pj of
        JobSendBloomFilter bf -> rnf bf
        JobSendTxInv hs -> rnf hs
        JobSendTx tx -> rnf tx
        JobHeaderSync bl bM -> rnf bl `seq` rnf bM
        JobDwnTxs hs -> rnf hs
        JobDwnBlocks did bs -> rnf did `seq` rnf bs
        JobDwnMerkles did bs -> rnf did `seq` rnf bs
        JobMempool -> ()
        JobStatus -> ()

showJob :: PeerJob -> String
showJob pJob = case pJob of
    JobSendBloomFilter _ -> "JobSendBloomFilter"
    JobSendTx _          -> "JobSendTx"
    JobSendTxInv _       -> "JobSendTxInv"
    JobHeaderSync _ _    -> "JobHeaderSync"
    JobDwnTxs _          -> "JobDwnTxs"
    JobDwnBlocks _ _     -> "JobDwnBlocks"
    JobDwnMerkles _ _    -> "JobDwnMerkles"
    JobMempool           -> "JobMempool"
    JobStatus            -> "JobStatus"

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
    | MngrStatus

-- | Messages handled by the Blockchain actor
data BlockChainMessage
    = BlockTickle !PeerId !BlockHash
    | BlockInv !PeerId ![BlockHash]
    | IncHeaders PeerId ![BlockHeader]
    | IncBlocks !DwnBlockId ![Block]
    | IncMerkleBatch !DwnMerkleId ![MerkleTxs]
    | StartMerkleDownload !(Either Timestamp BlockHash)
    | StartBlockDownload !(Either Timestamp BlockHash)
    | SetBloomFilter !BloomFilter
    | NetworkHeight !BlockHeight
    | SetBatchSize !Int
    | BkchHeartbeat
    | BkchStatus

-- | Messages handled by the TxManager actor
data TxManagerMessage
    = TxManagerTxInv !PeerId ![TxHash]
    | TxManagerTx !Tx !Bool
    | TxManagerSendTx !Tx
    | TxManagerGetTx !PeerId !TxHash
    | TxManagerMerkles !BlockChainAction ![MerkleTxs]
    | TxManagerBlocks !BlockChainAction ![Block]
    | TxManagerStartDownload !(Either Timestamp BlockHash)
    | TxManagerSynced
    | TxManagerStatus

-- | Node events sent to the wallet
data WalletMessage
    = WalletTx !Tx !Bool
    | WalletBlocks !BlockChainAction ![Block]
    | WalletMerkles !BlockChainAction ![MerkleTxs]
    | WalletGetTx !TxHash
    | WalletSynced

-- | Requests from the wallet to the node
data NodeRequest
    = NodeBloomFilter !BloomFilter
    | NodeSendTx !Tx
    | NodePublishTxs ![TxHash]
    | NodeStartMerkleDownload !(Either Timestamp BlockHash)
    | NodeStartBlockDownload !(Either Timestamp BlockHash)
    | NodeConnectPeers ![RemoteHost]
    | NodeBatchSize !Int
    | NodeStatus

-- | Describes the behavior of a remote peer
data Behavior
    = GoodBehavior
    | Misbehaving !Int
    | Banned
    deriving (Eq, Show, Read)

instance NFData Behavior

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

