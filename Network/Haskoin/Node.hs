{-|
  This package provides basic types used for the Bitcoin networking protocol
  together with Data.Binary instances for efficiently serializing and
  de-serializing them. More information on the bitcoin protocol is available
  here: <http://en.bitcoin.it/wiki/Protocol_specification>
-}
module Network.Haskoin.Node
( 
  -- * Requesting data
  GetData(..)
, Inv(..)
, InvVector(..)
, InvType(..)
, NotFound(..)

  -- * Network types
, VarInt(..)
, VarString(..)
, NetworkAddress(..)
, Addr(..)
, NetworkAddressTime
, Version(..)
, Ping(..)
, Pong(..)
, Alert(..)
, Reject(..)
, RejectCode(..)
, reject

  -- * Messages
, Message(..)
, MessageHeader(..)
, MessageCommand(..)

  -- * Bloom filters
, BloomFlags(..)
, BloomFilter(..)
, FilterLoad(..)
, FilterAdd(..)
, bloomCreate
, bloomInsert
, bloomContains
, isBloomValid
, isBloomEmpty
, isBloomFull

  -- * Peer
, runPeer
, runCustomPeer
, peerLogStdout
, PeerMessage(..)
, DecodedMerkleBlock(..)
, RemoteHost(..)
, NodeException(..)

  -- * Peer Manager
, ManagerSession(..)
, PeerManager(..)
, PeerData(..)
, withAsyncNode
, sendMessage
, getPeerData
, putPeerData
, modifyPeerData
, deletePeerData
, peerExists
, getPeerKeys
, getPeerValues
, getPeers
, increasePeerHeight
, getBestPeerHeight

  -- * SPV Node
, SPVNode(..)
, SPVSession(..)
, SPVRequest( BloomFilterUpdate, PublishTx, NodeRescan )
, SPVData(..)
, withAsyncSPV
, processBloomFilter

) where

import Network.Haskoin.Node.Message
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.SPVNode

