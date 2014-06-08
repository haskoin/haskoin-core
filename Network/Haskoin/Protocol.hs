{-|
  This package provides all of the basic types used for the Bitcoin 
  networking protocol together with Data.Binary instances for efficiently
  serializing and de-serializing them. More information on the bitcoin protocol
  is available here: <http://en.bitcoin.it/wiki/Protocol_specification>
-}
module Network.Haskoin.Protocol
( 
  -- * Blocks
  Block(..)
, BlockLocator
, GetBlocks(..)

  -- * Block Headers
, BlockHeader(..)
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount
, blockid

  -- * Requesting data
, GetData(..)
, Inv(..)
, InvVector(..)
, InvType(..)
, NotFound(..)

  -- *Transactions
, Tx(..)
, txid
, cbid
, CoinbaseTx(..)
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, encodeTxid
, decodeTxid

  -- * Merkle trees and bloom filters
, MerkleBlock(..)

 -- * Bloom Filter
, BloomFlags(..)
, BloomFilter(..)
, FilterLoad(..)
, FilterAdd(..)

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

  -- *Messages
, Message(..)
, MessageHeader(..)
, MessageCommand(..)
) where

import Network.Haskoin.Protocol.Message
import Network.Haskoin.Protocol.Types

