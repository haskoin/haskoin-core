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

  -- *Scripts
  -- | More informations on scripts is available here:
  -- <http://en.bitcoin.it/wiki/Script>
, Script(..)
, ScriptOp(..)
, PushDataType(..)
, opPushData
, getScriptOps
, putScriptOps
, decodeScriptOps
, encodeScriptOps

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

  -- *Messages
, Message(..)
, MessageHeader(..)
, MessageCommand(..)
) where

import Network.Haskoin.Protocol.Message
import Network.Haskoin.Protocol.Addr
import Network.Haskoin.Protocol.Alert
import Network.Haskoin.Protocol.BlockHeader
import Network.Haskoin.Protocol.Block
import Network.Haskoin.Protocol.MerkleBlock
import Network.Haskoin.Protocol.GetBlocks
import Network.Haskoin.Protocol.GetData
import Network.Haskoin.Protocol.GetHeaders
import Network.Haskoin.Protocol.Headers
import Network.Haskoin.Protocol.Inv
import Network.Haskoin.Protocol.InvVector
import Network.Haskoin.Protocol.MessageHeader
import Network.Haskoin.Protocol.NetworkAddress
import Network.Haskoin.Protocol.NotFound
import Network.Haskoin.Protocol.Ping
import Network.Haskoin.Protocol.Script
import Network.Haskoin.Protocol.Tx
import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.VarString
import Network.Haskoin.Protocol.Version
import Network.Haskoin.Protocol.BloomFilter

