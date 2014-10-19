{-|
  This package provides all of the basic types used for the Bitcoin 
  networking protocol together with Data.Binary instances for efficiently
  serializing and de-serializing them. More information on the bitcoin protocol
  is available here: <http://en.bitcoin.it/wiki/Protocol_specification>
-}
module Network.Haskoin.Protocol
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

  -- *Messages
, Message(..)
, MessageHeader(..)
, MessageCommand(..)
) where

import Network.Haskoin.Protocol.Message
import Network.Haskoin.Protocol.Types

