{-|
  This package provides an implementation of a Bitcoin SPV node.
-}
module Network.Haskoin.SPV
( 
  -- *Node
  withAsyncNode
, NodeEvent(..)
, NodeRequest(..)
) where

import Network.Haskoin.SPV.PeerManager

