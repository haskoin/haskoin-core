{-|
  This package provides an implementation of a Bitcoin network node.
-}
module Network.Haskoin.Node
( 
  -- *Node
  withAsyncNode
, NodeEvent(..)
, NodeRequest(..)

  -- *Checkpoints
, checkpointMap
, checkpointList
, verifyCheckpoint
) where

import Network.Haskoin.Node.Checkpoints
import Network.Haskoin.Node.PeerManager

