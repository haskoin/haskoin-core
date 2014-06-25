{-|
  This package provides an implementation of a Bitcoin network node.
-}
module Network.Haskoin.Node
( 
 -- *Node
 startNode
, NodeEvent(..)
, NodeRequest(..)

-- *Checkpoints
, checkpoints
, checkpointsList
, verifyCheckpoint
) where

import Network.Haskoin.Node.Checkpoints
import Network.Haskoin.Node.PeerManager

