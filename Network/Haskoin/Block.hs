{-|
  This package provides block and block-related types.
-}
module Network.Haskoin.Block
( 
  -- * Blocks
  Block(..)
, BlockLocator
, GetBlocks(..)

  -- * Block Headers
, headerHash
, BlockHeader(..)
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount

  -- * Merkle Blocks
, MerkleBlock(..)
, MerkleRoot
, FlagBits
, PartialMerkleTree
, calcTreeHeight
, calcTreeWidth
, buildMerkleRoot
, calcHash
, buildPartialMerkle
, extractMatches

  -- * HeaderTree
, BlockHeight
, Timestamp
, HeaderTree(..)
, BlockHeaderNode(..)
, BlockChainAction(..)
, actionNewNodes
, bestBlockHeaderHeight
, getBlockHeaderHeight
, genesisNode
, getParentNode
, initHeaderTree
, connectHeader
, connectHeaders
, commitAction
, blockLocator
, getNodeWindow

  -- * Checkpoints
, checkpointMap
, checkpointList
, verifyCheckpoint

) where

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle
import Network.Haskoin.Block.HeaderTree
import Network.Haskoin.Block.Checkpoints

