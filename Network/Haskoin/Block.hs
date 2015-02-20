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
, HeaderTree
, HeaderTreeT
, BlockHeaderNode(..)
, BlockChainAction(..)
, actionNewNodes
, BlockHeight
, Timestamp
, getBlockHeaderNode
, getBestBlockHeader
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

