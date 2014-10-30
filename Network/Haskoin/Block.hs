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
, calcTreeHeight
, calcTreeWidth
, buildMerkleRoot
, calcHash
, buildPartialMerkle
, extractMatches

  -- * HeaderChain
, BlockHeaderNode(..)
, BlockHeaderStore(..)
, BlockHeaderAction(..)
, BlockChainAction(..)
, getActionNode
, initHeaderChain
, connectBlockHeader
, connectBlock
, downloadBlockHeaders
, blockLocator
, rescanFrom
, bestBlockHeaderHeight
, bestBlockHeight
, lastSeenCheckpoint
, findSplitNode
, getParentNode
, nextWorkRequired
, workFromInterval
, isValidPOW
, headerPOW
, headerWork

  -- * Checkpoints
, checkpointMap
, checkpointList
, verifyCheckpoint

) where

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle
import Network.Haskoin.Block.HeaderChain
import Network.Haskoin.Block.Checkpoints

