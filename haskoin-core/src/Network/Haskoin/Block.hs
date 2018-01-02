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
, BlockHeader(..)
, headerHash
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount
, BlockHash(..)
, blockHashToHex
, hexToBlockHash

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

  -- * Difficulty Target
, decodeCompact
, encodeCompact
) where

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle

