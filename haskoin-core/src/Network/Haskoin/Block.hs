{-|
  This package provides block and block-related types.
-}
module Network.Haskoin.Block
(
  -- * Blocks
  Block(..)
, Timestamp

  -- * Block Headers
, BlockHeader(..)
, headerHash
, createBlockHeader
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount
, BlockHash(..)
, isValidPOW
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
, merkleBlockTxs
, testMerkleRoot

  -- * Difficulty Target
, decodeCompact
, encodeCompact

  -- * Block Chain
, BlockMap
, BlockLocator
, GetBlocks(..)
, BlockWork
, BlockHeight
, BlockNode(..)
, BlockHeaders(..)
, HeaderMemory(..)
, MinWork
, ShortBlockHash
, isGenesis
, shortBlockHash
, initialChain
, addBlockToMap
, getAncestor
, connectBlock
, connectBlocks
, blockLocator
, blockLocatorNodes
, chooseBest
, mineBlock
, appendBlocks
, addBlockHeaderMemory
, getBlockHeaderMemory
, splitPoint

  -- * Genesis
, genesisBlock
, genesisNode
, genesisMap
) where

import           Network.Haskoin.Block.Genesis
import           Network.Haskoin.Block.Merkle
import           Network.Haskoin.Block.Types
import           Network.Haskoin.Block.Headers
