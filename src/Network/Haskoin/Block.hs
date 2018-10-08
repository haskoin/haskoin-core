{-|
Module      : Network.Haskoin.Block
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Most functions relating to blocks are exported by this module.
-}
module Network.Haskoin.Block
    ( module Network.Haskoin.Block.Common
      -- * Block Header Chain
    , BlockWork
    , BlockHeaders(..)
    , BlockNode(..)
    , HeaderMemory(..)
    , BlockMap
    , getAncestor
    , isGenesis
    , initialChain
    , genesisMap
    , genesisNode
    , genesisBlock
    , connectBlocks
    , connectBlock
    , parentBlock
    , splitPoint
    , blockLocator
      -- * Merkle Blocks
    , MerkleBlock(..)
    , MerkleRoot
    , FlagBits
    , PartialMerkleTree
    , buildMerkleRoot
    , buildPartialMerkle
    , merkleBlockTxs
    , testMerkleRoot
    ) where

import           Network.Haskoin.Block.Headers
import           Network.Haskoin.Block.Merkle
import           Network.Haskoin.Block.Common
