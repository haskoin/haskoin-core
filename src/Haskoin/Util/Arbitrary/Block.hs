{- |
Module      : Haskoin.Test.Block
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX
-}
module Haskoin.Util.Arbitrary.Block where

import qualified Data.HashMap.Strict as HashMap
import Haskoin.Block
import Haskoin.Constants
import Haskoin.Util.Arbitrary.Crypto
import Haskoin.Util.Arbitrary.Network
import Haskoin.Util.Arbitrary.Transaction
import Haskoin.Util.Arbitrary.Util
import Test.QuickCheck

-- | Block full or arbitrary transactions.
arbitraryBlock :: Network -> Gen Block
arbitraryBlock net = do
    h <- arbitraryBlockHeader
    c <- choose (0, 10)
    txs <- vectorOf c (arbitraryTx net)
    return $ Block h txs

-- | Block header with random hash.
arbitraryBlockHeader :: Gen BlockHeader
arbitraryBlockHeader =
    BlockHeader <$> arbitrary
        <*> arbitraryBlockHash
        <*> arbitraryHash256
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- | Arbitrary block hash.
arbitraryBlockHash :: Gen BlockHash
arbitraryBlockHash = BlockHash <$> arbitraryHash256

-- | Arbitrary 'GetBlocks' object with at least one block hash.
arbitraryGetBlocks :: Gen GetBlocks
arbitraryGetBlocks =
    GetBlocks <$> arbitrary
        <*> listOf1 arbitraryBlockHash
        <*> arbitraryBlockHash

-- | Arbitrary 'GetHeaders' object with at least one block header.
arbitraryGetHeaders :: Gen GetHeaders
arbitraryGetHeaders =
    GetHeaders <$> arbitrary
        <*> listOf1 arbitraryBlockHash
        <*> arbitraryBlockHash

-- | Arbitrary 'Headers' object with at least one block header.
arbitraryHeaders :: Gen Headers
arbitraryHeaders =
    Headers <$> listOf1 ((,) <$> arbitraryBlockHeader <*> arbitraryVarInt)

-- | Arbitrary 'MerkleBlock' with at least one hash.
arbitraryMerkleBlock :: Gen MerkleBlock
arbitraryMerkleBlock = do
    bh <- arbitraryBlockHeader
    ntx <- arbitrary
    hashes <- listOf1 arbitraryHash256
    c <- choose (1, 10)
    flags <- vectorOf (c * 8) arbitrary
    return $ MerkleBlock bh ntx hashes flags

-- | Arbitrary 'BlockNode'
arbitraryBlockNode :: Gen BlockNode
arbitraryBlockNode =
    oneof
        [ BlockNode
            <$> arbitraryBlockHeader
            <*> choose (0, maxBound)
            <*> arbitrarySizedNatural
            <*> arbitraryBlockHash
        ]

-- | Arbitrary 'HeaderMemory'
arbitraryHeaderMemory :: Gen HeaderMemory
arbitraryHeaderMemory = do
    ls <- listOf $ (,) <$> arbitrary <*> arbitraryBSS
    HeaderMemory
        <$> (return $ HashMap.fromList ls)
        <*> arbitraryBlockNode
