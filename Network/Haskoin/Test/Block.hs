{-|
  Arbitrary types for Network.Haskoin.Block
-}
module Network.Haskoin.Test.Block
( ArbitraryBlock(..)
, ArbitraryBlockHeader(..)
, ArbitraryBlockHash(..)
, ArbitraryGetBlocks(..)
, ArbitraryGetHeaders(..)
, ArbitraryHeaders(..)
, ArbitraryMerkleBlock(..)
) where

import Test.QuickCheck
    ( Arbitrary
    , arbitrary
    , choose
    , vectorOf
    , listOf1
    )

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Transaction
import Network.Haskoin.Test.Node

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle

-- | Arbitrary Block
newtype ArbitraryBlock = ArbitraryBlock Block
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryBlock where
    arbitrary = do
        ArbitraryBlockHeader h <- arbitrary
        ArbitraryCoinbaseTx cb <- arbitrary
        c <- choose (0,10)
        txs <- map (\(ArbitraryTx x) -> x) <$> vectorOf c arbitrary
        return $ ArbitraryBlock $ Block h cb txs

-- | Arbitrary BlockHeader
newtype ArbitraryBlockHeader = ArbitraryBlockHeader BlockHeader
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryBlockHeader where
    arbitrary = do
        ArbitraryBlockHash h1 <- arbitrary
        ArbitraryHash256 h2 <- arbitrary
        h <- BlockHeader <$> arbitrary <*> return h1 <*> return h2
                         <*> arbitrary <*> arbitrary <*> arbitrary
        return $ ArbitraryBlockHeader h

newtype ArbitraryBlockHash = ArbitraryBlockHash BlockHash
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryBlockHash where
    arbitrary = do
        ArbitraryHash256 h <- arbitrary
        return $ ArbitraryBlockHash $ BlockHash h

-- | Arbitrary GetBlocks
newtype ArbitraryGetBlocks = ArbitraryGetBlocks GetBlocks
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryGetBlocks where
    arbitrary = do
        hs <- listOf1 arbitrary
        let hs' = map (\(ArbitraryBlockHash h) -> h) hs
        ArbitraryBlockHash h <- arbitrary
        b <- GetBlocks <$> arbitrary <*> return hs' <*> return h
        return $ ArbitraryGetBlocks b

-- | Arbitrary GetHeaders
newtype ArbitraryGetHeaders = ArbitraryGetHeaders GetHeaders
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryGetHeaders where
    arbitrary = do
        hs <- listOf1 arbitrary
        let hs' = map (\(ArbitraryBlockHash h) -> h) hs
        ArbitraryBlockHash h' <- arbitrary
        h <- GetHeaders <$> arbitrary <*> return hs' <*> return h'
        return $ ArbitraryGetHeaders h

-- | Arbitrary Headers
newtype ArbitraryHeaders = ArbitraryHeaders Headers
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryHeaders where
    arbitrary = ArbitraryHeaders <$> do
        xs <- listOf1 $ do
            ArbitraryBlockHeader h <- arbitrary
            ArbitraryVarInt v <- arbitrary
            return (h,v)
        return $ Headers xs

-- | Arbitrary MerkleBlock
newtype ArbitraryMerkleBlock = ArbitraryMerkleBlock MerkleBlock
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMerkleBlock where
    arbitrary = ArbitraryMerkleBlock <$> do
        ArbitraryBlockHeader bh <- arbitrary
        ntx <- arbitrary
        hashes <- listOf1 arbitrary
        let hashes' = map (\(ArbitraryHash256 h) -> h) hashes
        c <- choose (1,10)
        flags <- vectorOf (c*8) arbitrary
        return $ MerkleBlock bh ntx hashes' flags


