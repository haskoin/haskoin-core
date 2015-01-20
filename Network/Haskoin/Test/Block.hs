{-| 
  Arbitrary types for Network.Haskoin.Block
-}
module Network.Haskoin.Test.Block
( ArbitraryBlock(..)
, ArbitraryBlockHeader(..)
, ArbitraryGetBlocks(..)
, ArbitraryGetHeaders(..)
, ArbitraryHeaders(..)
, ArbitraryMerkleBlock(..)
) where

import Test.QuickCheck 
    ( Arbitrary
    , Gen
    , arbitrary
    , choose
    , vectorOf
    , listOf1
    )

import Control.Applicative ((<$>), (<*>))

import Network.Haskoin.Test.Transaction
import Network.Haskoin.Network
import Network.Haskoin.Test.Node

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle

-- | Arbitrary Block
newtype ArbitraryBlock a = ArbitraryBlock Block
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryBlock a) where
    arbitrary = do
        ArbitraryBlockHeader h <- arbitrary :: Gen (ArbitraryBlockHeader a)
        ArbitraryCoinbaseTx cb <- arbitrary :: Gen (ArbitraryCoinbaseTx a)
        c <- choose (0,10)
        txs <- map (\(ArbitraryTx x) -> x) <$>
            vectorOf c (arbitrary :: Gen (ArbitraryTx a))
        return $ ArbitraryBlock $ Block h cb txs

-- | Arbitrary BlockHeader
newtype ArbitraryBlockHeader a = ArbitraryBlockHeader BlockHeader
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryBlockHeader a) where
    arbitrary = do
        h <- BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary
        return $ ArbitraryBlockHeader h   
                    
-- | Arbitrary GetBlocks
newtype ArbitraryGetBlocks = ArbitraryGetBlocks GetBlocks
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryGetBlocks where
    arbitrary = do
        b <- GetBlocks <$> arbitrary <*> (listOf1 arbitrary) <*> arbitrary
        return $ ArbitraryGetBlocks b

-- | Arbitrary GetHeaders
newtype ArbitraryGetHeaders = ArbitraryGetHeaders GetHeaders
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryGetHeaders where
    arbitrary = do
        h <- GetHeaders <$> arbitrary <*> (listOf1 arbitrary) <*> arbitrary
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
        hashes <- arbitrary
        c <- choose (1,10)
        flags <- vectorOf (c*8) arbitrary
        return $ MerkleBlock bh ntx hashes flags


