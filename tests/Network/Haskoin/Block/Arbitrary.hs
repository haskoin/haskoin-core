{-|
  This package provides QuickCheck Arbitrary instances for all the block
  data types defined in 'Network.Haskoin.Block.
-}
module Network.Haskoin.Block.Arbitrary () where

import Test.QuickCheck

import Control.Monad
import Control.Applicative 

import Network.Haskoin.Crypto.Arbitrary ()
import Network.Haskoin.Protocol.Arbitrary ()
import Network.Haskoin.Transaction.Arbitrary ()

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle
import Network.Haskoin.Crypto.Hash

instance Arbitrary Block where
    arbitrary = do
        h <- arbitrary
        c <- arbitrary
        t <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        return $ Block h c t

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader <$> arbitrary
                            <*> (fromIntegral . hash256 <$> arbitrary)
                            <*> (hash256 <$> arbitrary)
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            
instance Arbitrary GetBlocks where
    arbitrary = GetBlocks <$> arbitrary
                          <*> (listOf arbitrary)
                          <*> arbitrary

instance Arbitrary GetHeaders where
    arbitrary = GetHeaders <$> arbitrary
                           <*> (listOf arbitrary)
                           <*> arbitrary

instance Arbitrary Headers where
    arbitrary = Headers <$> (listOf (liftM2 (,) arbitrary arbitrary))

instance Arbitrary MerkleBlock where
    arbitrary = do
        h <- arbitrary
        ntx <- arbitrary
        hashes <- arbitrary
        c <- choose (1,10)
        flags <- vectorOf (c*8) arbitrary
        return $ MerkleBlock h ntx hashes flags


