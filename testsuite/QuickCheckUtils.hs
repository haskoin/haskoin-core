module QuickCheckUtils where

import Test.QuickCheck
import Control.Applicative
import Control.Monad

import Bitcoin.Type.VarInt
import Bitcoin.Type.VarString
import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.Version
import Bitcoin.Type.Addr
import Bitcoin.Type.BlockHeader

import Bitcoin.BigWord

import qualified Data.ByteString as BS

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
  arbitrary = do
    bs <- BS.pack `fmap` arbitrary
    n  <- choose (0, 2)
    return (BS.drop n bs) -- to give us some with non-0 offset

instance Arbitrary (BigWord a b) where
    arbitrary = do
        a <- arbitrary :: Gen a
        b <- arbitrary :: Gen b
        (fromIntegral a `shiftL` bitSize b) + fromIntegral b

instance Arbitrary VarInt where
    arbitrary = VarInt <$> arbitrary

instance Arbitrary VarString where
    arbitrary = VarString <$> arbitrary

instance Arbitrary NetworkAddress where
    arbitrary = liftM3 NetworkAddress arbitrary arbitrary arbitrary

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Addr where
    arbitrary = Addr <$> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary

