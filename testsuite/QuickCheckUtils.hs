module QuickCheckUtils where

import Test.QuickCheck
import Control.Applicative

import Bitcoin.Type.Version
import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.VarInt
import Bitcoin.Type.VarString

import qualified Data.ByteString as BS

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary NetworkAddress where
    arbitrary = NetworkAddress <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary

instance Arbitrary VarInt where
    arbitrary = VarInt <$> arbitrary

instance Arbitrary VarString where
    arbitrary = VarString <$> arbitrary

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
  arbitrary = do
    bs <- BS.pack `fmap` arbitrary
    n  <- choose (0, 2)
    return (BS.drop n bs) -- to give us some with non-0 offset

