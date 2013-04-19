module QuickCheckUtils where

import Test.QuickCheck

import Control.Monad
import Control.Applicative

import Bitcoin.Protocol
import Bitcoin.Protocol.BigWord
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.Addr
import Bitcoin.Protocol.BlockHeader

import qualified Data.ByteString as BS

instance (Arbitrary a, Arbitrary b) => Arbitrary (BigWord a b) where
    arbitrary = BigWord <$> arbitrary <*> arbitrary

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n  <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

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

