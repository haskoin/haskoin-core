module QuickCheckUtils where

import Test.QuickCheck

import Control.Applicative

import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Crypto

curveN :: Integer
curveN = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 

instance Arbitrary XPrvKey where
    arbitrary = do
        d <- arbitrary
        p <- arbitrary
        i <- arbitrary
        c <- choose (0, 2^256 - 1) :: Gen Integer
        k <- choose (1, curveN - 1) :: Gen Integer
        let pk = fromJust $ makePrvKey k
        return $ XPrvKey d p i (fromIntegral c) pk

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

