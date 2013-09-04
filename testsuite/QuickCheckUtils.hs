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

newtype PrvWallet = PrvWallet Wallet
    deriving (Show, Eq)

newtype PubWallet  = PubWallet  Wallet
    deriving (Show, Eq)

instance Arbitrary PrvWallet where
    arbitrary = do
        d <- choose (0,10) :: Gen Integer
        p <- arbitrary
        i <- arbitrary
        c <- choose (0, 2^256 - 1) :: Gen Integer
        k <- choose (1, curveN - 1) :: Gen Integer
        let pk = fromJust $ makePrvKey k
        return $ PrvWallet $ XPrvKey (fromIntegral d) p i (fromIntegral c) pk

instance Arbitrary PubWallet where
    arbitrary = do
        (PrvWallet w) <- arbitrary :: Gen PrvWallet
        return $ PubWallet $ toPubWallet w

instance Arbitrary Wallet where
    arbitrary = do
        d <- choose (0,10) :: Gen Integer
        p <- arbitrary
        i <- arbitrary
        c <- choose (0, 2^256 - 1) :: Gen Integer
        k <- choose (1, curveN - 1) :: Gen Integer
        let pk   = fromJust $ makePrvKey k
            wPrv = XPrvKey (fromIntegral d) p i (fromIntegral c) pk
            wPub = toPubWallet wPrv
        elements [wPrv, wPub]

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

