module QuickCheckUtils where

import Test.QuickCheck

import Control.Applicative

import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Crypto

maxN :: Integer
maxN = fromIntegral $ (maxBound :: FieldN) - 1

newtype PrivateWallet = PrivateWallet Wallet
    deriving (Show, Eq)
newtype PublicWallet  = PublicWallet  Wallet
    deriving (Show, Eq)

instance Arbitrary PrivateWallet where
    arbitrary = do
        k <- choose (1, maxN) :: Gen Integer
        c <- choose (0, 2^256 - 1) :: Gen Integer
        d <- choose (0,10) :: Gen Integer
        i <- arbitrary
        p <- arbitrary
        let pk = fromJust $ makePrivateKey k
        return $ PrivateWallet $ 
            XPrivateKey pk (fromIntegral c) (fromIntegral d) i p

instance Arbitrary PublicWallet where
    arbitrary = do
        (PrivateWallet w) <- arbitrary :: Gen PrivateWallet
        return $ PublicWallet $ publicWallet w

instance Arbitrary Wallet where
    arbitrary = do
        k <- choose (1, maxN) :: Gen Integer
        c <- choose (0, 2^256 - 1) :: Gen Integer
        d <- choose (0,10) :: Gen Integer
        i <- arbitrary
        p <- arbitrary
        let pk    = fromJust $ makePrivateKey k
            wPriv = XPrivateKey pk (fromIntegral c) (fromIntegral d) i p
            wPub  = publicWallet wPriv
        elements [ wPriv, wPub ]

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

