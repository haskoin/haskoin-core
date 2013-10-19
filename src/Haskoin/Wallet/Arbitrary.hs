module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Store
import Haskoin.Crypto

instance Arbitrary XPrvKey where
    arbitrary = XPrvKey <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> genPrvKeyC

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

instance Arbitrary WAccount where
    arbitrary = do
        name   <- arbitrary
        index  <- arbitrary
        key    <- AccPubKey <$> arbitrary
        ext    <- arbitrary
        int    <- arbitrary
        msN    <- choose (1,16)
        msM    <- choose (1,msN)
        msKeys <- vectorOf (msN-1) arbitrary
        url    <- arbitrary
        elements [ WAccount name index key ext int
                 , WAccountMS name index key ext int msKeys msM url
                 ]

instance Arbitrary WAddr where
    arbitrary = WAddr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

