module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Crypto

instance Arbitrary XPrvKey where
    arbitrary = XPrvKey <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> genPrvKeyC

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

