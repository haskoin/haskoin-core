{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck (Arbitrary, arbitrary)

import Control.Applicative ((<$>))

import Network.Haskoin.Wallet

instance Arbitrary NodeAction where
    arbitrary = Rescan <$> arbitrary

