{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , oneof
    , vectorOf
    , listOf1
    , elements
    )

import Control.Applicative ((<$>), (<*>))

import Network.Haskoin.Test
import Network.Haskoin.Wallet

instance Arbitrary NodeAction where
    arbitrary = Rescan <$> arbitrary

