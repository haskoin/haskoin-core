{-# LANGUAGE EmptyDataDecls #-}
module Network.Haskoin.Types.Arbitrary ()  where

import Test.QuickCheck

import Control.Applicative ((<$>))

import Network.Haskoin.Types.BTC

instance Arbitrary BTC where
    arbitrary = satoshi <$> choose (0,2100000000000000)


