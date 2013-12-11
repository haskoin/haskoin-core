module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Network.Haskoin.Wallet.Tests (tests)
import qualified Network.Haskoin.Wallet.Store.Units (tests)
import qualified Units (tests)

main = defaultMain
    (  Network.Haskoin.Wallet.Tests.tests 
    ++ Network.Haskoin.Wallet.Store.Units.tests
    ++ Units.tests 
    )

