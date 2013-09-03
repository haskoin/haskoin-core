module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Haskoin.Wallet.Tests (tests)
import qualified Units (tests)

main = defaultMain 
    (  Haskoin.Wallet.Tests.tests 
    ++ Units.tests 
    )

