module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Wallet.Tests (tests)
import qualified Network.Haskoin.Wallet.Store.Units (tests)
import qualified Units (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Wallet.Tests.tests 
    ++ Network.Haskoin.Wallet.Store.Units.tests
    ++ Units.tests 
    )

