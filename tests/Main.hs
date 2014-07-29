module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Wallet.Units (tests)
import qualified Network.Haskoin.Wallet.Tests (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Wallet.Tests.tests
    ++ Network.Haskoin.Wallet.Units.tests 
    )

