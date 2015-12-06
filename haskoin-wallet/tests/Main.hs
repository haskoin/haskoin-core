module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Wallet.Units (tests)
import qualified Network.Haskoin.Wallet.Tests (tests)

import Network.Haskoin.Constants

main :: IO ()
main | networkName == "prodnet" = defaultMain
        (  Network.Haskoin.Wallet.Tests.tests
        ++ Network.Haskoin.Wallet.Units.tests
        )
     | otherwise = error "Tests are only available on prodnet"

