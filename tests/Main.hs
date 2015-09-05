module Main where

import Test.Framework (defaultMain)

-- import qualified Network.Haskoin.Wallet.Units (tests) -- TODO
import Test.Framework.Providers.HUnit (testCase) -- Remove when above fixed
import qualified Network.Haskoin.Wallet.Tests (tests)

import Network.Haskoin.Constants

main :: IO ()
main | networkName == "prodnet" = defaultMain
        (  Network.Haskoin.Wallet.Tests.tests
        -- ++ Network.Haskoin.Wallet.Units.tests -- TODO
        ++ [testCase "Align unit tests" $ error "just do it"]
        )
     | otherwise = error "Tests are only available on prodnet"

