module Main where

import Test.Framework (defaultMain)

import Network.Haskoin.Constants
import qualified Network.Haskoin.Node.Tests (tests)
import qualified Network.Haskoin.Node.Units (tests)

main :: IO ()
main = do
    setProdnet
    defaultMain
        (  Network.Haskoin.Node.Tests.tests
        ++ Network.Haskoin.Node.Units.tests
        )

