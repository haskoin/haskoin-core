module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Node.Tests (tests)
import qualified Network.Haskoin.Node.Units (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Node.Tests.tests
    ++ Network.Haskoin.Node.Units.tests
    )

