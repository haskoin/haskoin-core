module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Node.Actors.Tests (tests)
import qualified Network.Haskoin.Node.Actors.Units (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Node.Actors.Tests.tests
    ++ Network.Haskoin.Node.Actors.Units.tests
    )

