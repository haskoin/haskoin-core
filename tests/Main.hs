module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Wallet.Units (tests)

main :: IO ()
main = defaultMain
    ( Network.Haskoin.Wallet.Units.tests )

