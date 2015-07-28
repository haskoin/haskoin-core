module Network.Haskoin.Node.Actors.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import Network.Haskoin.Node.Actors.Types

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize haskoin node types to JSON"
        [ 
        ]
    ]

