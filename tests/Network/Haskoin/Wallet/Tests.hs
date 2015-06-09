module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, encode, decode)

import Network.Haskoin.Wallet.Arbitrary ()
import Network.Haskoin.Wallet

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize types to JSON"
        [ testProperty "NodeAction" (metaID :: NodeAction -> Bool)
        , testProperty "TxAction" (metaID :: TxAction -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

