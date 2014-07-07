module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Aeson

import Network.Haskoin.Wallet.Arbitrary()
import Network.Haskoin.Wallet.Types
tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize wallet types to JSON"
        [ testProperty "Wallet" (metaID :: Wallet -> Bool)
        , testProperty "Account" (metaID :: Account -> Bool)
        , testProperty "PaymentAddress" (metaID :: PaymentAddress -> Bool)
        , testProperty "AccTx" (metaID :: AccTx -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) x == Just x
