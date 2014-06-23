module Network.Haskoin.Types.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Haskoin.Types.Arbitrary ()
import Network.Haskoin.Types.BTC
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "BTC Binary"
        [ testProperty "decode . encode BTC = BTC" decEncBTC
        ]
    ]

decEncBTC :: BTC -> Bool
decEncBTC b = (decode' . encode') b == b


