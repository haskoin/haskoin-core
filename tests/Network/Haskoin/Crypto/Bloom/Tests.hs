module Network.Haskoin.Crypto.Bloom.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize protocol messages"
        [ testProperty "BloomFlags" $ \(ArbitraryBloomFlags x) -> metaBinary x
        , testProperty "BloomFilter" $ \(ArbitraryBloomFilter _ _ x) -> metaBinary x
        , testProperty "FilterLoad" $ \(ArbitraryFilterLoad x) -> metaBinary x
        , testProperty "FilterAdd" $ \(ArbitraryFilterAdd x) -> metaBinary x
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

