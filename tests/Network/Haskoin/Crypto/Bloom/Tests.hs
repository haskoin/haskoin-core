module Network.Haskoin.Crypto.Bloom.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Network.Haskoin.Crypto.Arbitrary ()
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize protocol messages"
        [ testProperty "BloomFlags" (metaBinary :: BloomFlags -> Bool)
        , testProperty "BloomFilter" (metaBinary :: BloomFilter -> Bool)
        , testProperty "FilterLoad" (metaBinary :: FilterLoad -> Bool)
        , testProperty "FilterAdd" (metaBinary :: FilterAdd -> Bool)
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

