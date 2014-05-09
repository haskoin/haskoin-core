module Network.Haskoin.Crypto.Bloom.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Haskoin.Protocol.Arbitrary ()
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Bloom Filters"
        [ testProperty "decode . encode BloomFilter" decodeEncodeBloom
        ]
    ]

{- Bloom Filters -}

decodeEncodeBloom :: BloomFilter -> Bool
decodeEncodeBloom bfilter = (decode' $ encode' bfilter) == bfilter


