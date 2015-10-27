module Network.Haskoin.Crypto.Hash.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Test

tests :: [Test]
tests =
    [ testGroup "Hash tests"
        [ testProperty "join512( split512(h) ) == h" joinSplit512
        , testProperty "decodeCompact . encodeCompact i == i" decEncCompact
        ]
    ]

joinSplit512 :: (ArbitraryHash256, ArbitraryHash256) -> Bool
joinSplit512 (ArbitraryHash256 a, ArbitraryHash256 b) =
    (split512 $ join512 (a, b)) == (a, b)

-- After encoding and decoding, we may loose precision so the new result is >=
-- to the old one.
decEncCompact :: Integer -> Bool
decEncCompact i
    -- Integer completely fits inside the mantisse
    | (abs i) <= 0x007fffff = (decodeCompact $ encodeCompact i) == i
    -- Otherwise precision will be lost and the decoded result will
    -- be smaller than the original number
    | i >= 0                = (decodeCompact $ encodeCompact i) < i
    | otherwise             = (decodeCompact $ encodeCompact i) > i

