module Network.Haskoin.Crypto.Hash.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.String (fromString)
import Data.String.Conversions (cs)
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Test
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Hash tests"
        [ testProperty "join512( split512(h) ) == h" joinSplit512
        , testProperty "decodeCompact . encodeCompact i == i" decEncCompact
        , testProperty "Read/Show 64-byte hash" testReadShowHash512
        , testProperty "From string 64-byte hash" testFromStringHash512
        , testProperty "Read/Show 32-byte hash" testReadShowHash256
        , testProperty "From string 32-byte hash" testFromStringHash256
        , testProperty "Read/Show 20-byte hash" testReadShowHash160
        , testProperty "From string 20-byte hash" testFromStringHash160
        , testProperty "Read/Show checksum" testReadShowCheckSum32
        , testProperty "From string checksum" testFromStringCheckSum32
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


testReadShowHash512 :: ArbitraryHash512 -> Bool
testReadShowHash512 (ArbitraryHash512 k) = read (show k) == k

testFromStringHash512 :: ArbitraryHash512 -> Bool
testFromStringHash512 (ArbitraryHash512 k) = fromString (cs $ encodeHex $ encode' k) == k


testReadShowHash256 :: ArbitraryHash256 -> Bool
testReadShowHash256 (ArbitraryHash256 k) = read (show k) == k

testFromStringHash256 :: ArbitraryHash256 -> Bool
testFromStringHash256 (ArbitraryHash256 k) = fromString (cs $ encodeHex $ encode' k) == k


testReadShowHash160 :: ArbitraryHash160 -> Bool
testReadShowHash160 (ArbitraryHash160 k) = read (show k) == k

testFromStringHash160 :: ArbitraryHash160 -> Bool
testFromStringHash160 (ArbitraryHash160 k) = fromString (cs $ encodeHex $ encode' k) == k


testReadShowCheckSum32 :: ArbitraryCheckSum32 -> Bool
testReadShowCheckSum32 (ArbitraryCheckSum32 k) = read (show k) == k

testFromStringCheckSum32 :: ArbitraryCheckSum32 -> Bool
testFromStringCheckSum32 (ArbitraryCheckSum32 k) = fromString (cs $ encodeHex $ encode' k) == k
