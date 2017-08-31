module Network.Haskoin.Crypto.Hash.Tests (tests) where

import           Data.Serialize                       (encode)
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup "Hash tests"
        [ testProperty "join512( split512(h) ) == h" $
          forAll arbitraryHash256 $ forAll arbitraryHash256 . joinSplit512
        , testProperty "decodeCompact . encodeCompact i == i" decEncCompact
        , testProperty "Read/Show 64-byte hash" $ forAll arbitraryHash512 $
          \h -> read (show h) == h
        , testProperty "From string 64-byte hash" $ forAll arbitraryHash512 $
          \h -> fromString (cs $ encodeHex $ encode h) == h
        , testProperty "Read/Show 32-byte hash" $ forAll arbitraryHash256 $
          \h -> read (show h) == h
        , testProperty "From string 32-byte hash" $ forAll arbitraryHash256 $
          \h -> fromString (cs $ encodeHex $ encode h) == h
        , testProperty "Read/Show 20-byte hash" $ forAll arbitraryHash160 $
          \h -> read (show h) == h
        , testProperty "From string 20-byte hash" $ forAll arbitraryHash160 $
          \h -> fromString (cs $ encodeHex $ encode h) == h
        , testProperty "Read/Show checksum" $ forAll arbitraryCheckSum32 $
          \h -> read (show h) == h
        , testProperty "From string checksum" $ forAll arbitraryCheckSum32 $
          \h -> fromString (cs $ encodeHex $ encode h) == h
        ]
    ]

joinSplit512 :: Hash256 -> Hash256 -> Bool
joinSplit512 a b = split512 (join512 (a, b)) == (a, b)

-- After encoding and decoding, we may loose precision so the new result is >=
-- to the old one.
decEncCompact :: Integer -> Bool
decEncCompact i
    -- Integer completely fits inside the mantisse
    | abs i <= 0x007fffff = decodeCompact (encodeCompact i) == i
    -- Otherwise precision will be lost and the decoded result will
    -- be smaller than the original number
    | i >= 0              = decodeCompact (encodeCompact i) < i
    | otherwise           = decodeCompact (encodeCompact i) > i

