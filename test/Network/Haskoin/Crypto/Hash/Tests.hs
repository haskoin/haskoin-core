module Network.Haskoin.Crypto.Hash.Tests (spec) where

import           Data.Serialize            (encode)
import           Data.String               (fromString)
import           Data.String.Conversions
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Network.Haskoin.Util      (encodeHex)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "hash" $ do
        it "join512( split512(h) ) == h" $
            property $
            forAll arbitraryHash256 $ forAll arbitraryHash256 . joinSplit512
        it "decodeCompact . encodeCompact i == i" $ property decEncCompact
        it "from string 64-byte hash" $
            property $
            forAll arbitraryHash512 $ \h ->
                fromString (cs $ encodeHex $ encode h) == h
        it "from string 32-byte hash" $
            property $
            forAll arbitraryHash256 $ \h ->
                fromString (cs $ encodeHex $ encode h) == h
        it "from string 20-byte hash" $
            property $
            forAll arbitraryHash160 $ \h ->
                fromString (cs $ encodeHex $ encode h) == h

joinSplit512 :: Hash256 -> Hash256 -> Bool
joinSplit512 a b = split512 (join512 (a, b)) == (a, b)

-- After encoding and decoding, we may loose precision so the new result is >=
-- to the old one.
decEncCompact :: Integer -> Bool
decEncCompact i
    -- Integer completely fits inside the mantisse
    | abs i <= 0x007fffff = decodeCompact (encodeCompact i) == (i, False)
    -- Otherwise precision will be lost and the decoded result will
    -- be smaller than the original number
    | i >= 0              = fst (decodeCompact (encodeCompact i)) < i
    | otherwise           = fst (decodeCompact (encodeCompact i)) > i

