{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Crypto.HashSpec (spec) where

import           Data.ByteString         (ByteString)
import           Data.Map.Strict         (singleton)
import           Data.Maybe              (fromJust)
import           Data.Serialize          as S
import           Data.String             (fromString)
import           Data.String.Conversions
import           Haskoin.Block
import           Haskoin.Crypto
import           Haskoin.Util
import           Haskoin.Util.Arbitrary
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit              (Assertion, assertBool)
import           Test.QuickCheck

serialVals :: [SerialBox]
serialVals =
    [ SerialBox arbitraryBS
    , SerialBox arbitraryBSS
    , SerialBox arbitraryHash160
    , SerialBox arbitraryHash256
    , SerialBox arbitraryHash512
    ]

readVals :: [ReadBox]
readVals =
    [ ReadBox arbitraryBS
    , ReadBox arbitraryBSS
    , ReadBox arbitraryHash160
    , ReadBox arbitraryHash256
    , ReadBox arbitraryHash512
    ]

spec :: Spec
spec =
    describe "hash" $ do
        testIdentity serialVals readVals [] []
        prop "join512( split512(h) ) == h" $
            forAll arbitraryHash256 $ forAll arbitraryHash256 . joinSplit512
        prop "decodeCompact . encodeCompact i == i" decEncCompact
        prop "from string 64-byte hash" $
            forAll arbitraryHash512 $ \h ->
                fromString (cs $ encodeHex $ encode h) == h
        prop "from string 32-byte hash" $
            forAll arbitraryHash256 $ \h ->
                fromString (cs $ encodeHex $ encode h) == h
        prop "from string 20-byte hash" $
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
    | i >= 0 = fst (decodeCompact (encodeCompact i)) < i
    | otherwise = fst (decodeCompact (encodeCompact i)) > i
