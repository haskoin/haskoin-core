module Network.Haskoin.Util.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (permutations)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (update, fromList)

import Network.Haskoin.Test
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Utility functions"
        [ testProperty "bsToInteger . integerToBS" getPutInteger
        , testProperty "decodeHex . encodeHex" fromToHex
        , testProperty "compare updateIndex with Data.Sequence" testUpdateIndex
        , testProperty "matchTemplate" testMatchTemplate
        , testProperty
            "testing matchTemplate with two lists" testMatchTemplateLen
        , testProperty "Testing Either helper functions" testEither
        ]
    ]

{- Various utilities -}

getPutInteger :: Integer -> Bool
getPutInteger i = (bsToInteger $ integerToBS p) == p
  where
    p = abs i

fromToHex :: ArbitraryByteString -> Bool
fromToHex (ArbitraryByteString bs) = (fromJust $ decodeHex $ encodeHex bs) == bs

testUpdateIndex :: [Int] -> Int -> Int -> Bool
testUpdateIndex xs v i =
    (updateIndex i xs $ const v) == (toList $ Seq.update i v s)
  where
    s = Seq.fromList xs

testMatchTemplate :: [Int] -> Int -> Bool
testMatchTemplate as i = catMaybes res == bs
  where
    res = matchTemplate as bs (==)
    idx = if length as == 0 then 0 else i `mod` length as
    bs  = (permutations as) !! idx

testMatchTemplateLen :: [Int] -> [Int] -> Bool
testMatchTemplateLen as bs = length bs == length res
  where
    res = matchTemplate as bs (==)

testEither :: Either String Int -> Bool
testEither e = case e of
    (Right v) -> (isRight e)
              && (not $ isLeft e)
              && (fromRight e == v)
              && (eitherToMaybe e == Just v)
    (Left v)  -> (isLeft e)
              && (not $ isRight e)
              && (fromLeft e == v)
              && (eitherToMaybe e == Nothing)

