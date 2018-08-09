module Network.Haskoin.Util.Tests (tests) where

import qualified Data.ByteString                      as BS
import           Data.Either                          (fromLeft, fromRight,
                                                       isLeft, isRight)
import           Data.Foldable                        (toList)
import           Data.List                            (permutations)
import           Data.Maybe
import qualified Data.Sequence                        as Seq
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup
          "Utility functions"
          [ testProperty "bsToInteger . integerToBS" getPutInteger
          , testProperty "decodeHex . encodeHex" $ forAll arbitraryBS fromToHex
          , testProperty
                "compare updateIndex with Data.Sequence"
                testUpdateIndex
          , testProperty "matchTemplate" testMatchTemplate
          , testProperty
                "testing matchTemplate with two lists"
                testMatchTemplateLen
          , testProperty "Testing Either helper functions" testEither
          ]
    ]

{- Various utilities -}

getPutInteger :: Integer -> Bool
getPutInteger i = bsToInteger (integerToBS $ abs i) == abs i

fromToHex :: BS.ByteString -> Bool
fromToHex bs = decodeHex (encodeHex bs) == Just bs

testUpdateIndex :: [Int] -> Int -> Int -> Bool
testUpdateIndex xs v i =
    updateIndex i xs (const v) == toList (Seq.update i v $ Seq.fromList xs)

testMatchTemplate :: [Int] -> Int -> Bool
testMatchTemplate as i = catMaybes res == bs
  where
    res = matchTemplate as bs (==)
    idx =
        if null as
            then 0
            else i `mod` length as
    bs = permutations as !! idx

testMatchTemplateLen :: [Int] -> [Int] -> Bool
testMatchTemplateLen as bs = length bs == length res
  where
    res = matchTemplate as bs (==)

testEither :: Either String Int -> Bool
testEither e =
    case e of
        (Right v) ->
            isRight e &&
            not (isLeft e) &&
            fromRight (error "Unexpected Left") e == v &&
            eitherToMaybe e == Just v
        (Left v) ->
            isLeft e &&
            not (isRight e) &&
            fromLeft (error "Unexpected Right") e == v &&
            isNothing (eitherToMaybe e)
