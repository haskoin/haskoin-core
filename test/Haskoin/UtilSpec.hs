{-# LANGUAGE ImportQualifiedPost #-}

module Haskoin.UtilSpec (spec) where

import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (Parser, parse)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Foldable (toList)
import Data.List (permutations)
import Data.Map.Strict (singleton)
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Serialize as S
import Haskoin.Crypto
import Haskoin.Util
import Haskoin.Util.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll)

spec :: Spec
spec =
  describe "utility functions" $ do
    prop "bsToInteger . integerToBS" getPutInteger
    prop "decodeHex . encodeHex" $ forAll arbitraryBS fromToHex
    prop "compare updateIndex with Data.Sequence" testUpdateIndex
    prop "matchTemplate" testMatchTemplate
    prop "testing matchTemplate with two lists" testMatchTemplateLen
    prop "test eitherToMaybe" testEitherToMaybe
    prop "test maybeToEither" testMaybeToEither

{- Various utilities -}

getPutInteger :: Integer -> Bool
getPutInteger i = bsToInteger (integerToBS $ abs i) == abs i

fromToHex :: ByteString -> Bool
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

testEitherToMaybe :: Either String Int -> Bool
testEitherToMaybe (Right v) = eitherToMaybe (Right v) == Just v
testEitherToMaybe e = isNothing (eitherToMaybe e)

testMaybeToEither :: Maybe Int -> String -> Bool
testMaybeToEither (Just v) str = maybeToEither str (Just v) == Right v
testMaybeToEither m str = maybeToEither str m == Left str
