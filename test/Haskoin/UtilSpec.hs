module Haskoin.UtilSpec
    ( spec
    , testJsonID
    , testCustomJSON
    , testCustomEncoding
    , cerealID
    , customCerealID
    ) where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Aeson          as A
import           Data.Aeson.Encoding (encodingToLazyByteString)
import           Data.Aeson.Types    (Parser, parseMaybe)
import qualified Data.ByteString     as BS
import           Data.Either         (fromLeft, fromRight, isLeft, isRight)
import           Data.Foldable       (toList)
import           Data.List           (permutations)
import           Data.Map.Strict     (singleton)
import           Data.Maybe
import qualified Data.Sequence       as Seq
import           Data.Serialize      as S
import           Haskoin.Test
import           Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "utility functions" $ do
        it "bsToInteger . integerToBS" $ property getPutInteger
        it "decodeHex . encodeHex" $ property $ forAll arbitraryBS fromToHex
        it "compare updateIndex with Data.Sequence" $ property testUpdateIndex
        it "matchTemplate" $ property testMatchTemplate
        it "testing matchTemplate with two lists" $
            property testMatchTemplateLen
        it "either helper functions" $ property testEither

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

cerealID :: (Serialize a, Eq a) => a -> Bool
cerealID x = S.decode (S.encode x) == Right x

customCerealID :: Eq a => Get a -> Putter a -> a -> Bool
customCerealID g p a = runGet g (runPut (p a)) == Right a

testJsonID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testJsonID x = jsonID_ x && encodingID_ x

jsonID_ :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
jsonID_ x =
    (A.fromJSON . A.toJSON) (singleton ("object" :: String) x) ==
    A.Success (singleton ("object" :: String) x)

encodingID_ :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
encodingID_ x =
    (A.decode . encodingToLazyByteString . A.toEncoding)
        (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)

testCustomJSON :: Eq a => (A.Value -> Parser a) -> (a -> A.Value) -> a -> Bool
testCustomJSON f g x = parseMaybe f (g x) == Just x

testCustomEncoding ::
       Eq a => (A.Value -> Parser a) -> (a -> A.Encoding) -> a -> Bool
testCustomEncoding f g x =
    dec (encodingToLazyByteString $ g x) == Just x
  where
    dec bs = parseMaybe f =<< A.decode bs
