{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.UtilSpec (
    spec,
    readTestFile,
    SerialBox (..),
    ReadBox (..),
    JsonBox (..),
    NetBox (..),
    testIdentity,
    testJson,
    testRead,
    testSerial,
    testNetJson,
) where

import Bitcoin (Network)
import Bitcoin.Util (
    bsToInteger,
    decodeHex,
    eitherToMaybe,
    encodeHex,
    integerToBS,
    matchTemplate,
    maybeToEither,
    updateIndex,
 )
import qualified Bitcoin.Util as U
import Bitcoin.Util.Arbitrary (arbitraryBS, fromMap, toMap)
import Control.Monad (forM_, (<=<))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import Data.Aeson.Encoding (
    Encoding,
    encodingToLazyByteString,
    pair,
 )
import Data.Aeson.Types (
    Parser,
    Result (Success),
    ToJSON (toEncoding, toJSON),
    Value,
    fromJSON,
    parseMaybe,
 )
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List (permutations)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Sequence as Seq
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, forAll)


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
getPutInteger i = (bsToInteger . BSL.fromStrict . integerToBS . abs) i == abs i


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


testEitherToMaybe :: Either String Int -> Bool
testEitherToMaybe (Right v) = eitherToMaybe (Right v) == Just v
testEitherToMaybe e = isNothing (eitherToMaybe e)


testMaybeToEither :: Maybe Int -> String -> Bool
testMaybeToEither (Just v) str = maybeToEither str (Just v) == Right v
testMaybeToEither m str = maybeToEither str m == Left str


{-- Test Utilities --}

readTestFile :: A.FromJSON a => FilePath -> IO a
readTestFile fp =
    A.eitherDecodeFileStrict ("data/" <> fp) >>= either (error . message) return
  where
    message aesonErr = "Could not read test file " <> fp <> ": " <> aesonErr


-- Helpers for creating Serial and JSON Identity tests

data SerialBox
    = forall a.
        (Show a, Eq a, Typeable a, Binary a) =>
      SerialBox (Gen a)


data ReadBox
    = forall a.
        (Read a, Show a, Eq a, Typeable a) =>
      ReadBox (Gen a)


data JsonBox
    = forall a.
        (Show a, Eq a, Typeable a, ToJSON a, FromJSON a) =>
      JsonBox (Gen a)


data NetBox
    = forall a.
        (Show a, Eq a, Typeable a) =>
      NetBox
        ( Network -> a -> Value
        , Network -> a -> Encoding
        , Network -> Value -> Parser a
        , Gen (Network, a)
        )


testIdentity :: [SerialBox] -> [ReadBox] -> [JsonBox] -> [NetBox] -> Spec
testIdentity serialVals readVals jsonVals netVals = do
    describe "Binary Encoding" $
        forM_ serialVals $
            \(SerialBox g) -> testSerial g
    describe "Read/Show Encoding" $
        forM_ readVals $
            \(ReadBox g) -> testRead g
    describe "Data.Aeson Encoding" $
        forM_ jsonVals $
            \(JsonBox g) -> testJson g
    describe "Data.Aeson Encoding with Network" $
        forM_ netVals $
            \(NetBox (j, e, p, g)) -> testNetJson j e p g


-- | Generate Data.Aeson identity tests
testJson ::
    (Eq a, Show a, Typeable a, ToJSON a, FromJSON a) => Gen a -> Spec
testJson gen = do
    prop ("Data.Aeson toJSON/fromJSON identity for " <> name) $
        forAll gen (`shouldSatisfy` jsonID)
    prop ("Data.Aeson toEncoding/fromJSON identity for " <> name) $
        forAll gen (`shouldSatisfy` encodingID)
  where
    name = show $ typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy
    jsonID x = (fromJSON . toJSON) (toMap x) == Data.Aeson.Types.Success (toMap x)
    encodingID x =
        (A.decode . encodingToLazyByteString . toEncoding) (toMap x)
            == Just (toMap x)


-- | Generate Data.Aeson identity tests for type that need the @Network@
testNetJson ::
    (Eq a, Show a, Typeable a) =>
    (Network -> a -> Value) ->
    (Network -> a -> Encoding) ->
    (Network -> Value -> Parser a) ->
    Gen (Network, a) ->
    Spec
testNetJson j e p g = do
    prop ("Data.Aeson toJSON/fromJSON identity (with network) for " <> name) $
        forAll g $
            \(net, x) -> dec net (encVal net x) `shouldBe` Just x
    prop ("Data.Aeson toEncoding/fromJSON identity (with network) for " <> name) $
        forAll g $
            \(net, x) -> dec net (encEnc net x) `shouldBe` Just x
  where
    encVal net = A.encode . toMap . j net
    encEnc net = encodingToLazyByteString . toMapE . e net
    dec net = parseMaybe (p net) . fromMap <=< A.decode
    name = show $ typeRep $ proxy j
    proxy :: (Network -> a -> Value) -> Proxy a
    proxy = const Proxy


-- | Generate binary identity tests
testSerial ::
    (Eq a, Show a, Typeable a, Binary a) => Gen a -> Spec
testSerial gen =
    prop ("Binary encoding/decoding identity for " <> name) $
        forAll gen $ \x -> do
            (U.decode . Bin.encode) x `shouldBe` Right x
  where
    name = show $ typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy


-- | Generate Read/Show identity tests
testRead ::
    (Eq a, Read a, Show a, Typeable a) => Gen a -> Spec
testRead gen =
    prop ("read/show identity for " <> name) $
        forAll gen $
            \x -> (read . show) x `shouldBe` x
  where
    name = show $ typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy


toMapE :: A.Encoding -> A.Encoding
toMapE = A.pairs . pair "object"
