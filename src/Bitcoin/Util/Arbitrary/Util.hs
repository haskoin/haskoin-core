{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
module Bitcoin.Util.Arbitrary.Util (
    arbitraryBS,
    arbitraryBS1,
    arbitraryBSn,
    arbitraryBSS,
    arbitraryBSS1,
    arbitraryBSSn,
    arbitraryMaybe,
    arbitraryNetwork,
    arbitraryUTCTime,
    SerialBox (..),
    JsonBox (..),
    NetBox (..),
    ReadBox (..),
    testIdentity,
    testSerial,
    testRead,
    testJson,
    testNetJson,
    arbitraryNetData,
    genNetData,
) where

import Control.Monad (forM_, (<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString, pack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Short as BSS
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Typeable as T
import Data.Word (Word32)
import Bitcoin.Constants
import Bitcoin.Data
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


-- | Arbitrary strict 'ByteString'.
arbitraryBS :: Gen ByteString
arbitraryBS = pack <$> arbitrary


-- | Arbitrary non-empty strict 'ByteString'
arbitraryBS1 :: Gen ByteString
arbitraryBS1 = pack <$> listOf1 arbitrary


-- | Arbitrary strict 'ByteString' of a given length
arbitraryBSn :: Int -> Gen ByteString
arbitraryBSn n = pack <$> vectorOf n arbitrary


-- | Arbitrary 'ShortByteString'.
arbitraryBSS :: Gen BSS.ShortByteString
arbitraryBSS = BSS.pack <$> arbitrary


-- | Arbitrary non-empty 'ShortByteString'
arbitraryBSS1 :: Gen BSS.ShortByteString
arbitraryBSS1 = BSS.pack <$> listOf1 arbitrary


-- | Arbitrary 'ShortByteString' of a given length
arbitraryBSSn :: Int -> Gen BSS.ShortByteString
arbitraryBSSn n = BSS.pack <$> vectorOf n arbitrary


-- | Arbitrary UTCTime that generates dates after 01 Jan 1970 01:00:00 CET
arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
    w <- arbitrary :: Gen Word32
    return $ posixSecondsToUTCTime $ realToFrac w


-- | Generate a Maybe from a Gen a
arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe g =
    frequency
        [ (1, return Nothing)
        , (5, Just <$> g)
        ]


-- | Generate an Network
arbitraryNetwork :: Gen Network
arbitraryNetwork = elements allNets


-- Helpers for creating Serial and JSON Identity tests

data SerialBox
    = forall a.
        (Show a, Eq a, T.Typeable a, Serial a) =>
      SerialBox (Gen a)


data ReadBox
    = forall a.
        (Read a, Show a, Eq a, T.Typeable a) =>
      ReadBox (Gen a)


data JsonBox
    = forall a.
        (Show a, Eq a, T.Typeable a, A.ToJSON a, A.FromJSON a) =>
      JsonBox (Gen a)


data NetBox
    = forall a.
        (Show a, Eq a, T.Typeable a) =>
      NetBox
        ( Network -> a -> A.Value
        , Network -> a -> A.Encoding
        , Network -> A.Value -> A.Parser a
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


-- | Generate binary identity tests
testSerial ::
    (Eq a, Show a, T.Typeable a, Serial a) => Gen a -> Spec
testSerial gen =
    prop ("Binary encoding/decoding identity for " <> name) $
        forAll gen $ \x -> do
            (runGetL deserialize . runPutL . serialize) x `shouldBe` x
            (runGetL deserialize . fromStrict . runPutS . serialize) x `shouldBe` x
            (runGetS deserialize . runPutS . serialize) x `shouldBe` Right x
            (runGetS deserialize . toStrict . runPutL . serialize) x `shouldBe` Right x
  where
    name = show $ T.typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy


-- | Generate Read/Show identity tests
testRead ::
    (Eq a, Read a, Show a, T.Typeable a) => Gen a -> Spec
testRead gen =
    prop ("read/show identity for " <> name) $
        forAll gen $
            \x -> (read . show) x `shouldBe` x
  where
    name = show $ T.typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy


-- | Generate Data.Aeson identity tests
testJson ::
    (Eq a, Show a, T.Typeable a, A.ToJSON a, A.FromJSON a) => Gen a -> Spec
testJson gen = do
    prop ("Data.Aeson toJSON/fromJSON identity for " <> name) $
        forAll gen (`shouldSatisfy` jsonID)
    prop ("Data.Aeson toEncoding/fromJSON identity for " <> name) $
        forAll gen (`shouldSatisfy` encodingID)
  where
    name = show $ T.typeRep $ proxy gen
    proxy :: Gen a -> Proxy a
    proxy = const Proxy
    jsonID x = (A.fromJSON . A.toJSON) (toMap x) == A.Success (toMap x)
    encodingID x =
        (A.decode . A.encodingToLazyByteString . A.toEncoding) (toMap x)
            == Just (toMap x)


-- | Generate Data.Aeson identity tests for type that need the @Network@
testNetJson ::
    (Eq a, Show a, T.Typeable a) =>
    (Network -> a -> A.Value) ->
    (Network -> a -> A.Encoding) ->
    (Network -> A.Value -> A.Parser a) ->
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
    encEnc net = A.encodingToLazyByteString . toMapE . e net
    dec net = A.parseMaybe (p net) . fromMap <=< A.decode
    name = show $ T.typeRep $ proxy j
    proxy :: (Network -> a -> A.Value) -> Proxy a
    proxy = const Proxy


arbitraryNetData :: Arbitrary a => Gen (Network, a)
arbitraryNetData = do
    net <- arbitraryNetwork
    x <- arbitrary
    return (net, x)


genNetData :: Gen a -> Gen (Network, a)
genNetData gen = do
    net <- arbitraryNetwork
    x <- gen
    return (net, x)


toMap :: a -> Map.Map String a
toMap = Map.singleton "object"


toMapE :: A.Encoding -> A.Encoding
toMapE = A.pairs . A.pair "object"


fromMap :: Map.Map String a -> a
fromMap = (Map.! "object")
