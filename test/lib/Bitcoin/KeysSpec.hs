{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.KeysSpec (spec) where

import Bitcoin (getSecKey, secKey)
import Bitcoin.Address (
    addrToText,
    addressToOutput,
    outputAddress,
    pubKeyAddr,
    textToAddr,
 )
import Bitcoin.Constants (allNets, btc, btcRegTest, btcTest)
import Bitcoin.Crypto (
    Hash256,
    SecKey,
    doubleSHA256,
    signHash,
    verifyHashSig,
 )
import Bitcoin.Keys (
    PubKeyI (pubKeyCompressed, pubKeyPoint),
    SecKeyI (secKeyCompressed, secKeyData),
    derivePubKeyI,
    fromMiniKey,
    fromWif,
    toWif,
    wrapSecKey,
 )
import Bitcoin.Orphans ()
import Bitcoin.Script (
    ScriptOutput,
    decodeOutputBS,
    encodeOutputBS,
 )
import Bitcoin.Util (decodeHex, eitherToMaybe, encodeHex)
import qualified Bitcoin.Util as U
import Bitcoin.Util.Arbitrary (
    arbitraryKeyPair,
    arbitraryNetwork,
    arbitrarySecKeyI,
 )
import Bitcoin.UtilSpec (
    JsonBox (..),
    ReadBox (..),
    SerialBox (..),
    readTestFile,
    testIdentity,
 )
import Control.Monad (forM_, (<=<))
import Data.Aeson as A (Object, Value (Bool))
import qualified Data.Aeson.KeyMap as A
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Test.HUnit (
    Assertion,
    assertBool,
    assertEqual,
    assertFailure,
 )
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, forAll)


serialVals :: [SerialBox]
serialVals =
    [ SerialBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]


readVals :: [ReadBox]
readVals =
    [ ReadBox (arbitrary :: Gen SecKey)
    , ReadBox arbitrarySecKeyI
    , ReadBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]


jsonVals :: [JsonBox]
jsonVals =
    [ JsonBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]


spec :: Spec
spec = do
    testIdentity serialVals readVals jsonVals []
    describe "PubKey properties" $ do
        prop "Public key is canonical" $
            forAll arbitraryKeyPair (isCanonicalPubKey . snd)
        prop "Public key fromString identity" $
            forAll arbitraryKeyPair $ \(_, k) ->
                fromString (cs . encodeHex $ U.encodeS k) == k
    describe "SecKey properties" $
        prop "fromWif . toWif identity" $
            forAll arbitraryNetwork $ \net ->
                forAll arbitraryKeyPair $ \(pk, _) ->
                    fromWif net (toWif net pk) == Just pk
    describe "Bitcoin core vectors /src/test/key_tests.cpp" $ do
        it "Passes WIF decoding tests" testPrivkey
        it "Passes SecKey compression tests" testPrvKeyCompressed
        it "Passes PubKey compression tests" testKeyCompressed
        it "Passes address matching tests" testMatchingAddress
        it "Passes signature verification" testSigs
        it "Passes deterministic signing tests" testDetSigning
    describe "MiniKey vectors" $
        it "Passes MiniKey decoding tests" testMiniKey
    describe "key_io_valid.json vectors" $ do
        vectors <- runIO (readTestFile "key_io_valid.json" :: IO [(Text, Text, A.Object)])
        it "Passes the key_io_valid.json vectors" $
            mapM_ testKeyIOValidVector vectors
    describe "key_io_invalid.json vectors" $ do
        vectors <- runIO (readTestFile "key_io_invalid.json" :: IO [[Text]])
        it "Passes the key_io_invalid.json vectors" $
            mapM_ testKeyIOInvalidVector vectors


-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalPubKey
isCanonicalPubKey :: PubKeyI -> Bool
isCanonicalPubKey p =
    not $
        -- Non-canonical public key: too short
        (BSL.length bs < 33)
            ||
            -- Non-canonical public key: invalid length for uncompressed key
            (BSL.index bs 0 == 4 && BSL.length bs /= 65)
            ||
            -- Non-canonical public key: invalid length for compressed key
            (BSL.index bs 0 `elem` [2, 3] && BSL.length bs /= 33)
            ||
            -- Non-canonical public key: compressed nor uncompressed
            (BSL.index bs 0 `notElem` [2, 3, 4])
  where
    bs = Bin.encode p


testMiniKey :: Assertion
testMiniKey =
    assertEqual "fromMiniKey" (Just res) (go "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy")
  where
    go = fmap (encodeHex . getSecKey . secKeyData) . fromMiniKey
    res = "4c7a9640c72dc2099f23715d0c8a0d8a35f8906e3cab61dd3f78b67bf887c9ab"


-- Test vectors from:
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_io_tests.cpp

testKeyIOValidVector :: (Text, Text, A.Object) -> Assertion
testKeyIOValidVector (a, payload, obj)
    | disabled = return () -- There are invalid version 1 bech32 addresses
    | isPrv = do
        -- Test from WIF to SecKey
        let Just isComp = A.lookup "isCompressed" obj >>= getBool
            prvKeyM = fromWif net a
            prvKeyHexM = encodeHex . getSecKey . secKeyData <$> prvKeyM
        assertBool "Valid PrvKey" $ isJust prvKeyM
        assertEqual "Valid compression" (Just isComp) (secKeyCompressed <$> prvKeyM)
        assertEqual "WIF matches payload" (Just payload) prvKeyHexM
        let prvAsPubM = (eitherToMaybe . decodeOutputBS <=< decodeHex) a
        assertBool "PrvKey is invalid ScriptOutput" $ isNothing prvAsPubM
        -- Test from SecKey to WIF
        let secM = secKey =<< decodeHex payload
            wifM = toWif net . wrapSecKey isComp <$> secM
        assertEqual "Payload matches WIF" (Just a) wifM
    | otherwise = do
        -- Test Addr to Script
        let addrM = textToAddr net a
            scriptM = encodeHex . encodeOutputBS . addressToOutput <$> addrM
        assertBool ("Valid Address " <> cs a) $ isJust addrM
        assertEqual "Address matches payload" (Just payload) scriptM
        let pubAsWifM = fromWif net a
            pubAsSecM = secKey =<< decodeHex a
        assertBool "Address is invalid Wif" $ isNothing pubAsWifM
        assertBool "Address is invalid PrvKey" $ isNothing pubAsSecM
        -- Test Script to Addr
        let outM = eitherToMaybe . decodeOutputBS =<< decodeHex payload
            resM = addrToText net =<< outputAddress =<< outM
        assertEqual "Payload matches address" (Just a) resM
  where
    Just isPrv = A.lookup "isPrivkey" obj >>= getBool
    disabled = fromMaybe False $ A.lookup "disabled" obj >>= getBool
    Just chain = A.lookup "chain" obj
    net =
        case chain of
            "main" -> btc
            "test" -> btcTest
            "regtest" -> btcRegTest
            _ -> error "Invalid chain key in key_io_valid.json"
    getBool = \case
        Bool b -> Just b
        _ -> Nothing


testKeyIOInvalidVector :: [Text] -> Assertion
testKeyIOInvalidVector [a] = do
    let wifMs = (`fromWif` a) <$> allNets
        secKeyM = (secKey <=< decodeHex) a :: Maybe SecKey
        scriptM = (eitherToMaybe . decodeOutputBS <=< decodeHex) a :: Maybe ScriptOutput
    assertBool "Payload is invalid WIF" $ all isNothing wifMs
    assertBool "Payload is invalid SecKey" $ isNothing secKeyM
    assertBool "Payload is invalid Script" $ isNothing scriptM
testKeyIOInvalidVector _ = assertFailure "Invalid test vector"


-- Test vectors from:
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_tests.cpp

testPrivkey :: Assertion
testPrivkey = do
    assertBool "Key 1" $ isJust $ fromWif btc strSecret1
    assertBool "Key 2" $ isJust $ fromWif btc strSecret2
    assertBool "Key 1C" $ isJust $ fromWif btc strSecret1C
    assertBool "Key 2C" $ isJust $ fromWif btc strSecret2C
    assertBool "Bad key" $ isNothing $ fromWif btc strAddressBad


testPrvKeyCompressed :: Assertion
testPrvKeyCompressed = do
    assertBool "Key 1" $ not $ secKeyCompressed sec1
    assertBool "Key 2" $ not $ secKeyCompressed sec2
    assertBool "Key 1C" $ secKeyCompressed sec1C
    assertBool "Key 2C" $ secKeyCompressed sec2C


testKeyCompressed :: Assertion
testKeyCompressed = do
    assertBool "Key 1" $ not $ pubKeyCompressed pub1
    assertBool "Key 2" $ not $ pubKeyCompressed pub2
    assertBool "Key 1C" $ pubKeyCompressed pub1C
    assertBool "Key 2C" $ pubKeyCompressed pub2C


testMatchingAddress :: Assertion
testMatchingAddress = do
    assertEqual "Key 1" (Just addr1) $ addrToText btc (pubKeyAddr pub1)
    assertEqual "Key 2" (Just addr2) $ addrToText btc (pubKeyAddr pub2)
    assertEqual "Key 1C" (Just addr1C) $ addrToText btc (pubKeyAddr pub1C)
    assertEqual "Key 2C" (Just addr2C) $ addrToText btc (pubKeyAddr pub2C)


testSigs :: Assertion
testSigs = forM_ sigMsg $ testSignature . doubleSHA256


sigMsg :: [BS.ByteString]
sigMsg =
    [ mconcat ["Very secret message ", C.pack (show (i :: Int)), ": 11"]
    | i <- [0 .. 15]
    ]


testSignature :: Hash256 -> Assertion
testSignature h = do
    let sign1 = signHash (secKeyData sec1) h
        sign2 = signHash (secKeyData sec2) h
        sign1C = signHash (secKeyData sec1C) h
        sign2C = signHash (secKeyData sec2C) h
    assertBool "Key 1, Sign1" $ verifyHashSig h sign1 (pubKeyPoint pub1)
    assertBool "Key 1, Sign2" $ not $ verifyHashSig h sign2 (pubKeyPoint pub1)
    assertBool "Key 1, Sign1C" $ verifyHashSig h sign1C (pubKeyPoint pub1)
    assertBool "Key 1, Sign2C" $ not $ verifyHashSig h sign2C (pubKeyPoint pub1)
    assertBool "Key 2, Sign1" $ not $ verifyHashSig h sign1 (pubKeyPoint pub2)
    assertBool "Key 2, Sign2" $ verifyHashSig h sign2 (pubKeyPoint pub2)
    assertBool "Key 2, Sign1C" $ not $ verifyHashSig h sign1C (pubKeyPoint pub2)
    assertBool "Key 2, Sign2C" $ verifyHashSig h sign2C (pubKeyPoint pub2)
    assertBool "Key 1C, Sign1" $ verifyHashSig h sign1 (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign2" $ not $ verifyHashSig h sign2 (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign1C" $ verifyHashSig h sign1C (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign2C" $ not $ verifyHashSig h sign2C (pubKeyPoint pub1C)
    assertBool "Key 2C, Sign1" $ not $ verifyHashSig h sign1 (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign2" $ verifyHashSig h sign2 (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign1C" $ not $ verifyHashSig h sign1C (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign2C" $ verifyHashSig h sign2C (pubKeyPoint pub2C)


testDetSigning :: Assertion
testDetSigning = do
    let m = doubleSHA256 ("Very deterministic message" :: BS.ByteString)
    assertEqual
        "Det sig 1"
        (signHash (secKeyData sec1) m)
        (signHash (secKeyData sec1C) m)
    assertEqual
        "Det sig 2"
        (signHash (secKeyData sec2) m)
        (signHash (secKeyData sec2C) m)


strSecret1, strSecret2, strSecret1C, strSecret2C :: Text
strSecret1 = "5HxWvvfubhXpYYpS3tJkw6fq9jE9j18THftkZjHHfmFiWtmAbrj"
strSecret2 = "5KC4ejrDjv152FGwP386VD1i2NYc5KkfSMyv1nGy1VGDxGHqVY3"
strSecret1C = "Kwr371tjA9u2rFSMZjTNun2PXXP3WPZu2afRHTcta6KxEUdm1vEw"
strSecret2C = "L3Hq7a8FEQwJkW1M2GNKDW28546Vp5miewcCzSqUD9kCAXrJdS3g"


sec1, sec2, sec1C, sec2C :: SecKeyI
sec1 = fromJust $ fromWif btc strSecret1
sec2 = fromJust $ fromWif btc strSecret2
sec1C = fromJust $ fromWif btc strSecret1C
sec2C = fromJust $ fromWif btc strSecret2C


addr1, addr2, addr1C, addr2C :: Text
addr1 = "1QFqqMUD55ZV3PJEJZtaKCsQmjLT6JkjvJ"
addr2 = "1F5y5E5FMc5YzdJtB9hLaUe43GDxEKXENJ"
addr1C = "1NoJrossxPBKfCHuJXT4HadJrXRE9Fxiqs"
addr2C = "1CRj2HyM1CXWzHAXLQtiGLyggNT9WQqsDs"


strAddressBad :: Text
strAddressBad = "1HV9Lc3sNHZxwj4Zk6fB38tEmBryq2cBiF"


pub1, pub2, pub1C, pub2C :: PubKeyI
pub1 = derivePubKeyI sec1
pub2 = derivePubKeyI sec2
pub1C = derivePubKeyI sec1C
pub2C = derivePubKeyI sec2C
