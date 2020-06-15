{-# LANGUAGE OverloadedStrings #-}
module Haskoin.CryptoSpec (spec) where

import           Control.Monad
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C (pack)
import           Data.Maybe
import           Data.Maybe              (fromMaybe, isJust, isNothing)
import           Data.Serialize          (encode)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Keys
import           Haskoin.Util
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = do
    describe "Bitcoin core vectors /src/test/key_tests.cpp" $ do
        it "Passes WIF decoding tests" testPrivkey
        it "Passes SecKey compression tests" testPrvKeyCompressed
        it "Passes PubKey compression tests" testKeyCompressed
        it "Passes address matching tests" testMatchingAddress
        it "Passes signature verification" testSigs
        it "Passes deterministic signing tests" testDetSigning
    describe "MiniKey vectors" $
        it "Passes MiniKey decoding tests" testMiniKey

testMiniKey :: Assertion
testMiniKey =
    assertEqual "fromMiniKey" (Just res) (go "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy")
  where
    go = fmap (encodeHex . encode . secKeyData) . fromMiniKey
    res = "4c7a9640c72dc2099f23715d0c8a0d8a35f8906e3cab61dd3f78b67bf887c9ab"

-- Unit tests copied from bitcoind implementation
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_tests.cpp

testPrivkey :: Assertion
testPrivkey = do
    assertBool "Key 1"  $ isJust $ fromWif btc strSecret1
    assertBool "Key 2"  $ isJust $ fromWif btc strSecret2
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

sigMsg :: [ByteString]
sigMsg =
    [ mconcat ["Very secret message ", C.pack (show (i :: Int)), ": 11"]
    | i <- [0..15]
    ]

testSignature :: Hash256 -> Assertion
testSignature h = do
    let sign1  = signHash (secKeyData sec1) h
        sign2  = signHash (secKeyData sec2) h
        sign1C = signHash (secKeyData sec1C) h
        sign2C = signHash (secKeyData sec2C) h
    assertBool "Key 1, Sign1"   $ verifyHashSig h sign1 (pubKeyPoint pub1)
    assertBool "Key 1, Sign2"   $ not $ verifyHashSig h sign2 (pubKeyPoint pub1)
    assertBool "Key 1, Sign1C"  $ verifyHashSig h sign1C (pubKeyPoint pub1)
    assertBool "Key 1, Sign2C"  $ not $ verifyHashSig h sign2C (pubKeyPoint pub1)
    assertBool "Key 2, Sign1"   $ not $ verifyHashSig h sign1 (pubKeyPoint pub2)
    assertBool "Key 2, Sign2"   $ verifyHashSig h sign2 (pubKeyPoint pub2)
    assertBool "Key 2, Sign1C"  $ not $ verifyHashSig h sign1C (pubKeyPoint pub2)
    assertBool "Key 2, Sign2C"  $ verifyHashSig h sign2C (pubKeyPoint pub2)
    assertBool "Key 1C, Sign1"  $ verifyHashSig h sign1 (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign2"  $ not $ verifyHashSig h sign2 (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign1C" $ verifyHashSig h sign1C (pubKeyPoint pub1C)
    assertBool "Key 1C, Sign2C" $ not $ verifyHashSig h sign2C (pubKeyPoint pub1C)
    assertBool "Key 2C, Sign1"  $ not $ verifyHashSig h sign1 (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign2"  $ verifyHashSig h sign2 (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign1C" $ not $ verifyHashSig h sign1C (pubKeyPoint pub2C)
    assertBool "Key 2C, Sign2C" $ verifyHashSig h sign2C (pubKeyPoint pub2C)

testDetSigning :: Assertion
testDetSigning = do
    let m = doubleSHA256 ("Very deterministic message" :: ByteString)
    assertEqual
        "Det sig 1"
        (signHash (secKeyData sec1) m)
        (signHash (secKeyData sec1C) m)
    assertEqual
        "Det sig 2"
        (signHash (secKeyData sec2) m)
        (signHash (secKeyData sec2C) m)

strSecret1, strSecret2, strSecret1C, strSecret2C :: Text
strSecret1  = "5HxWvvfubhXpYYpS3tJkw6fq9jE9j18THftkZjHHfmFiWtmAbrj"
strSecret2  = "5KC4ejrDjv152FGwP386VD1i2NYc5KkfSMyv1nGy1VGDxGHqVY3"
strSecret1C = "Kwr371tjA9u2rFSMZjTNun2PXXP3WPZu2afRHTcta6KxEUdm1vEw"
strSecret2C = "L3Hq7a8FEQwJkW1M2GNKDW28546Vp5miewcCzSqUD9kCAXrJdS3g"

sec1, sec2, sec1C, sec2C :: SecKeyI
sec1 = fromJust $ fromWif btc strSecret1
sec2 = fromJust $ fromWif btc strSecret2
sec1C = fromJust $ fromWif btc strSecret1C
sec2C = fromJust $ fromWif btc strSecret2C

addr1, addr2, addr1C, addr2C :: Text
addr1  = "1QFqqMUD55ZV3PJEJZtaKCsQmjLT6JkjvJ"
addr2  = "1F5y5E5FMc5YzdJtB9hLaUe43GDxEKXENJ"
addr1C = "1NoJrossxPBKfCHuJXT4HadJrXRE9Fxiqs"
addr2C = "1CRj2HyM1CXWzHAXLQtiGLyggNT9WQqsDs"

strAddressBad :: Text
strAddressBad = "1HV9Lc3sNHZxwj4Zk6fB38tEmBryq2cBiF"

pub1, pub2, pub1C, pub2C :: PubKeyI
pub1  = derivePubKeyI sec1
pub2  = derivePubKeyI sec2
pub1C = derivePubKeyI sec1C
pub2C = derivePubKeyI sec2C
