{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.CryptoSpec (spec) where

import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C (pack)
import           Data.Maybe                (fromMaybe, isJust, isNothing)
import           Data.Text                 (Text)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit                (Assertion, assertBool)

-- Unit tests copied from bitcoind implementation
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_tests.cpp

strSecret1 :: Text
strSecret1  = "5HxWvvfubhXpYYpS3tJkw6fq9jE9j18THftkZjHHfmFiWtmAbrj"

strSecret2 :: Text
strSecret2  = "5KC4ejrDjv152FGwP386VD1i2NYc5KkfSMyv1nGy1VGDxGHqVY3"

strSecret1C :: Text
strSecret1C = "Kwr371tjA9u2rFSMZjTNun2PXXP3WPZu2afRHTcta6KxEUdm1vEw"

strSecret2C :: Text
strSecret2C = "L3Hq7a8FEQwJkW1M2GNKDW28546Vp5miewcCzSqUD9kCAXrJdS3g"

addr1 :: Text
addr1  = "1QFqqMUD55ZV3PJEJZtaKCsQmjLT6JkjvJ"

addr2 :: Text
addr2  = "1F5y5E5FMc5YzdJtB9hLaUe43GDxEKXENJ"

addr1C :: Text
addr1C = "1NoJrossxPBKfCHuJXT4HadJrXRE9Fxiqs"

addr2C :: Text
addr2C = "1CRj2HyM1CXWzHAXLQtiGLyggNT9WQqsDs"

strAddressBad :: Text
strAddressBad = "1HV9Lc3sNHZxwj4Zk6fB38tEmBryq2cBiF"

sigMsg :: [ByteString]
sigMsg =
    [ mconcat ["Very secret message ", C.pack (show (i :: Int)), ": 11"]
    | i <- [0..15]
    ]

sec1 :: SecKeyI
sec1 =
    fromMaybe (error "Could not decode WIF secret 1") (fromWif btc strSecret1)

sec2 :: SecKeyI
sec2 =
    fromMaybe (error "Could not decode WIF secret 2") (fromWif btc strSecret2)

sec1C :: SecKeyI
sec1C =
    fromMaybe
        (error "Could not decode WIF compressed secret 1")
        (fromWif btc strSecret1C)

sec2C :: SecKeyI
sec2C =
    fromMaybe
        (error "Could not decode WIF compressed secret 2")
        (fromWif btc strSecret2C)

pub1 :: PubKeyI
pub1  = derivePubKeyI sec1

pub2 :: PubKeyI
pub2  = derivePubKeyI sec2

pub1C :: PubKeyI
pub1C = derivePubKeyI sec1C

pub2C :: PubKeyI
pub2C = derivePubKeyI sec2C

spec :: Spec
spec =
    describe "bitcoind /src/test/key_tests.cpp" $ do
        it "decode valid wif" checkPrivkey
        it "decode invalid wif" checkInvalidKey
        it "decode minikey format" checkMiniKey
        it "check private key compression" checkPrvKeyCompressed
        it "check public key compression" checkKeyCompressed
        it "check matching address" checkMatchingAddress
        it "check various signatures" sigCheck

sigCheck :: Assertion
sigCheck = forM_ sigMsg $ checkSignatures . doubleSHA256

{- bitcoind /src/test/key_tests.cpp -}

checkPrivkey :: Assertion
checkPrivkey = do
    assertBool "Key 1"  $ isJust $ fromWif btc strSecret1
    assertBool "Key 2"  $ isJust $ fromWif btc strSecret2
    assertBool "Key 1C" $ isJust $ fromWif btc strSecret1C
    assertBool "Key 2C" $ isJust $ fromWif btc strSecret2C

checkInvalidKey :: Assertion
checkInvalidKey =
    assertBool "Bad key" $ isNothing $ fromWif btc strAddressBad

checkMiniKey :: Assertion
checkMiniKey =
    assertBool "Bad mini key" $
    isJust res && fromMiniKey "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy" == res
  where
    res = do
        bs <-
            decodeHex
                "4C7A9640C72DC2099F23715D0C8A0D8A35F8906E3CAB61DD3F78B67BF887C9AB"
        wrapSecKey False <$> secKey bs

checkPrvKeyCompressed :: Assertion
checkPrvKeyCompressed = do
    assertBool "Key 1"  $ not $ secKeyCompressed sec1
    assertBool "Key 2"  $ not $ secKeyCompressed sec2
    assertBool "Key 1C" $ secKeyCompressed sec1C
    assertBool "Key 2C" $ secKeyCompressed sec2C

checkKeyCompressed :: Assertion
checkKeyCompressed = do
    assertBool "Key 1"  $ not $ pubKeyCompressed pub1
    assertBool "Key 2"  $ not $ pubKeyCompressed pub2
    assertBool "Key 1C" $ pubKeyCompressed pub1C
    assertBool "Key 2C" $ pubKeyCompressed pub2C

checkMatchingAddress :: Assertion
checkMatchingAddress = do
    assertBool "Key 1"  $ Just addr1  == addrToString btc (pubKeyAddr pub1)
    assertBool "Key 2"  $ Just addr2  == addrToString btc (pubKeyAddr pub2)
    assertBool "Key 1C" $ Just addr1C == addrToString btc (pubKeyAddr pub1C)
    assertBool "Key 2C" $ Just addr2C == addrToString btc (pubKeyAddr pub2C)

checkSignatures :: Hash256 -> Assertion
checkSignatures h = do
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
