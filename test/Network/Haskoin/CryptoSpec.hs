{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.CryptoSpec (spec) where

import           Control.Monad             (forM_, replicateM_)
import           Control.Monad.Trans       (liftIO)
import qualified Crypto.Secp256k1          as EC (SecKey, exportCompactSig)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C (pack)
import           Data.Maybe                (fromJust, isJust, isNothing)
import           Data.Serialize            (encode)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit                (Assertion, assertBool)

-- Unit tests copied from bitcoind implementation
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_tests.cpp

strSecret1 :: ByteString
strSecret1  = "5HxWvvfubhXpYYpS3tJkw6fq9jE9j18THftkZjHHfmFiWtmAbrj"

strSecret2 :: ByteString
strSecret2  = "5KC4ejrDjv152FGwP386VD1i2NYc5KkfSMyv1nGy1VGDxGHqVY3"

strSecret1C :: ByteString
strSecret1C = "Kwr371tjA9u2rFSMZjTNun2PXXP3WPZu2afRHTcta6KxEUdm1vEw"

strSecret2C :: ByteString
strSecret2C = "L3Hq7a8FEQwJkW1M2GNKDW28546Vp5miewcCzSqUD9kCAXrJdS3g"

addr1 :: ByteString
addr1  = "1QFqqMUD55ZV3PJEJZtaKCsQmjLT6JkjvJ"

addr2 :: ByteString
addr2  = "1F5y5E5FMc5YzdJtB9hLaUe43GDxEKXENJ"

addr1C :: ByteString
addr1C = "1NoJrossxPBKfCHuJXT4HadJrXRE9Fxiqs"

addr2C :: ByteString
addr2C = "1CRj2HyM1CXWzHAXLQtiGLyggNT9WQqsDs"

strAddressBad :: ByteString
strAddressBad = "1HV9Lc3sNHZxwj4Zk6fB38tEmBryq2cBiF"

sigMsg :: [ByteString]
sigMsg =
    [ mconcat ["Very secret message ", C.pack (show (i :: Int)), ": 11"]
    | i <- [0..15]
    ]

sec1 :: PrvKey
sec1  = fromJust $ fromWif btc strSecret1

sec2 :: PrvKey
sec2  = fromJust $ fromWif btc strSecret2

sec1C :: PrvKey
sec1C = fromJust $ fromWif btc strSecret1C

sec2C :: PrvKey
sec2C = fromJust $ fromWif btc strSecret2C

pub1 :: PubKey
pub1  = derivePubKey sec1

pub2 :: PubKey
pub2  = derivePubKey sec2

pub1C :: PubKey
pub1C = derivePubKey sec1C

pub2C :: PubKey
pub2C = derivePubKey sec2C

spec :: Spec
spec = do
    describe "ecdsa prng unit tests" $
        it "genPrvKey produces unique keys" uniqueKeys
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

{- ECDSA PRNG unit tests -}

uniqueKeys :: Assertion
uniqueKeys = do
    (k1,k2,k3) <- liftIO $ withSource getEntropy $ do
        a <- genPrvKey
        b <- genPrvKey
        replicateM_ 20 genPrvKey
        c <- genPrvKey
        return (a,b,c)
    assertBool "DiffKey" $ k1 /= k2 && k1 /= k3 && k2 /= k3

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
        decodePrvKey makePrvKeyU bs

checkPrvKeyCompressed :: Assertion
checkPrvKeyCompressed = do
    assertBool "Key 1"  $ not $ prvKeyCompressed sec1
    assertBool "Key 2"  $ not $ prvKeyCompressed sec2
    assertBool "Key 1C" $ prvKeyCompressed sec1C
    assertBool "Key 2C" $ prvKeyCompressed sec2C

checkKeyCompressed :: Assertion
checkKeyCompressed = do
    assertBool "Key 1"  $ not $ pubKeyCompressed pub1
    assertBool "Key 2"  $ not $ pubKeyCompressed pub2
    assertBool "Key 1C" $ pubKeyCompressed pub1C
    assertBool "Key 2C" $ pubKeyCompressed pub2C

checkMatchingAddress :: Assertion
checkMatchingAddress = do
    assertBool "Key 1"  $ Just addr1  == addrToString (pubKeyAddr btc pub1)
    assertBool "Key 2"  $ Just addr2  == addrToString (pubKeyAddr btc pub2)
    assertBool "Key 1C" $ Just addr1C == addrToString (pubKeyAddr btc pub1C)
    assertBool "Key 2C" $ Just addr2C == addrToString (pubKeyAddr btc pub2C)

checkSignatures :: Hash256 -> Assertion
checkSignatures h = do
    let sign1  = signMsg h sec1
        sign2  = signMsg h sec2
        sign1C = signMsg h sec1C
        sign2C = signMsg h sec2C
    assertBool "Key 1, Sign1"   $ verifySig h sign1 pub1
    assertBool "Key 1, Sign2"   $ not $ verifySig h sign2 pub1
    assertBool "Key 1, Sign1C"  $ verifySig h sign1C pub1
    assertBool "Key 1, Sign2C"  $ not $ verifySig h sign2C pub1
    assertBool "Key 2, Sign1"   $ not $ verifySig h sign1 pub2
    assertBool "Key 2, Sign2"   $ verifySig h sign2 pub2
    assertBool "Key 2, Sign1C"  $ not $ verifySig h sign1C pub2
    assertBool "Key 2, Sign2C"  $ verifySig h sign2C pub2
    assertBool "Key 1C, Sign1"  $ verifySig h sign1 pub1C
    assertBool "Key 1C, Sign2"  $ not $ verifySig h sign2 pub1C
    assertBool "Key 1C, Sign1C" $ verifySig h sign1C pub1C
    assertBool "Key 1C, Sign2C" $ not $ verifySig h sign2C pub1C
    assertBool "Key 2C, Sign1"  $ not $ verifySig h sign1 pub2C
    assertBool "Key 2C, Sign2"  $ verifySig h sign2 pub2C
    assertBool "Key 2C, Sign1C" $ not $ verifySig h sign1C pub2C
    assertBool "Key 2C, Sign2C" $ verifySig h sign2C pub2C
