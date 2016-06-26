{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Units (tests) where

import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (replicateM_)
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust, isJust, isNothing)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C (pack)
import Data.Serialize (put, runPut)

import qualified Crypto.Secp256k1 as EC (SecKey, exportCompactSig)

import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Internals (PrvKeyI(..), PubKeyI(..), Signature(..))

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
    [ mconcat ["Very secret message ", (C.pack $ show (i :: Int)), ": 11"]
    | i <- [0..15]
    ]

sec1 :: PrvKey
sec1  = fromJust $ fromWif strSecret1

sec2 :: PrvKey
sec2  = fromJust $ fromWif strSecret2

sec1C :: PrvKey
sec1C = fromJust $ fromWif strSecret1C

sec2C :: PrvKey
sec2C = fromJust $ fromWif strSecret2C

pub1 :: PubKey
pub1  = derivePubKey sec1

pub2 :: PubKey
pub2  = derivePubKey sec2

pub1C :: PubKey
pub1C = derivePubKey sec1C

pub2C :: PubKey
pub2C = derivePubKey sec2C

tests :: [Test]
tests =
    [ testGroup "ECDSA PRNG unit tests"
        [ testCase "genPrvKey produces unique keys" uniqueKeys
        ]
    , testGroup "bitcoind /src/test/key_tests.cpp" $
        [ testCase "Decode Valid WIF" checkPrivkey
        , testCase "Decode Invalid WIF" checkInvalidKey
        , testCase "Check private key compression" checkPrvKeyCompressed
        , testCase "Check public key compression" checkKeyCompressed
        , testCase "Check matching address" checkMatchingAddress
        ] ++
        ( map (\x -> (testCase ("Check sig: " ++ (show x))
                (checkSignatures $ doubleHash256 x))) sigMsg )
    , testGroup "Trezor RFC 6979 Test Vectors"
        [ testCase "RFC 6979 Test Vector 1" (testSigning $ detVec !! 0)
        , testCase "RFC 6979 Test Vector 2" (testSigning $ detVec !! 1)
        , testCase "RFC 6979 Test Vector 3" (testSigning $ detVec !! 2)
        , testCase "RFC 6979 Test Vector 4" (testSigning $ detVec !! 3)
        , testCase "RFC 6979 Test Vector 5" (testSigning $ detVec !! 4)
        , testCase "RFC 6979 Test Vector 6" (testSigning $ detVec !! 5)
        , testCase "RFC 6979 Test Vector 7" (testSigning $ detVec !! 6)
        , testCase "RFC 6979 Test Vector 8" (testSigning $ detVec !! 7)
        , testCase "RFC 6979 Test Vector 9" (testSigning $ detVec !! 8)
        , testCase "RFC 6979 Test Vector 10" (testSigning $ detVec !! 9)
        , testCase "RFC 6979 Test Vector 11" (testSigning $ detVec !! 10)
        , testCase "RFC 6979 Test Vector 12" (testSigning $ detVec !! 11)
        ]
    ]

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
    assertBool "Key 1"  $ isJust $ fromWif strSecret1
    assertBool "Key 2"  $ isJust $ fromWif strSecret2
    assertBool "Key 1C" $ isJust $ fromWif strSecret1C
    assertBool "Key 2C" $ isJust $ fromWif strSecret2C

checkInvalidKey :: Assertion
checkInvalidKey =
    assertBool "Bad key" $ isNothing $ fromWif strAddressBad

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
    assertBool "Key 1"  $ addr1  == (addrToBase58 $ pubKeyAddr pub1)
    assertBool "Key 2"  $ addr2  == (addrToBase58 $ pubKeyAddr pub2)
    assertBool "Key 1C" $ addr1C == (addrToBase58 $ pubKeyAddr pub1C)
    assertBool "Key 2C" $ addr2C == (addrToBase58 $ pubKeyAddr pub2C)

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


{- Trezor RFC 6979 Test Vectors -}
-- github.com/trezor/python-ecdsa/blob/master/ecdsa/test_pyecdsa.py

detVec :: [(EC.SecKey, ByteString, ByteString)]
detVec =
    [
      ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "Satoshi Nakamoto"
      , "934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d82442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "All those moments will be lost in time, like tears in rain. Time to die..."
      , "8600dbd41e348fe5c9465ab92d23e3db8b98b873beecd930736488696438cb6b547fe64427496db33bf66019dacbf0039c04199abb0122918601db38a72cfc21"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Satoshi Nakamoto"
      , "fd567d121db66e382991534ada77a6bd3106f0a1098c231e47993447cd6af2d06b39cd0eb1bc8603e159ef5c20a5c8ad685a45b06ce9bebed3f153d10d93bed5"
      )
    , ( "f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181"
      , "Alan Turing"
      , "7063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c58dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea"
      )
    , ( "e91671c46231f833a6406ccbea0e3e392c76c167bac1cb013f6f1013980455c2"
      , "There is a computer disease that anybody who works with computers knows about. It's a very serious disease and it interferes completely with the work. The trouble with computers is that you 'play' with them!"
      , "b552edd27580141f3b2a5463048cb7cd3e047b97c9f98076c32dbdf85a68718b279fa72dd19bfae05577e06c7c0c1900c371fcd5893f7e1d56a37d30174671f6"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "Everything should be made as simple as possible, but not simpler."
      , "33a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c96f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
      , "54c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed07082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Not only is the Universe stranger than we think, it is stranger than we can think."
      , "ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd06fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
      , "c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d375afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
      )
    , ( "69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64"
      , "Computer science is no more about computers than astronomy is about telescopes."
      , "7186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d0de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
      )
    , ( "00000000000000000000000000007246174ab1e92e9149c6e446fe194d072637"
      , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
      , "fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda4870e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
      )
    , ( "000000000000000000000000000000000000000000056916d0f9b31dc9b637f3"
      , "The question of whether computers can think is like the question of whether submarines can swim."
      , "cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf906ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
      )
    ]

testSigning :: (EC.SecKey, ByteString, ByteString) -> Assertion
testSigning (prv, msg, str) = do
    assertBool "RFC 6979 Vector" $ res == fromJust (decodeHex str)
    assertBool "Valid sig" $ verifySig msg' sig (derivePubKey prv')
  where
    sig@(Signature g) = signMsg msg' prv'
    msg' = hash256 msg
    prv' = makePrvKey prv
    compact = EC.exportCompactSig g
    res = runPut $ put compact
