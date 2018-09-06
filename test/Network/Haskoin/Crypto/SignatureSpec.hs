{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.SignatureSpec (spec) where

import           Data.Bits                 (testBit)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS (index, length)
import           Data.Maybe
import           Data.Serialize            as S
import           Data.Text                 (Text)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit                (Assertion, assertBool)
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "signatures" $ do
        it "verify signature" $
            property $
            forAll arbitrarySignature $ \(msg, key, sig) ->
                verifyHashSig msg sig (derivePubKey key)
        it "s component less than half order" $
            property $ forAll arbitrarySignature $ isCanonicalHalfOrder . lst3
        it "encoded signature is canonical" $
            property $ forAll arbitrarySignature $ testIsCanonical . lst3
        it "encode signature and decode strictly" $
            property $
            forAll arbitrarySignature $
            (\s -> decodeStrictSig (exportSig s) == Just s) . lst3
        it "encodes and decodes signature" $
            property $
            forAll arbitrarySignature $
            (\s -> decodeStrictSig (exportSig s) == Just s) . lst3
    describe "trezor rfc6979 test vectors" $ do
        it "rfc6979 test vector 1" (testSigning $ head detVec)
        it "rfc6979 test vector 2" (testSigning $ detVec !! 1)
        it "rfc6979 test vector 3" (testSigning $ detVec !! 2)
        it "rfc6979 test vector 4" (testSigning $ detVec !! 3)
        it "rfc6979 test vector 5" (testSigning $ detVec !! 4)
        it "rfc6979 test vector 6" (testSigning $ detVec !! 5)
        it "rfc6979 test vector 7" (testSigning $ detVec !! 6)
        it "rfc6979 test vector 8" (testSigning $ detVec !! 7)
        it "rfc6979 test vector 9" (testSigning $ detVec !! 8)
        it "rfc6979 test vector 10" (testSigning $ detVec !! 9)
        it "rfc6979 test vector 11" (testSigning $ detVec !! 10)
        it "rfc6979 test vector 12" (testSigning $ detVec !! 11)

testSigning :: (SecKey, ByteString, Text) -> Assertion
testSigning (prv, msg, str) = do
    assertBool "RFC 6979 Vector" $ res == fromJust (decodeHex str)
    assertBool "valid sig" $ verifyHashSig msg' g (derivePubKey prv)
  where
    g = signHash prv msg'
    msg' = sha256 msg
    compact = exportCompactSig g
    res = encode compact


{- ECDSA Canonical -}

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
testIsCanonical :: Sig -> Bool
testIsCanonical sig = not $
    -- Non-canonical signature: too short
    (len < 8) ||
    -- Non-canonical signature: too long
    (len > 72) ||
    -- Non-canonical signature: wrong type
    (BS.index s 0 /= 0x30) ||
    -- Non-canonical signature: wrong length marker
    (BS.index s 1 /= len - 2) ||
    -- Non-canonical signature: S length misplaced
    (5 + rlen >= len) ||
    -- Non-canonical signature: R+S length mismatch
    (rlen + slen + 6 /= len) ||
    -- Non-canonical signature: R value type mismatch
    (BS.index s 2 /= 0x02) ||
    -- Non-canonical signature: R length is zero
    (rlen == 0) ||
    -- Non-canonical signature: R value negative
    testBit (BS.index s 4) 7 ||
    -- Non-canonical signature: R value excessively padded
    (  rlen > 1
    && BS.index s 4 == 0
    && not (testBit (BS.index s 5) 7)
    ) ||
    -- Non-canonical signature: S value type mismatch
    (BS.index s (fromIntegral rlen + 4) /= 0x02) ||
    -- Non-canonical signature: S length is zero
    (slen == 0) ||
    -- Non-canonical signature: S value negative
    testBit (BS.index s (fromIntegral rlen+6)) 7 ||
    -- Non-canonical signature: S value excessively padded
    (  slen > 1
    && BS.index s (fromIntegral rlen + 6) == 0
    && not (testBit (BS.index s (fromIntegral rlen + 7)) 7)
    )
  where
    s = exportSig sig
    len = fromIntegral $ BS.length s
    rlen = BS.index s 3
    slen = BS.index s (fromIntegral rlen + 5)


{- Trezor RFC 6979 Test Vectors -}
-- github.com/trezor/python-ecdsa/blob/master/ecdsa/test_pyecdsa.py

detVec :: [(SecKey, ByteString, Text)]
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
