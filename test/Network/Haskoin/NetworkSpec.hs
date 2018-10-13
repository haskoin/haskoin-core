{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.NetworkSpec (spec) where

import           Data.Maybe                (fromJust)
import           Data.Serialize            as S
import           Data.Text                 (Text)
import           Data.Word                 (Word32)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Network
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit                (Assertion, assertBool)
import           Test.QuickCheck

spec :: Spec
spec = do
    let net = btc
    describe "bloom filters" $ do
        it "bloom filter vector 1" bloomFilter1
        it "bloom filter vector 2" bloomFilter2
        it "bloom filter vector 3" bloomFilter3
    describe "serialization of protocol types" $ do
        it "encodes and decodes varint" $
            property $ forAll arbitraryVarInt cerealID
        it "encodes and decodes varstring" $
            property $ forAll arbitraryVarString cerealID
        it "encodes and decodes network address" $
            property $ forAll arbitraryNetworkAddress cerealID
        it "encodes and decodes invtype" $
            property $ forAll arbitraryInvType cerealID
        it "encodes and decodes invvector" $
            property $ forAll arbitraryInvVector cerealID
        it "encodes and decodes inv" $ property $ forAll arbitraryInv1 cerealID
        it "encodes and decodes version" $
            property $ forAll arbitraryVersion cerealID
        it "encodes and decodes addr" $ property $ forAll arbitraryAddr1 cerealID
        it "encodes and decodes alert" $ property $ forAll arbitraryAlert cerealID
        it "encodes and decodes reject" $
            property $forAll arbitraryReject cerealID
        it "encodes and decodes getdata" $
            property $ forAll arbitraryGetData cerealID
        it "encodes and decodes notfound" $
            property $ forAll arbitraryNotFound cerealID
        it "encodes and decodes ping" $ property $ forAll arbitraryPing cerealID
        it "encodes and decodes pong" $ property $ forAll arbitraryPong cerealID
        it "encodes and decodes message command" $
            property $ forAll arbitraryMessageCommand cerealID
        it "encodes and decodes message header" $
            property $ forAll arbitraryMessageHeader cerealID
        it "encodes and decodes message" $
            property $
            forAll (arbitraryMessage net) $
            testPutGet (getMessage net) (putMessage net)
    describe "serialization of bloom types" $ do
        it "encodes and decodes bloom flags" $
            property $ forAll arbitraryBloomFlags cerealID
        it "encodes and decodes bloom filter" $
            property $ forAll arbitraryBloomFilter $ cerealID . lst3
        it "encodes and decodes filterload" $
            property $ forAll arbitraryFilterLoad cerealID
        it "encodes and decodes filteradd" $
            property $ forAll arbitraryFilterAdd cerealID

bloomFilter :: Word32 -> Text -> Assertion
bloomFilter n x = do
    assertBool "Bloom filter doesn't contain vector 1" $ bloomContains f1 v1
    assertBool "Bloom filter contains something it should not" $
        not $ bloomContains f1 v2
    assertBool "Bloom filter doesn't contain vector 3" $ bloomContains f3 v3
    assertBool "Bloom filter doesn't contain vector 4" $ bloomContains f4 v4
    assertBool "Bloom filter serialization is incorrect" $
        S.encode f4 == bs
  where
    f0 = bloomCreate 3 0.01 n BloomUpdateAll
    f1 = bloomInsert f0 v1
    f3 = bloomInsert f1 v3
    f4 = bloomInsert f3 v4
    v1 = fromJust $ decodeHex "99108ad8ed9bb6274d3980bab5a85c048f0950c8"
    v2 = fromJust $ decodeHex "19108ad8ed9bb6274d3980bab5a85c048f0950c8"
    v3 = fromJust $ decodeHex "b5a2c786d9ef4658287ced5914b37a1b4aa32eee"
    v4 = fromJust $ decodeHex "b9300670b4c5366e95b2699e8b18bc75e5f729c5"
    bs = fromJust $ decodeHex x

bloomFilter1 :: Assertion
bloomFilter1 = bloomFilter 0 "03614e9b050000000000000001"

bloomFilter2 :: Assertion
bloomFilter2 = bloomFilter 2147483649 "03ce4299050000000100008001"

bloomFilter3 :: Assertion
bloomFilter3 =
    assertBool "Bloom filter serialization is incorrect" $
        S.encode f2 == bs
  where
    f0 = bloomCreate 2 0.001 0 BloomUpdateAll
    f1 = bloomInsert f0 $ S.encode p
    f2 = bloomInsert f1 $ S.encode $ getAddrHash160 $ pubKeyAddr p
    k = fromJust $ fromWif btc "5Kg1gnAjaLfKiwhhPpGS3QfRg2m6awQvaj98JCZBZQ5SuS2F15C"
    p = derivePubKeyI k
    bs = fromJust $ decodeHex "038fc16b080000000000000001"

cerealID :: (Serialize a, Eq a) => a -> Bool
cerealID x = S.decode (S.encode x) == Right x

testPutGet :: Eq a => Get a -> Putter a -> a -> Bool
testPutGet g p a = runGet g (runPut (p a)) == Right a
