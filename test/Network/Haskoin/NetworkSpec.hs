{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.NetworkSpec (spec) where

import           Data.ByteString           (ByteString)
import           Data.Maybe                (fromJust)
import           Data.Serialize            (Get, Putter, Serialize, decode,
                                            encode, runGet, runPut)
import           Data.Word                 (Word32)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
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
    describe "serialization of keys and hashes" $ do
        it "encodes and decodes bytestring" $
            property $ forAll arbitraryBS testId
        it "encodes and decodes hash160" $
            property $ forAll arbitraryHash160 testId
        it "encodes and decodes hash256" $
            property $ forAll arbitraryHash256 testId
        it "encodes and decodes hash512" $
            property $ forAll arbitraryHash512 testId
        it "encodes and decodes signature" $
            property $ forAll arbitrarySignature $ testId . lst3
        it "encodes and decodes public key" $
            property $ forAll arbitraryPubKey $ testId . snd
        it "encodes and decodes extended private key" $
            property $
            forAll (arbitraryXPrvKey net) $
            testPutGet (getXPrvKey net) putXPrvKey
    describe "serialization of protocol types" $ do
        it "encodes and decodes varint" $
            property $ forAll arbitraryVarInt testId
        it "encodes and decodes varstring" $
            property $ forAll arbitraryVarString testId
        it "encodes and decodes network address" $
            property $ forAll arbitraryNetworkAddress testId
        it "encodes and decodes invtype" $
            property $ forAll arbitraryInvType testId
        it "encodes and decodes invvector" $
            property $ forAll arbitraryInvVector testId
        it "encodes and decodes inv" $ property $ forAll arbitraryInv1 testId
        it "encodes and decodes version" $
            property $ forAll arbitraryVersion testId
        it "encodes and decodes addr" $ property $ forAll arbitraryAddr1 testId
        it "encodes and decodes alert" $ property $ forAll arbitraryAlert testId
        it "encodes and decodes reject" $
            property $forAll arbitraryReject testId
        it "encodes and decodes getdata" $
            property $ forAll arbitraryGetData testId
        it "encodes and decodes notfound" $
            property $ forAll arbitraryNotFound testId
        it "encodes and decodes ping" $ property $ forAll arbitraryPing testId
        it "encodes and decodes pong" $ property $ forAll arbitraryPong testId
        it "encodes and decodes message command" $
            property $ forAll arbitraryMessageCommand testId
        it "encodes and decodes message header" $
            property $ forAll arbitraryMessageHeader testId
        it "encodes and decodes message" $
            property $
            forAll (arbitraryMessage net) $
            testPutGet (getMessage net) (putMessage net)
    describe "serialization of script types" $ do
        it "encodes and decodes script op" $
            property $ forAll arbitraryScriptOp testId
        it "encodes and decodes script" $
            property $ forAll arbitraryScript testId
    describe "serialization of transaction types" $ do
        it "encodes and decodes tx input" $
            property $ forAll arbitraryTxIn testId
        it "encodes and decodes tx output" $
            property $ forAll (arbitraryTxOut net) testId
        it "encodes and decodes outpoint" $
            property $ forAll arbitraryOutPoint testId
        it "encodes and decodes transaction" $
            property $ forAll (arbitraryTx net) testId
        it "encodes and decodes witness transaction" $
            property $ forAll (arbitraryWitnessTx net) testId
        it "encodes and decodes legacy transaction" $
            property $ forAll (arbitraryLegacyTx net) testId
    describe "serialization of block types" $ do
        it "encodes and decodes block" $
            property $ forAll (arbitraryBlock net) testId
        it "encodes and decodes block header" $
            property $ forAll arbitraryBlockHeader testId
        it "encodes and decodes getblocks" $
            property $ forAll arbitraryGetBlocks testId
        it "encodes and decodes getheaders" $
            property $ forAll arbitraryGetHeaders testId
        it "encodes and decdoes headers" $
            property $ forAll arbitraryHeaders testId
        it "encodes and decodes merklel block" $
            property $ forAll arbitraryMerkleBlock testId
    describe "serialization of bloom types" $ do
        it "encodes and decodes bloom flags" $
            property $ forAll arbitraryBloomFlags testId
        it "encodes and decodes bloom filter" $
            property $ forAll arbitraryBloomFilter $ testId . lst3
        it "encodes and decodes filterload" $
            property $ forAll arbitraryFilterLoad testId
        it "encodes and decodes filteradd" $
            property $ forAll arbitraryFilterAdd testId

bloomFilter :: Word32 -> ByteString -> Assertion
bloomFilter n x = do
    assertBool "Bloom filter doesn't contain vector 1" $ bloomContains f1 v1
    assertBool "Bloom filter contains something it should not" $
        not $ bloomContains f1 v2
    assertBool "Bloom filter doesn't contain vector 3" $ bloomContains f3 v3
    assertBool "Bloom filter doesn't contain vector 4" $ bloomContains f4 v4
    assertBool "Bloom filter serialization is incorrect" $
        encode f4 == bs
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
        encode f2 == bs
  where
    f0 = bloomCreate 2 0.001 0 BloomUpdateAll
    f1 = bloomInsert f0 $ encode p
    f2 = bloomInsert f1 $ encode $ getAddrHash $ pubKeyAddr btc p
    k = fromJust $ fromWif btc "5Kg1gnAjaLfKiwhhPpGS3QfRg2m6awQvaj98JCZBZQ5SuS2F15C"
    p = derivePubKey k
    bs = fromJust $ decodeHex "038fc16b080000000000000001"

testId :: (Serialize a, Eq a) => a -> Bool
testId x = decode (encode x) == Right x

testPutGet :: Eq a => Get a -> Putter a -> a -> Bool
testPutGet g p a = runGet g (runPut (p a)) == Right a
