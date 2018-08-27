module Network.Haskoin.Cereal.Tests (spec) where

import           Data.Serialize
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Network
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net = do
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

testId :: (Serialize a, Eq a) => a -> Bool
testId x = decode (encode x) == Right x

testPutGet :: Eq a => Get a -> Putter a -> a -> Bool
testPutGet g p a = runGet g (runPut (p a)) == Right a
