module Network.Haskoin.Json.Tests
    ( spec
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict       (singleton)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net = do
    describe "serialize & de-serialize haskoin types to json" $ do
        it "encodes and decodes script output" $
            forAll (arbitraryScriptOutput net) testID
        it "encodes and decodes outpoint" $ forAll arbitraryOutPoint testID
        it "encodes and decodes address" $
            forAll (arbitraryAddress net) (testCustom (addrFromJSON net))
        it "encodes and decodes transaction" $ forAll (arbitraryTx net) testID
        it "encodes and decodes transaction hash" $
            forAll arbitraryTxHash testID
        it "encodes and decodes block hash" $ forAll arbitraryBlockHash testID
        it "encodes and decodes sighash" $ forAll arbitrarySigHash testID
        it "encodes and decodes siginput" $
            forAll (arbitrarySigInput net) (testID . fst)
        it "encodes and decodes public key" $
            forAll arbitraryPubKey (testID . snd)
        it "encodes and decodes compressed public key" $
            forAll arbitraryPubKeyC (testID . snd)
        it "encodes and decodes uncompressed public key" $
            forAll arbitraryPubKeyU (testID . snd)
        it "encodes and decodes extended private key" $
            forAll (arbitraryXPrvKey net) (testCustom (xPrvFromJSON net))
        it "encodes and decodes extended public key" $
            forAll (arbitraryXPubKey net) (testCustom (xPubFromJSON net) . snd)
        it "encodes and decodes derivation path" $
            forAll arbitraryDerivPath testID
        it "encodes and decodes parsed derivation path" $
            forAll arbitraryParsedPath testID

testID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testID x =
    (decode . encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)

testCustom :: (ToJSON a, Eq a) => (Value -> Parser a) -> a -> Bool
testCustom f x = parseMaybe f (toJSON x) == Just x
