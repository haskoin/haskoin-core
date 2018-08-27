module Network.Haskoin.Crypto.ExtendedKeys.Tests (spec) where

import           Data.Bits                 ((.&.))
import           Data.String               (fromString)
import           Data.String.Conversions   (cs)
import           Data.Word                 (Word32)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Hspec
import           Test.QuickCheck hiding ((.&.))

spec :: Network -> Spec
spec net = do
    describe "extended keys" $ do
        it "computes pubkey of a subkey is subkey of the pubkey" $
            property $
            forAll (arbitraryXPrvKey net) pubKeyOfSubKeyIsSubKeyOfPubKey
        it "exports and imports extended private key" $
            property $
            forAll (arbitraryXPrvKey net) $ \k ->
                xPrvImport net (xPrvExport k) == Just k
        it "exports and imports extended public key" $
            property $
            forAll (arbitraryXPubKey net) $ \(_, k) ->
                xPubImport net (xPubExport k) == Just k
        it "show and read derivation path" $
            property $ forAll arbitraryDerivPath $ \p -> read (show p) == p
        it "show and read hard derivation path" $
            property $ forAll arbitraryHardPath $ \p -> read (show p) == p
        it "show and read soft derivation path" $
            property $ forAll arbitrarySoftPath $ \p -> read (show p) == p
        it "from string derivation path" $
            property $
            forAll arbitraryDerivPath $ \p -> fromString (cs $ pathToStr p) == p
        it "from string hard derivation path" $
            property $
            forAll arbitraryHardPath $ \p -> fromString (cs $ pathToStr p) == p
        it "from string soft derivation path" $
            property $
            forAll arbitrarySoftPath $ \p -> fromString (cs $ pathToStr p) == p
        it "from and to lists of derivation paths" $
            property $
            forAll arbitraryDerivPath $ \p -> listToPath (pathToList p) == p
        it "from and to lists of hard derivation paths" $
            property $
            forAll arbitraryHardPath $ \p ->
                toHard (listToPath $ pathToList p) == Just p
        it "from and to lists of soft derivation paths" $
            property $
            forAll arbitrarySoftPath $ \p ->
                toSoft (listToPath $ pathToList p) == Just p
        it "read and show parsed path" $
            property $ forAll arbitraryParsedPath $ \p -> read (show p) == p

pubKeyOfSubKeyIsSubKeyOfPubKey :: XPrvKey -> Word32 -> Bool
pubKeyOfSubKeyIsSubKeyOfPubKey k i =
    deriveXPubKey (prvSubKey k i') == pubSubKey (deriveXPubKey k) i'
  where
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

