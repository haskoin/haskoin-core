module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import           Data.Bits                            ((.&.))
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Data.Word                            (Word32)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck                      (forAll)

tests :: [Test]
tests =
    [ testGroup
          "HDW Extended Keys"
          [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" $
            forAll arbitraryXPrvKey pubKeyOfSubKeyIsSubKeyOfPubKey
          , testProperty "fromB58 . toB58 prvKey" $ forAll arbitraryXPrvKey $
            \k -> xPrvImport (xPrvExport k) == Just k
          , testProperty "fromB58 . toB58 pubKey" $ forAll arbitraryXPubKey $
            \(_, k) -> xPubImport (xPubExport k) == Just k
          ]
    , testGroup
          "From/To strings"
          [ testProperty "Read/Show extended public key" $ forAll arbitraryXPubKey $
            \(_, k) -> read (show k) == k
          , testProperty "Read/Show extended private key" $ forAll arbitraryXPrvKey $
            \k -> read (show k) == k
          , testProperty "From string extended public key" $ forAll arbitraryXPubKey $
            \(_, k) -> fromString (cs $ xPubExport k) == k
          , testProperty "From string extended private key" $ forAll arbitraryXPrvKey $
            \k -> fromString (cs $ xPrvExport k) == k
          , testProperty "Read/Show derivation path" $ forAll arbitraryDerivPath $
            \p -> read (show p) == p
          , testProperty "Read/Show hard derivation path" $ forAll arbitraryHardPath $
            \p -> read (show p) == p
          , testProperty "Read/Show soft derivation path" $ forAll arbitrarySoftPath $
            \p -> read (show p) == p
          , testProperty "From string derivation path" $ forAll arbitraryDerivPath $
            \p -> fromString (cs $ pathToStr p) == p
          , testProperty "From string hard derivation path" $ forAll arbitraryHardPath $
            \p -> fromString (cs $ pathToStr p) == p
          , testProperty "From string soft derivation path" $ forAll arbitrarySoftPath $
            \p -> fromString (cs $ pathToStr p) == p
          , testProperty "listToPath . pathToList == id" $ forAll arbitraryDerivPath $
            \p -> listToPath (pathToList p) == p
          , testProperty "listToPath . pathToList == id (Hard)" $
            forAll arbitraryHardPath $ \p -> toHard (listToPath $ pathToList p) == Just p
          , testProperty "listToPath . pathToList == id (Soft)" $
            forAll arbitrarySoftPath $ \p -> toSoft (listToPath $ pathToList p) == Just p
          , testProperty "read . show == id (ParsedPath)" $
            forAll arbitraryParsedPath $ \p -> read (show p) == p
          ]
    ]

pubKeyOfSubKeyIsSubKeyOfPubKey :: XPrvKey -> Word32 -> Bool
pubKeyOfSubKeyIsSubKeyOfPubKey k i =
    deriveXPubKey (prvSubKey k i') == pubSubKey (deriveXPubKey k) i'
  where
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

