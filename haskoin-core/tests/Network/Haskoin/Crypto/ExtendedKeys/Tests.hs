module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Word (Word32)
import Data.Bits ((.&.))

import Network.Haskoin.Test
import Network.Haskoin.Crypto

tests :: [Test]
tests =
    [ testGroup "HDW Extended Keys"
        [ testProperty "pubkey of subkey is subkey of pubkey / prvSubKey(k,c)*G = pubSubKey(k*G,c)" pubKeyOfSubKeyIsSubKeyOfPubKey
        , testProperty "fromB58 . toB58 prvKey" b58PrvKey
        , testProperty "fromB58 . toB58 pubKey" b58PubKey
        ]
    , testGroup "From/To strings"
        [ testProperty "Read/Show extended public key" testReadShowPubKey
        , testProperty "Read/Show extended private key" testReadShowPrvKey
        , testProperty "Read/Show derivation path" testReadShowDerivPath
        , testProperty "Read/Show hard derivation path" testReadShowHardPath
        , testProperty "Read/Show soft derivation path" testReadShowSoftPath
        , testProperty "From string extended public key" testFromStringPubKey
        , testProperty "From string extended private key" testFromStringPrvKey
        , testProperty "From string derivation path" testFromStringDerivPath
        , testProperty "From string hard derivation path" testFromStringHardPath
        , testProperty "From string soft derivation path" testFromStringSoftPath
        ]
    ]

{- HDW Extended Keys -}

pubKeyOfSubKeyIsSubKeyOfPubKey :: ArbitraryXPrvKey -> Word32 -> Bool
pubKeyOfSubKeyIsSubKeyOfPubKey (ArbitraryXPrvKey k) i =
    (deriveXPubKey $ prvSubKey k i') == (pubSubKey (deriveXPubKey k) i')
  where
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

b58PrvKey :: ArbitraryXPrvKey -> Bool
b58PrvKey (ArbitraryXPrvKey k) = (xPrvImport $ xPrvExport k) == Just k

b58PubKey :: ArbitraryXPubKey -> Bool
b58PubKey (ArbitraryXPubKey _ k) = (xPubImport $ xPubExport k) == Just k

{- Strings -}

testReadShowPubKey :: ArbitraryXPubKey -> Bool
testReadShowPubKey (ArbitraryXPubKey _ k) = read (show k) == k

testReadShowPrvKey :: ArbitraryXPrvKey -> Bool
testReadShowPrvKey (ArbitraryXPrvKey k) = read (show k) == k

testFromStringPubKey :: ArbitraryXPubKey -> Bool
testFromStringPubKey (ArbitraryXPubKey _ k) = fromString (cs $ xPubExport k) == k

testFromStringPrvKey :: ArbitraryXPrvKey -> Bool
testFromStringPrvKey (ArbitraryXPrvKey k) = fromString (cs $ xPrvExport k) == k

testReadShowDerivPath :: ArbitraryDerivPath -> Bool
testReadShowDerivPath (ArbitraryDerivPath p) = read (show p) == p

testReadShowHardPath :: ArbitraryHardPath -> Bool
testReadShowHardPath (ArbitraryHardPath p) = read (show p) == p

testReadShowSoftPath :: ArbitrarySoftPath -> Bool
testReadShowSoftPath (ArbitrarySoftPath p) = read (show p) == p

testFromStringDerivPath :: ArbitraryDerivPath -> Bool
testFromStringDerivPath (ArbitraryDerivPath k) = fromString (cs $ pathToStr k) == k

testFromStringHardPath :: ArbitraryHardPath -> Bool
testFromStringHardPath (ArbitraryHardPath k) = fromString (cs $ pathToStr k) == k

testFromStringSoftPath :: ArbitrarySoftPath -> Bool
testFromStringSoftPath (ArbitrarySoftPath k) = fromString (cs $ pathToStr k) == k
