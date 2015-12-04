module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
-- import Test.QuickCheck (verboseCheck)

import Data.String (fromString)
-- import Data.String.Conversions (cs)

import Network.Haskoin.Test
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Data.Function (on)
import Data.String (IsString)

tests :: [Test]
tests =
    [ testGroup "HDW Extended Keys"
        [ testProperty "pubKeyOfSubKeyIsSubKeyOfPubKey / prvSubKey(k,c)*G = pubSubKey(k*G,c)" prop_pubKeyOfSubKeyIsSubKeyOfPubKey
        , testProperty "fromB58 . toB58 prvKey" b58PrvKey
        , testProperty "fromB58 . toB58 pubKey" b58PubKey
        , testProperty "prvIDOfPrvKeyIsPubIdOfPubKey" prop_prvIDOfPrvKeyIsPubIdOfPubKey
        , testProperty "prvFPOfPrvKeyIsPubFPOfPubKey" prop_prvFPOfPrvKeyIsPubFPOfPubKey
        , testProperty "addPrivPriv_pvk1_pvk2_is_addPubPriv_pbk1_pvk2" prop_addPrivPriv_pvk1_pvk2_is_addPubPriv_pbk1_pvk2
        ]
    , testGroup "From/To strings"
        [ testProperty "Read/Show extended public key"    $ \(ArbitraryXPubKey _ k) -> prop_readShowIsId k
        , testProperty "From string extended public key"  $ \(ArbitraryXPubKey _ k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "Read/Show extended private key"   $ \(ArbitraryXPrvKey k) -> prop_readShowIsId k
        , testProperty "From string extended private key" $ \(ArbitraryXPrvKey k) -> prop_fromStringToHaskoinStringIsId  k

        , testProperty "Read/Show derivation path"        $ \(ArbitraryDerivPath p) -> prop_readShowIsId p
        , testProperty "From string derivation path"      $ \(ArbitraryDerivPath k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "From string DerivPath (trailing slash)" $ \(ArbitraryDerivPath k) -> prop_fromStringAddSlashToHaskoinStringIsId k


        , testProperty "Read/Show Bip32Path"        $ \(ArbitraryBip32Path p) -> prop_readShowIsId p
        , testProperty "From string Bip32Path"      $ \(ArbitraryBip32Path k) -> prop_fromStringToHaskoinStringIsId k 
        , testProperty "From string Bip32Path (trailing slash)" $ \(ArbitraryBip32Path k) -> prop_fromStringAddSlashToHaskoinStringIsId k


        , testProperty "Read/Show XKeyChildIndex"        $ \(ArbitraryXKeyChildIndex p) -> prop_readShowIsId p
        , testProperty "From string XKeyChildIndex"      $ \(ArbitraryXKeyChildIndex k) -> prop_fromStringToHaskoinStringIsId k 


        , testProperty "Read/Show hard index"             $ \(ArbitraryXKeyHardIndex p) -> prop_readShowIsId p
        , testProperty "From string hard index"           $ \(ArbitraryXKeyHardIndex k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "Read/Show hard derivation path"   $ \(ArbitraryHardPath p) -> prop_readShowIsId p
        , testProperty "From string hard derivation path" $ \(ArbitraryHardPath k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "From string hard derivation path (trailing slash)" $ \(ArbitraryHardPath k) -> prop_fromStringAddSlashToHaskoinStringIsId k

        , testProperty "Read/Show soft index"             $ \(ArbitraryXKeySoftIndex p) -> prop_readShowIsId p
        , testProperty "From string soft index"           $ \(ArbitraryXKeySoftIndex k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "Read/Show soft derivation path"   $ \(ArbitrarySoftPath p) -> prop_readShowIsId p
        , testProperty "From string soft derivation path" $ \(ArbitrarySoftPath k) -> prop_fromStringToHaskoinStringIsId k
        , testProperty "From string soft derivation path (trailing slash)" $ \(ArbitrarySoftPath k) -> prop_fromStringAddSlashToHaskoinStringIsId k
        ]
    ]

{- HDW Extended Keys -}
-- todo, i should be XKeyChildIndex
-- pubKeyOfSubKeyIsSubKeyOfPubKey :: ArbitraryXPrvKey -> XKeyChildIndex -> Bool
prop_pubKeyOfSubKeyIsSubKeyOfPubKey :: ArbitraryXPrvKey -> ArbitraryXKeySoftIndex -> Bool
prop_pubKeyOfSubKeyIsSubKeyOfPubKey (ArbitraryXPrvKey k) (ArbitraryXKeySoftIndex softIndex) = 
    (deriveXPubKey . (`prvSubKey` softIndex) $ k ) == ( (`pubSubKey` softIndex) . deriveXPubKey $ k)

b58PrvKey :: ArbitraryXPrvKey -> Bool
b58PrvKey (ArbitraryXPrvKey k) = (xPrvImport $ xPrvExport k) == Just k

b58PubKey :: ArbitraryXPubKey -> Bool
b58PubKey (ArbitraryXPubKey _ k) = (xPubImport $ xPubExport k) == Just k

prop_prvIDOfPrvKeyIsPubIdOfPubKey :: ArbitraryXPrvKey -> Bool
prop_prvIDOfPrvKeyIsPubIdOfPubKey (ArbitraryXPrvKey prvKey) = 
  let pubKey = deriveXPubKey $ prvKey
  in  xPubID pubKey == xPrvID prvKey

prop_prvFPOfPrvKeyIsPubFPOfPubKey :: ArbitraryXPrvKey -> Bool
prop_prvFPOfPrvKeyIsPubFPOfPubKey (ArbitraryXPrvKey prvKey) = 
  let pubKey = deriveXPubKey $ prvKey
  in  xPubFP pubKey == xPrvFP prvKey

{- Strings -}

prop_readShowIsId :: (Eq a, Read a, Show a) => a -> Bool
prop_readShowIsId x = ( (==) `on` ($ x) ) (read . show ) id

prop_fromStringToHaskoinStringIsId :: (Eq a, IsString a, ToHaskoinString a) => a -> Bool
prop_fromStringToHaskoinStringIsId x = ( (==) `on` ($ x) ) (fromString . toHaskoinString) id

prop_fromStringAddSlashToHaskoinStringIsId :: (Eq a, IsString a, ToHaskoinString a) => a -> Bool
prop_fromStringAddSlashToHaskoinStringIsId x = ( (==) `on` ($ x) ) (fromString . (++ "/") . toHaskoinString) id

prop_addPrivPriv_pvk1_pvk2_is_addPubPriv_pbk1_pvk2 :: ArbitraryXPrvKey -> ArbitraryHash256 -> Bool
prop_addPrivPriv_pvk1_pvk2_is_addPubPriv_pbk1_pvk2  (ArbitraryXPrvKey prvKey1) ( ArbitraryHash256 pvk2) = 
  let pvkC1 = xPrvKey prvKey1
      pbkC1 = xPubKey . deriveXPubKey $ prvKey1
  in  ( (derivePubKey <$> ) . tweakPrvKeyC pvkC1 $ pvk2) == (tweakPubKeyC pbkC1 $ pvk2)




