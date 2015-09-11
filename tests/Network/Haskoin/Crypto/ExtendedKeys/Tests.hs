module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word (Word32)
import Data.Bits ((.&.))

import Network.Haskoin.Test
import Network.Haskoin.Crypto

tests :: [Test]
tests =
    [ testGroup "HDW Extended Keys"
        [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" subkeyTest
        , testProperty "fromB58 . toB58 prvKey" b58PrvKey
        , testProperty "fromB58 . toB58 pubKey" b58PubKey
        ]
    ]

{- HDW Extended Keys -}

subkeyTest :: ArbitraryXPrvKey -> Word32 -> Bool
subkeyTest (ArbitraryXPrvKey k) i =
    (deriveXPubKey $ prvSubKey k i') == (pubSubKey (deriveXPubKey k) i')
  where
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

b58PrvKey :: ArbitraryXPrvKey -> Bool
b58PrvKey (ArbitraryXPrvKey k) = (xPrvImport $ xPrvExport k) == Just k

b58PubKey :: ArbitraryXPubKey -> Bool
b58PubKey (ArbitraryXPubKey _ k) = (xPubImport $ xPubExport k) == Just k

