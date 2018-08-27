{-|
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto where

import           Crypto.Secp256k1                    ()
import           Data.Bits                           (clearBit)
import           Data.Either                         (fromRight)
import           Data.List                           (foldl')
import           Data.Serialize                      (decode)
import           Data.Word                           (Word32)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Address
import           Network.Haskoin.Crypto.ECDSA
import           Network.Haskoin.Crypto.ExtendedKeys
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Crypto.Keys
import           Network.Haskoin.Test.Util
import           Test.QuickCheck

arbitraryHash160 :: Gen Hash160
arbitraryHash160 =
    (fromRight (error "Could not decode Hash160") . decode) <$> arbitraryBSn 20

arbitraryHash256 :: Gen Hash256
arbitraryHash256 =
    (fromRight (error "Could not decode Hash256") . decode) <$> arbitraryBSn 32

arbitraryHash512 :: Gen Hash512
arbitraryHash512 =
    (fromRight (error "Could not decode Hash512") . decode) <$> arbitraryBSn 64

arbitraryCheckSum32 :: Gen CheckSum32
arbitraryCheckSum32 =
    (fromRight (error "Could not decode CheckSum32") . decode) <$>
    arbitraryBSn 4

arbitraryPrvKey :: Gen PrvKey
arbitraryPrvKey = oneof [ toPrvKeyG <$> arbitraryPrvKeyC
                        , toPrvKeyG <$> arbitraryPrvKeyU
                        ]

-- | Arbitrary compressed private key
arbitraryPrvKeyC :: Gen PrvKeyC
arbitraryPrvKeyC = makePrvKeyC <$> arbitrary

-- | Arbitrary uncompressed private key
arbitraryPrvKeyU :: Gen PrvKeyU
arbitraryPrvKeyU = makePrvKeyU <$> arbitrary

-- | Arbitrary public key (can be both compressed or uncompressed) with its
-- corresponding private key.
arbitraryPubKey :: Gen (PrvKey, PubKey)
arbitraryPubKey =
    oneof [ f <$> arbitraryPubKeyC
          , f <$> arbitraryPubKeyU
          ]
  where
    f (k, p) = (toPrvKeyG k, toPubKeyG p)

-- | Arbitrary compressed public key with its corresponding private key.
arbitraryPubKeyC :: Gen (PrvKeyC, PubKeyC)
arbitraryPubKeyC = (\k -> (k, derivePubKey k)) <$> arbitraryPrvKeyC

-- | Arbitrary uncompressed public key with its corresponding private key.
arbitraryPubKeyU :: Gen (PrvKeyU, PubKeyU)
arbitraryPubKeyU = (\k -> (k, derivePubKey k)) <$> arbitraryPrvKeyU

-- | Arbitrary non-witness address (can be a pubkey or script hash address)
arbitraryAddress :: Network -> Gen Address
arbitraryAddress net =
    oneof [arbitraryPubKeyAddress net, arbitraryScriptAddress net]

-- | Arbitrary public key hash address
arbitraryPubKeyAddress :: Network -> Gen Address
arbitraryPubKeyAddress net = PubKeyAddress <$> arbitraryHash160 <*> pure net

-- | Arbitrary script hash address
arbitraryScriptAddress :: Network -> Gen Address
arbitraryScriptAddress net = ScriptAddress <$> arbitraryHash160 <*> pure net

-- | Arbitrary message hash, private key, nonce and corresponding signature.
-- The signature is generated with a random message, random private key and a
-- random nonce.
arbitrarySignature :: Gen (Hash256, PrvKey, Signature)
arbitrarySignature = do
    msg <- arbitraryHash256
    key <- arbitraryPrvKey
    let sig = signMsg msg key
    return (msg, key, sig)

-- | Arbitrary extended private key.
arbitraryXPrvKey :: Network -> Gen XPrvKey
arbitraryXPrvKey net =
    XPrvKey <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryHash256 <*>
    arbitraryPrvKeyC <*>
    pure net

-- | Arbitrary extended public key with its corresponding private key.
arbitraryXPubKey :: Network -> Gen (XPrvKey, XPubKey)
arbitraryXPubKey net = (\k -> (k, deriveXPubKey k)) <$> arbitraryXPrvKey net

{- Custom derivations -}

genIndex :: Gen Word32
genIndex = (`clearBit` 31) <$> arbitrary

arbitraryBip32PathIndex :: Gen Bip32PathIndex
arbitraryBip32PathIndex =
    oneof [ Bip32SoftIndex <$> genIndex
          , Bip32HardIndex <$> genIndex
          ]

arbitraryHardPath :: Gen HardPath
arbitraryHardPath = foldl' (:|) Deriv <$> listOf genIndex


arbitrarySoftPath :: Gen SoftPath
arbitrarySoftPath = foldl' (:/) Deriv <$> listOf genIndex

arbitraryDerivPath :: Gen DerivPath
arbitraryDerivPath = concatBip32Segments <$> listOf arbitraryBip32PathIndex

arbitraryParsedPath :: Gen ParsedPath
arbitraryParsedPath =
    oneof [ ParsedPrv <$> arbitraryDerivPath
          , ParsedPub <$> arbitraryDerivPath
          , ParsedEmpty <$> arbitraryDerivPath
          ]

