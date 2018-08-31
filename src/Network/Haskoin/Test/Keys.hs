{-|
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Keys where

import           Data.Bits                        (clearBit)
import           Data.Either                      (fromRight)
import           Data.List                        (foldl')
import           Data.Serialize                   (decode)
import           Data.Word                        (Word32)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Crypto.Signature
import           Network.Haskoin.Keys.Extended
import           Network.Haskoin.Keys.Types
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Util
import           Test.QuickCheck

-- | Arbitrary private key with compressed flag.
arbitrarySecKeyI :: Gen SecKeyI
arbitrarySecKeyI = wrapSecKey <$> arbitrary <*> arbitrary

-- | Arbitrary public key (can be both compressed or uncompressed) with its
-- corresponding private key.
arbitraryKeyPair :: Gen (SecKeyI, PubKeyI)
arbitraryKeyPair = do
    k <- arbitrarySecKeyI
    return (k, derivePubKeyI k)

-- | Arbitrary extended private key.
arbitraryXPrvKey :: Network -> Gen XPrvKey
arbitraryXPrvKey net =
    XPrvKey <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryHash256
            <*> arbitrary
            <*> pure net

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


-- | Arbitrary message hash, private key, nonce and corresponding signature.
-- The signature is generated with a random message, random private key and a
-- random nonce.
arbitrarySignature :: Gen (Hash256, SecKey, Sig)
arbitrarySignature = do
    msg <- arbitraryHash256
    key <- arbitrary
    let sig = signHash key msg
    return (msg, key, sig)
