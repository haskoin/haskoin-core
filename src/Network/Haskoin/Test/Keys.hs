{-|
Module      : Network.Haskoin.Test.Keys
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Keys where

import           Data.Bits                     (clearBit)
import           Data.List                     (foldl')
import           Data.Word                     (Word32)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Keys.Extended
import           Network.Haskoin.Test.Crypto
import           Test.QuickCheck

-- | Arbitrary private key with arbitrary compressed flag.
arbitrarySecKeyI :: Gen SecKeyI
arbitrarySecKeyI = wrapSecKey <$> arbitrary <*> arbitrary

-- | Arbitrary keypair, both either compressed or not.
arbitraryKeyPair :: Gen (SecKeyI, PubKeyI)
arbitraryKeyPair = do
    k <- arbitrarySecKeyI
    return (k, derivePubKeyI k)

-- | Arbitrary extended private key.
arbitraryXPrvKey :: Gen XPrvKey
arbitraryXPrvKey =
    XPrvKey <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryHash256
            <*> arbitrary

-- | Arbitrary extended public key with its corresponding private key.
arbitraryXPubKey :: Gen (XPrvKey, XPubKey)
arbitraryXPubKey = (\k -> (k, deriveXPubKey k)) <$> arbitraryXPrvKey

{- Custom derivations -}

-- | Arbitrary derivation index with last bit unset.
genIndex :: Gen Word32
genIndex = (`clearBit` 31) <$> arbitrary

-- | Arbitrary BIP-32 path index. Can be hardened or not.
arbitraryBip32PathIndex :: Gen Bip32PathIndex
arbitraryBip32PathIndex =
    oneof [ Bip32SoftIndex <$> genIndex
          , Bip32HardIndex <$> genIndex
          ]

-- | Arbitrary BIP-32 derivation path composed of only hardened derivations.
arbitraryHardPath :: Gen HardPath
arbitraryHardPath = foldl' (:|) Deriv <$> listOf genIndex

-- | Arbitrary BIP-32 derivation path composed of only non-hardened derivations.
arbitrarySoftPath :: Gen SoftPath
arbitrarySoftPath = foldl' (:/) Deriv <$> listOf genIndex

-- | Arbitrary derivation path composed of hardened and non-hardened derivations.
arbitraryDerivPath :: Gen DerivPath
arbitraryDerivPath = concatBip32Segments <$> listOf arbitraryBip32PathIndex

-- | Arbitrary parsed derivation path. Can contain 'ParsedPrv', 'ParsedPub' or
-- 'ParsedEmpty' elements.
arbitraryParsedPath :: Gen ParsedPath
arbitraryParsedPath =
    oneof [ ParsedPrv <$> arbitraryDerivPath
          , ParsedPub <$> arbitraryDerivPath
          , ParsedEmpty <$> arbitraryDerivPath
          ]

-- | Arbitrary message hash, private key, nonce and corresponding signature. The
-- signature is generated with a random message, random private key and a random
-- nonce.
arbitrarySignature :: Gen (Hash256, SecKey, Sig)
arbitrarySignature = do
    m <- arbitraryHash256
    key <- arbitrary
    let sig = signHash key m
    return (m, key, sig)
