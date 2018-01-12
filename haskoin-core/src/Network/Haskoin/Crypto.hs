{-|
  This package provides the elliptic curve cryptography required for creating
  and validating bitcoin transactions. It also provides SHA-256 and RIPEMD-160
  hashing functions; as well as mnemonic keys from BIP-0039.
-}
module Network.Haskoin.Crypto
(
  -- *Elliptic Curve Keys

  -- **Public Keys
  PubKey, PubKeyC, PubKeyU
, makePubKey
, makePubKeyG
, makePubKeyC
, makePubKeyU
, toPubKeyG
, eitherPubKey
, maybePubKeyC
, maybePubKeyU
, derivePubKey
, pubKeyAddr
, tweakPubKeyC

  -- **Private Keys
, PrvKey, PrvKeyC, PrvKeyU
, makePrvKey
, makePrvKeyG
, makePrvKeyC
, makePrvKeyU
, toPrvKeyG
, eitherPrvKey
, maybePrvKeyC
, maybePrvKeyU
, encodePrvKey
, decodePrvKey
, prvKeyPutMonad
, prvKeyGetMonad
, fromWif
, toWif
, tweakPrvKeyC

  -- *ECDSA
  -- **SecretT Monad
  -- | The 'SecretT' monad is a monadic wrapper around  HMAC DRBG
  -- (deterministic random byte generator) using SHA-256. The specification is
  -- defined in
  -- <http://csrc.nist.gov/publications/nistpubs/800-90A/SP800-90A.pdf>. The
  -- 'SecretT' monad is used to generate random private keys.
, SecretT
, withSource
, getEntropy
, genPrvKey

  -- **Signatures
  -- | Elliptic curve cryptography standards are defined in
  -- <http://www.secg.org/sec1-v2.pdf>
, Signature
, signMsg
, verifySig
, isCanonicalHalfOrder
, decodeDerSig
, decodeStrictSig

  -- *Hash functions
, CheckSum32(getCheckSum32)
, Hash512(getHash512)
, Hash256(getHash256)
, Hash160(getHash160)
, hash512ToBS
, hash256ToBS
, hash160ToBS
, hashSHA1ToBS
, checkSum32
, hash512
, hash256
, hash160
, hashSHA1
, doubleHash256
, hmac512
, hmac256
, split512
, join512

  -- *Base58 and Addresses
, Address(..)
, base58ToAddr
, addrToBase58
, encodeBase58
, decodeBase58
, encodeBase58Check
, decodeBase58Check

  -- *Mnemonic keys (BIP-0039)
, Entropy
, Mnemonic
, Passphrase
, Seed
, toMnemonic
, mnemonicToSeed

  -- *Extended Keys
, KeyIndex
, ChainCode

  -- **Extended Private Keys
, XPrvKey(..)
, makeXPrvKey
, xPrvIsHard
, xPrvChild
, xPrvID
, xPrvFP
, xPrvExport
, xPrvImport
, xPrvWif

  -- **Extended Public Keys
, XPubKey(..)
, deriveXPubKey
, xPubIsHard
, xPubChild
, xPubID
, xPubFP
, xPubAddr
, xPubExport
, xPubImport

  -- **Child key derivations
, prvSubKey
, pubSubKey
, hardSubKey
, prvSubKeys
, pubSubKeys
, hardSubKeys

  -- ** Address derivations
, deriveAddr
, deriveAddrs
, deriveMSAddr
, deriveMSAddrs

  -- ** Derivation paths
, DerivPathI((:|), (:/), Deriv)
, DerivPath
, HardPath
, SoftPath
, derivePath
, derivePubPath
, toHard
, toSoft
, toGeneric
, (++/)
, pathToStr
, pathToList
, listToPath

  -- ** Derivation path parsing
, XKey(..)
, ParsedPath(..)
, parsePath
, parseHard
, parseSoft
, applyPath

  -- * Custom path address derivations
, derivePathAddr
, derivePathAddrs
, derivePathMSAddr
, derivePathMSAddrs

) where

import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Mnemonic
import Network.Haskoin.Crypto.ExtendedKeys

