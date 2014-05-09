{-|
  This package provides the elliptic curve cryptography required for creating
  and validating bitcoin transactions. It also provides SHA-256 and RIPEMD-160
  hashing functions; as well as mnemonic keys from BIP-0039.
-}
module Network.Haskoin.Crypto
( 
  -- *Elliptic Curve Keys
  
  -- **Public Keys
  PubKey(..)
, isValidPubKey
, isPubKeyU
, derivePubKey
, pubKeyAddr
, addPubKeys

  -- **Private Keys
, PrvKey(..)
, isValidPrvKey
, makePrvKey
, makePrvKeyU
, fromPrvKey
, isPrvKeyU
, addPrvKeys
, putPrvKey
, getPrvKey
, getPrvKeyU
, fromWIF
, toWIF

  -- *ECDSA
  -- **SecretT Monad
  -- | The SecretT monad is a monadic wrapper around  HMAC DRBG (deterministic
  -- random byte generator) using SHA-256. The implementation is provided in 
  -- 'Network.Haskoin.Crypto.Hash' and the specification is defined in
  -- <http://csrc.nist.gov/publications/nistpubs/800-90A/SP800-90A.pdf>. The
  -- SecretT monad is used to generate random private keys and random nonces
  -- for ECDSA signatures.
, SecretT
, withSource
, devURandom
, devRandom
, genPrvKey

  -- **Signatures
  -- | Elliptic curve cryptography standards are defined in
  -- <http://www.secg.org/download/aid-780/sec1-v2.pdf>
, Signature
, signMsg
, detSignMsg
, verifySig
, isCanonicalHalfOrder

  -- * Big words
, Word512
, Word256
, Word160
, Word128

  -- *Hash functions
, Hash512
, Hash256
, Hash160
, CheckSum32
, hash512
, hash512BS
, hash256
, hash256BS
, hash160
, hash160BS
, doubleHash256
, doubleHash256BS
, chksum32
, hmac512
, hmac512BS
, hmac256
, hmac256BS
, split512
, join512
, murmurHash3

  -- *Number representations
, decodeCompact
, encodeCompact

  -- *Base58 and Addresses
, Address(..)
, base58ToAddr
, addrToBase58
, encodeBase58
, decodeBase58
, encodeBase58Check
, decodeBase58Check

  -- *Mnemonic keys (BIP-0039)
, toMnemonic
, fromMnemonic
, mnemonicToSeed
, anyToSeed
, english

  -- *Extended Keys
, ChainCode

  -- **Extended Private Keys
, XPrvKey(..)
, makeXPrvKey
, xPrvIsPrime
, xPrvChild
, xPrvID
, xPrvFP
, xPrvExport
, xPrvImport
, xPrvWIF

  -- **Extended Public Keys
, XPubKey(..)
, deriveXPubKey
, xPubIsPrime
, xPubChild
, xPubID
, xPubFP
, xPubAddr
, xPubExport
, xPubImport

  -- **Child key derivations
, prvSubKey
, pubSubKey
, primeSubKey
, prvSubKeys
, pubSubKeys
, primeSubKeys

  -- ***Multisig derivations
, mulSigSubKey
, mulSigSubKeys

  -- *Derivation tree interoperability

  -- | To improve BIP32 wallet interoperability, a standard derivation tree
  -- is used. All accounts are generated through prime derivations from the
  -- master key. This ensures that the master key is not compromised if
  -- an account is compromised. Every account will generate receiving
  -- addresses from the non-prime subtree index 0 and internal change
  -- addresses from the non-prime subtree index 1. MasterKey, AccountKey
  -- and AddressKey types are defined to conform to the wallet interoperability
  -- format.

, KeyIndex

  -- **Master keys
, MasterKey(..)
, makeMasterKey
, loadMasterKey

  -- **Account keys
, AccPrvKey(..)
, AccPubKey(..)
, loadPrvAcc
, loadPubAcc
, accPrvKey
, accPubKey
, accPrvKeys
, accPubKeys

  -- **Address keys
, AddrPrvKey(..)
, AddrPubKey(..)
, addr
, extPrvKey
, extPubKey
, intPrvKey
, intPubKey
, extPrvKeys
, extPubKeys
, intPrvKeys
, intPubKeys
, extAddr
, intAddr
, extAddrs
, intAddrs
, extAddrs'
, intAddrs'

  -- ***Multisig address keys
, extMulSigKey
, intMulSigKey
, extMulSigKeys
, intMulSigKeys
, extMulSigAddr
, intMulSigAddr
, extMulSigAddrs
, intMulSigAddrs

  -- *Bloom filters
, BloomFilter
, BloomFlags(..)
, bloomCreate
, bloomInsert
, bloomContains
, bloomUpdateEmptyFull
, bloomIsValid

  -- *Partial merkle trees
, calcTreeHeight
, calcTreeWidth
, buildMerkleRoot
, calcHash
, buildPartialMerkle
, extractMatches

) where

import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Mnemonic
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Crypto.NormalizedKeys
import Network.Haskoin.Crypto.Merkle
import Network.Haskoin.Crypto.Bloom

