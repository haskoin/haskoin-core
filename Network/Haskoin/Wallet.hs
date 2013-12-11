{-|
  This package provides functions for generating hierarchical deterministic
  keys (BIP32). It also provides functions for building and signing both
  simple transactions and multisignature transactions. This package also
  provides a command lines application called /hw/ (haskoin wallet). It is a
  lightweight bitcoin wallet featuring BIP32 key management, deterministic
  signatures (RFC-6979) and first order support for multisignature
  transactions. A library API for /hw/ is also exposed.
-}
module Network.Haskoin.Wallet
( 
  -- *Extended Keys
  ChainCode

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

-- Tx module
, Coin(..)
, buildTx
, buildAddrTx
, SigInput(..)
, signTx
, detSignTx
, isTxComplete

) where

import Network.Haskoin.Wallet.Keys
import Network.Haskoin.Wallet.Manager
import Network.Haskoin.Wallet.TxBuilder


