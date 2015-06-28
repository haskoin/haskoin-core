{-|
  This package provides a command line application called /hw/ (haskoin
  wallet). It is a lightweight bitcoin wallet featuring BIP32 key management,
  deterministic signatures (RFC-6979) and first order support for
  multisignature transactions. A library API for /hw/ is also exposed.
-}
module Network.Haskoin.Wallet
( 
-- *Client
  clientMain
, OutputFormat(..)
, Config(..)

-- *Server
, runSPVServer
, stopSPVServer
, SPVMode(..)

-- *API JSON Types
, JsonKeyRing(..)
, JsonAccount(..)
, JsonAddr(..)
, JsonCoin(..)
, JsonTx(..)
, JsonAddrTx(..)

-- *API Request Types
, WalletRequest(..)
, PageRequest(..)
, validPageRequest
, NewKeyRing(..)
, NewAccount(..)
, SetAccountGap(..)
, OfflineTxData(..)
, CoinSignData(..)
, TxAction(..)
, AddressLabel(..)
, NodeAction(..)
, AccountType(..)
, AddressType(..)
, addrTypeIndex
, TxType(..)
, TxConfidence(..)
, AddressInfo(..)
, AddressBalance(..)

-- *API Response Types
, WalletResponse(..)
, MnemonicRes(..)
, TxHashConfidenceRes(..)
, TxConfidenceRes(..)
, TxCompleteRes(..)
, PageRes(..)
, RescanRes(..)
, TxRes(..)
, BalanceRes(..)

-- *Database KeyRings
, initWallet
, newKeyRing
, keyRingSource
, getKeyRing

-- *Database Accounts
, accountSource
, newAccount
, addAccountKeys
, getAccount
, isMultisigAccount
, isReadAccount
, isCompleteAccount

-- *Database Addresses
, getAddress
, addressSourceAll
, addressSource
, addressPage
, unusedAddresses
, firstUnusedAddress
, addressCount
, setAddrLabel
, addressPrvKey
, useAddress
, setAccountGap
, firstAddrTime
, getPathRedeem
, getPathPubKey

-- *Database Bloom Filter
, getBloomFilter

-- *Database transactions
, txPage
, addrTxPage
, getTx 
, importTx
, importNetTx
, signKeyRingTx
, createTx
, signOfflineTx
, getOfflineTxData

-- *Database blocks
, importMerkles
, getBestBlock

-- *Database coins and balances
, spendableCoins
, spendableCoinsSource
, accountBalance

-- *Rescan
, resetRescan
) where

import Network.Haskoin.Wallet.Client
import Network.Haskoin.Wallet.Server
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Transaction

