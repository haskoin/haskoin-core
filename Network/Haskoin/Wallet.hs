{-|
  This package provides a command line application called /hw/ (haskoin
  wallet). It is a lightweight bitcoin wallet featuring BIP32 key management,
  deterministic signatures (RFC-6979) and first order support for
  multisignature transactions. A library API for /hw/ is also exposed.
-}
module Network.Haskoin.Wallet
( 
-- *Wallet Commands
  Wallet(..)
, WalletName
, initWalletDB
, newWallet
, getWallet
, walletList
, WalletException(..)

-- *Account Commands
, Account(..)
, AccountName
, getAccount
, newAccount
, newAccountMultisig
, newAccountRead
, newAccountReadMultisig
, addAccountKeys
, accountList
, accountPrvKey
, isMSAccount

-- *Address Commands
, Balance(..)
, BalanceAddress(..)
, LabeledAddress(..)
, RecipientAddress(..)
, getAddress
, addressList
, addressPage
, unusedAddrs
, unlabeledAddrs
, unusedAddrsGeneric
, setAddrLabel
, addLookAhead
, addressPrvKey
, toPaymentAddr

-- *Balance Commands
, addressBalance
, accountBalance
, spendableAccountBalance
, unspentCoins
, spendableCoins

-- *Tx Commands
, AccTx(..) 
, TxConfidence(..)
, TxSource(..)
, OfflineTxData(..)
, getTx
, getAccTx
, txList
, txPage
, importTx
, removeTx
, createTx
, signWalletTx
, getOfflineTxData
, signOfflineTxData
, walletBloomFilter
, getProposition
, isTxInWallet
, firstKeyTime
, resetRescan

-- *Block Commands
, importBlocks

-- *Request Types
, WalletRequest(..)
, PagedResult(..)
, NewWallet(..)
, NewAccount(..)
, AccTxAction(..)
, AddressData(..)
, NodeAction(..)

-- *Response Types
, WalletResponse(..)
, MnemonicRes(..)
, AddressPageRes(..)
, TxPageRes(..)
, TxHashStatusRes(..)
, TxStatusRes(..)
, TxRes(..)
, BalanceRes(..)
, SpendableRes(..)
, RescanRes(..)

-- *Client
, clientMain
, OutputFormat(..)
, Config(..)

-- *Server
, runSPVServer
, stopSPVServer
, SPVMode(..)

) where

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Wallet.Client
import Network.Haskoin.Wallet.Server
import Network.Haskoin.Wallet.Settings

