{-|
  This package provides functions for building and signing both simple
  transactions and multisignature transactions.
-}
module Network.Haskoin.Transaction
(
  -- *Transaction Types
  Tx
, createTx
, txVersion
, txIn
, txOut
, txLockTime
, txHash
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, Witness(..)
, TxHash(..)
, hexToTxHash
, txHashToHex
, nosigTxHash

  -- *Build Transactions
, buildTx
, buildAddrTx

  -- *Sign Transactions
, SigInput(..)
, signTx
, signInput
, mergeTxs
, verifyStdTx
, verifyStdInput

  -- *Coin selection
, Coin(..)
, chooseCoins
, chooseCoinsSink
, chooseMSCoins
, chooseMSCoinsSink
, guessTxSize
, getFee
, getMSFee

, buildInput
) where

import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Transaction.Types

