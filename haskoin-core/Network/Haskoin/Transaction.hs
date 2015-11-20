{-|
  This package provides functions for building and signing both simple
  transactions and multisignature transactions.
-}
module Network.Haskoin.Transaction
(
  -- *Transaction Types
  Tx(..)
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, CoinbaseTx(..)
, TxHash(..)
, txHash
, hexToTxHash
, txHashToHex
, nosigTxHash
, cbHash

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

) where

import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Transaction.Types

