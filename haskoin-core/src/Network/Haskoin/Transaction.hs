{-|
  This package provides functions for building and signing both simple
  transactions and multisignature transactions.
-}
module Network.Haskoin.Transaction
(
  -- *Transaction Types
  Tx(..)
, createTx
, txHash
, TxIn(..)
, TxOut(..)
, OutPoint(..)
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
, guessTxFee
, guessMSTxFee

, buildInput

  -- *Genesis
, genesisTx
) where

import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Transaction.Genesis

