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
, txHash
, nosigTxHash
, cbHash

  -- *Build Transactions
, buildTx
, buildAddrTx

  -- *Sign Transactions
, SigInput(..)
, signTx
, signInput
, detSignTx
, detSignInput
, mergeTxs
, verifyStdTx
, verifyStdInput

  -- *Coin selection
, Coin(..)
, chooseCoins
, chooseMSCoins
, guessTxSize

) where

import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Transaction.Types

