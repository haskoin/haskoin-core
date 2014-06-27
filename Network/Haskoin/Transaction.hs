{-|
  This package provides functions for building and signing both simple
  transactions and multisignature transactions.
-}
module Network.Haskoin.Transaction
(
  -- *Build Transactions
  buildTx
, buildAddrTx

  -- *Transaction signing
, SigInput(..)
, SigStatus(..)
, isSigInvalid
, signTx
, signInput
, detSignTx
, detSignInput

  -- *Coin selection
, Coin(..)
, chooseCoins
, chooseMSCoins
, guessTxSize

) where

import Network.Haskoin.Transaction.Builder

