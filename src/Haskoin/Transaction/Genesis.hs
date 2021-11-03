{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Transaction.Genesis
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Code related to transactions parsing and serialization.
-}
module Haskoin.Transaction.Genesis (
    genesisTx,
) where

import Data.String (fromString)
import Haskoin.Script.Standard
import Haskoin.Transaction.Common
import Haskoin.Util

-- | Transaction from Genesis block.
genesisTx :: Tx
genesisTx =
    Tx 1 [txin] [txout] [] locktime
  where
    txin = TxIn outpoint inputBS maxBound
    txout = TxOut 5000000000 (encodeOutputBS output)
    locktime = 0
    outpoint = OutPoint z maxBound
    Just inputBS =
        decodeHex $
            fromString $
                "04ffff001d0104455468652054696d65732030332f4a616e2f323030392043686"
                    ++ "16e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f"
                    ++ "757420666f722062616e6b73"
    output =
        PayPK $
            fromString $
                "04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb"
                    ++ "649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"
    z = "0000000000000000000000000000000000000000000000000000000000000000"
