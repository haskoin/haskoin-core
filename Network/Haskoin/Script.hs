{-|
  This package provides functions for parsing and evaluating bitcoin
  transaction scripts. Data types are provided for building and
  deconstructing all of the standard input and output script types. 
-}
module Network.Haskoin.Script
(
  -- *Script Parsing
  -- **Script Outputs
  ScriptOutput(..)
, encodeOutput
, decodeOutput
, isPayPK
, isPayPKHash
, isPayMulSig
, isPayScriptHash
, scriptAddr
, sortMulSig
  -- **Script Inputs
, ScriptInput(..)
, encodeInput
, decodeInput
, isSpendPK
, isSpendPKHash
, isSpendMulSig
  -- **ScriptHash Inputs
, ScriptHashInput(..)
, RedeemScript
, encodeScriptHash
, decodeScriptHash
  -- * Helpers
, scriptRecipient
, scriptSender
, intToScriptOp
, scriptOpToInt

-- *SigHash
-- | For additional information on sighashes, see:
-- <http://en.bitcoin.it/wiki/OP_CHECKSIG>
, SigHash(..)
, txSigHash
, encodeSigHash32
, isSigAll
, isSigNone
, isSigSingle
, isSigUnknown
, TxSignature(..)
, encodeSig
, decodeSig
, decodeCanonicalSig

) where

import Network.Haskoin.Script.Parser
import Network.Haskoin.Script.SigHash

