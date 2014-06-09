{-|
  This package provides functions for parsing and evaluating bitcoin
  transaction scripts. Data types are provided for building and
  deconstructing all of the standard input and output script types. 
-}
module Network.Haskoin.Script
(
  -- *Scripts
  -- | More informations on scripts is available here:
  -- <http://en.bitcoin.it/wiki/Script>
  Script(..)
, ScriptOp(..)
, PushDataType(..)
, opPushData

  -- *Script Parsing
  -- **Script Outputs
, ScriptOutput(..)
, encodeOutput
, encodeOutputBS
, decodeOutput
, decodeOutputBS
, isPayPK
, isPayPKHash
, isPayMulSig
, isPayScriptHash
, scriptAddr
, sortMulSig

  -- **Script Inputs
, ScriptInput(..)
, encodeInput
, encodeInputBS
, decodeInput
, decodeInputBS
, isSpendPK
, isSpendPKHash
, isSpendMulSig

  -- **ScriptHash Inputs
, ScriptHashInput(..)
, RedeemScript
, encodeScriptHash
, encodeScriptHashBS
, decodeScriptHash
, decodeScriptHashBS

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

import Network.Haskoin.Script.Types
import Network.Haskoin.Script.Parser
import Network.Haskoin.Script.SigHash

