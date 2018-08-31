{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Standard
( ScriptOutput(..)
, ScriptInput(..)
, SimpleInput(..)
, RedeemScript
, p2shAddr
, outputAddress
, inputAddress
, encodeInput
, encodeInputBS
, decodeInput
, decodeInputBS
, decodeInputStrict
, decodeInputStrictBS
, addressToOutput
, addressToScript
, addressToScriptBS
, scriptToAddress
, scriptToAddressBS
, sortMulSig
, scriptOpToInt
, isSpendPK
, isSpendPKHash
, isSpendMulSig
, isScriptHashInput
) where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData, rnf)
import           Control.Monad                  (guard, liftM2, (<=<))
import           Data.Aeson                     (FromJSON, ToJSON,
                                                 Value (String), parseJSON,
                                                 toJSON, withText)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Function                  (on)
import           Data.List                      (sortBy)
import           Data.Serialize                 (decode, encode)
import           Data.String.Conversions        (cs)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Keys.Types
import           Network.Haskoin.Script.SigHash
import           Network.Haskoin.Script.Types
import           Network.Haskoin.Util

-- | Computes a script address from a script output. This address can be used
-- in a pay to script hash output.
p2shAddr :: Network -> ScriptOutput -> Address
p2shAddr net out = ScriptAddress (addressHash (encodeOutputBS out)) net

-- | Computes a script address from a script output for a
-- pay-to-witness-script-hash output.
p2wshAddr :: Network -> ScriptOutput -> Maybe Address
p2wshAddr net out = do
    guard (getSegWit net)
    return $ WitnessScriptAddress (sha256 (encodeOutputBS out)) net

-- | Sorts the public keys of a multisignature output in ascending order by
-- comparing their serialized representations. This feature allows for easier
-- multisignature account management as participants in a multisignature wallet
-- will blindly agree on an ordering of the public keys without having to
-- communicate.
sortMulSig :: ScriptOutput -> ScriptOutput
sortMulSig out = case out of
    PayMulSig keys r -> PayMulSig (sortBy (compare `on` encode) keys) r
    _ -> error "Can only call orderMulSig on PayMulSig scripts"

addressToOutput :: Address -> ScriptOutput
addressToOutput (PubKeyAddress h _)        = PayPKHash h
addressToOutput (ScriptAddress h _)        = PayScriptHash h
addressToOutput (WitnessPubKeyAddress h _) = PayWitnessPKHash h
addressToOutput (WitnessScriptAddress h _) = PayWitnessScriptHash h

-- | Get output script AST for an address.
addressToScript :: Address -> Script
addressToScript = encodeOutput . addressToOutput

-- | Encode address as output script in ByteString form.
addressToScriptBS :: Address -> ByteString
addressToScriptBS = encode . addressToScript

-- | Encode an output script as an address if it has such representation.
scriptToAddress :: Network -> Script -> Maybe Address
scriptToAddress net = eitherToMaybe . (outputAddress net <=< decodeOutput)

scriptToAddressBS :: Network -> ByteString -> Maybe Address
scriptToAddressBS net = eitherToMaybe . (outputAddress net <=< decodeOutputBS)

-- | Get the address of a `ScriptOutput`
outputAddress :: Network -> ScriptOutput -> Either String Address
outputAddress net s = case s of
    PayPKHash h            -> return $ PubKeyAddress h net
    PayScriptHash h        -> return $ ScriptAddress h net
    PayPK k                -> return $ pubKeyAddr net k
    PayWitnessPKHash h     -> return $ WitnessPubKeyAddress h net
    PayWitnessScriptHash h -> return $ WitnessScriptAddress h net
    _                      -> Left "outputAddress: bad output script type"

-- | Get the address of a `ScriptInput`
inputAddress :: Network -> ScriptInput -> Either String Address
inputAddress net s = case s of
    RegularInput (SpendPKHash _ key) -> return $ pubKeyAddr net key
    ScriptHashInput _ rdm -> return $ p2shAddr net rdm
    _ -> Left "inputAddress: bad input script type"

-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend.
data SimpleInput
      -- | Spend the coins of a PayPK output.
    = SpendPK     { getInputSig :: !TxSignature }
      -- | Spend the coins of a PayPKHash output.
    | SpendPKHash { getInputSig :: !TxSignature
                  , getInputKey :: !PubKeyI
                  }
      -- | Spend the coins of a PayMulSig output.
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature] }
    deriving (Eq, Show)

instance NFData SimpleInput where
    rnf (SpendPK i)       = rnf i
    rnf (SpendPKHash i k) = rnf i `seq` rnf k
    rnf (SpendMulSig k)   = rnf k

-- | Returns True if the input script is spending a public key.
isSpendPK :: ScriptInput -> Bool
isSpendPK (RegularInput (SpendPK _)) = True
isSpendPK _                          = False

-- | Returns True if the input script is spending a public key hash.
isSpendPKHash :: ScriptInput -> Bool
isSpendPKHash (RegularInput (SpendPKHash _ _)) = True
isSpendPKHash _                                = False

-- | Returns True if the input script is spending a multisignature output.
isSpendMulSig :: ScriptInput -> Bool
isSpendMulSig (RegularInput (SpendMulSig _)) = True
isSpendMulSig _                              = False

isScriptHashInput :: ScriptInput -> Bool
isScriptHashInput (ScriptHashInput _ _) = True
isScriptHashInput _                     = False

type RedeemScript = ScriptOutput

data ScriptInput
    = RegularInput    { getRegularInput     :: SimpleInput }
    | ScriptHashInput { getScriptHashInput  :: SimpleInput
                      , getScriptHashRedeem :: RedeemScript
                      }
    deriving (Eq, Show)

instance NFData ScriptInput where
    rnf (RegularInput i)      = rnf i
    rnf (ScriptHashInput i o) = rnf i `seq` rnf o

-- | Computes a 'Script' from a 'SimpleInput'. The 'Script' is a list of
-- 'ScriptOp' that can be used to build a 'Tx'.
encodeSimpleInput :: SimpleInput -> Script
encodeSimpleInput s =
    Script $
    case s of
        SpendPK ts       -> [f ts]
        SpendPKHash ts p -> [f ts, opPushData $ encode p]
        SpendMulSig xs   -> OP_0 : map f xs
  where
    f TxSignatureEmpty = OP_0
    f ts               = opPushData $ encodeTxSig ts

decodeSimpleInput :: Network -> Bool -> Script -> Either String SimpleInput
decodeSimpleInput net strict (Script ops) =
    maybeToEither errMsg $ matchPK ops <|> matchPKHash ops <|> matchMulSig ops
  where
    matchPK [op] = SpendPK <$> f op
    matchPK _    = Nothing
    matchPKHash [op, OP_PUSHDATA pub _] =
        SpendPKHash <$> f op <*> eitherToMaybe (decode pub)
    matchPKHash _ = Nothing
    matchMulSig (x:xs) = do
        guard $
            if strict
                then x == OP_0
                else isPushOp x
        SpendMulSig <$> mapM f xs
    matchMulSig _ = Nothing
    f OP_0 = return TxSignatureEmpty
    f (OP_PUSHDATA "" OPCODE) = f OP_0
    f (OP_PUSHDATA bs _)
        | strict = eitherToMaybe $ decodeTxStrictSig net bs
        | otherwise = eitherToMaybe $ decodeTxLaxSig bs
    f _ = Nothing
    errMsg = "decodeInput: Could not decode script input"

encodeInput :: ScriptInput -> Script
encodeInput s = case s of
    RegularInput ri -> encodeSimpleInput ri
    ScriptHashInput i o -> Script $
        scriptOps (encodeSimpleInput i) ++ [opPushData $ encodeOutputBS o]

-- | Similar to 'encodeInput' but encodes to a ByteString
encodeInputBS :: ScriptInput -> ByteString
encodeInputBS = encode . encodeInput

-- | Decodes a 'ScriptInput' from a 'Script'. This function fails if the
-- script can not be parsed as a standard script input.
decodeInput :: Network -> Script -> Either String ScriptInput
decodeInput net = decodeInputGen net False

-- | Like 'decodeInput' but uses strict signature decoding
decodeInputStrict :: Network -> Script -> Either String ScriptInput
decodeInputStrict net = decodeInputGen net True

decodeInputGen :: Network -> Bool -> Script -> Either String ScriptInput
decodeInputGen net strict s@(Script ops) =
    maybeToEither errMsg $ matchSimpleInput <|> matchPayScriptHash
  where
    matchSimpleInput =
        RegularInput <$> eitherToMaybe (decodeSimpleInput net strict s)
    matchPayScriptHash =
        case splitAt (length (scriptOps s) - 1) ops of
            (is, [OP_PUSHDATA bs _]) -> do
                rdm <- eitherToMaybe $ decodeOutputBS bs
                inp <- eitherToMaybe $ decodeSimpleInput net strict $ Script is
                return $ ScriptHashInput inp rdm
            _ -> Nothing
    errMsg = "decodeInput: Could not decode script input"

-- | Like 'decodeInput' but decodes from a ByteString
decodeInputBS :: Network -> ByteString -> Either String ScriptInput
decodeInputBS net = decodeInput net <=< decode

-- | Like 'decodeInputStrict' but decodes from a ByteString
decodeInputStrictBS :: Network -> ByteString -> Either String ScriptInput
decodeInputStrictBS net = decodeInputStrict net <=< decode
