{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Standard
( ScriptInput(..)
, SimpleInput(..)
, RedeemScript
, p2shAddr
, p2wshAddr
, outputAddress
, inputAddress
, encodeInput
, encodeInputBS
, decodeInput
, decodeInputBS
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
import           Control.Monad                  (guard, (<=<))
import           Data.ByteString                (ByteString)
import           Data.Function                  (on)
import           Data.List                      (sortBy)
import           Data.Serialize                 (decode, encode)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Script.Common
import           Network.Haskoin.Script.SigHash
import           Network.Haskoin.Util


-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend, except in pay-to-witness-public-key-hash and
-- pay-to-script-hash transactions.
data SimpleInput
      -- | spend pay-to-public-key output
    = SpendPK     { getInputSig :: !TxSignature }
      -- | spend pay-to-public-key-hash output
    | SpendPKHash { getInputSig :: !TxSignature
                  , getInputKey :: !PubKeyI
                  }
      -- | spend multisig output
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature] }
    deriving (Eq, Show)

instance NFData SimpleInput where
    rnf (SpendPK i)       = rnf i
    rnf (SpendPKHash i k) = rnf i `seq` rnf k
    rnf (SpendMulSig k)   = rnf k

-- | Returns true if the input script is spending from a pay-to-public-key
-- output.
isSpendPK :: ScriptInput -> Bool
isSpendPK (RegularInput (SpendPK _)) = True
isSpendPK _                          = False

-- | Returns true if the input script is spending from a pay-to-public-key-hash
-- output.
isSpendPKHash :: ScriptInput -> Bool
isSpendPKHash (RegularInput (SpendPKHash _ _)) = True
isSpendPKHash _                                = False

-- | Returns true if the input script is spending a multisig output.
isSpendMulSig :: ScriptInput -> Bool
isSpendMulSig (RegularInput (SpendMulSig _)) = True
isSpendMulSig _                              = False

-- | Returns true if the input script is spending a pay-to-script-hash output.
isScriptHashInput :: ScriptInput -> Bool
isScriptHashInput (ScriptHashInput _ _) = True
isScriptHashInput _                     = False

-- | A redeem script is the output script serialized into the spending input
-- script. It must be included in inputs that spend pay-to-script-hash outputs.
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


-- | Compute a pay-to-script-hash address for an output script.
p2shAddr :: Network -> ScriptOutput -> Address
p2shAddr net out = ScriptAddress (addressHash (encodeOutputBS out)) net

-- | Compute a pay-to-witness-script-hash address for an output script. Only on
-- SegWit networks.
p2wshAddr :: Network -> ScriptOutput -> Maybe Address
p2wshAddr net out = do
    guard (getSegWit net)
    return $ WitnessScriptAddress (sha256 (encodeOutputBS out)) net

-- | Encode an output script from an address. Will fail if using a
-- pay-to-witness address on a non-SegWit network.
addressToOutput :: Address -> Maybe ScriptOutput
addressToOutput (PubKeyAddress h _)        = Just (PayPKHash h)
addressToOutput (ScriptAddress h _)        = Just (PayScriptHash h)
addressToOutput (WitnessPubKeyAddress h n)
    | getSegWit n = Just (PayWitnessPKHash h)
    | otherwise = Nothing
addressToOutput (WitnessScriptAddress h n)
    | getSegWit n = Just (PayWitnessScriptHash h)
    | otherwise = Nothing

-- | Get output script AST for an 'Address'.
addressToScript :: Address -> Maybe Script
addressToScript a = encodeOutput <$> addressToOutput a

-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Address -> Maybe ByteString
addressToScriptBS a = encode <$> addressToScript a

-- | Decode an output script into an 'Address' if it has such representation.
scriptToAddress :: Network -> Script -> Maybe Address
scriptToAddress net = eitherToMaybe . (outputAddress net <=< decodeOutput)

-- | Decode a serialized script into an 'Address'.
scriptToAddressBS :: Network -> ByteString -> Maybe Address
scriptToAddressBS net = eitherToMaybe . (outputAddress net <=< decodeOutputBS)

-- | Get the 'Address' of a 'ScriptOutput'.
outputAddress :: Network -> ScriptOutput -> Either String Address
outputAddress net s =
    case s of
        PayPKHash h -> return $ PubKeyAddress h net
        PayScriptHash h -> return $ ScriptAddress h net
        PayPK k -> return $ pubKeyAddr net k
        PayWitnessPKHash h
            | getSegWit net -> return $ WitnessPubKeyAddress h net
        PayWitnessScriptHash h
            | getSegWit net -> return $ WitnessScriptAddress h net
        _ -> Left "outputAddress: bad output script type"

-- | Infer the address of a 'ScriptInput'
inputAddress :: Network -> ScriptInput -> Either String Address
inputAddress net s = case s of
    RegularInput (SpendPKHash _ key) -> return $ pubKeyAddr net key
    ScriptHashInput _ rdm -> return $ p2shAddr net rdm
    _ -> Left "inputAddress: bad input script type"

-- | Heuristic to decode an input script into one of the standard types.
decodeSimpleInput :: Network -> Script -> Either String SimpleInput
decodeSimpleInput net (Script ops) =
    maybeToEither errMsg $ matchPK ops <|> matchPKHash ops <|> matchMulSig ops
  where
    matchPK [op] = SpendPK <$> f op
    matchPK _    = Nothing
    matchPKHash [op, OP_PUSHDATA pub _] =
        SpendPKHash <$> f op <*> eitherToMaybe (decode pub)
    matchPKHash _ = Nothing
    matchMulSig (x:xs) = do
        guard $ x == OP_0
        SpendMulSig <$> mapM f xs
    matchMulSig _ = Nothing
    f OP_0                    = return TxSignatureEmpty
    f (OP_PUSHDATA "" OPCODE) = f OP_0
    f (OP_PUSHDATA bs _)      = eitherToMaybe $ decodeTxSig net bs
    f _                       = Nothing
    errMsg = "decodeInput: Could not decode script input"

-- | Heuristic to decode a 'ScriptInput' from a 'Script'. This function fails if
-- the script can not be parsed as a standard script input.
decodeInput :: Network -> Script -> Either String ScriptInput
decodeInput net s@(Script ops) =
    maybeToEither errMsg $ matchSimpleInput <|> matchPayScriptHash
  where
    matchSimpleInput =
        RegularInput <$> eitherToMaybe (decodeSimpleInput net s)
    matchPayScriptHash =
        case splitAt (length (scriptOps s) - 1) ops of
            (is, [OP_PUSHDATA bs _]) -> do
                rdm <- eitherToMaybe $ decodeOutputBS bs
                inp <- eitherToMaybe $ decodeSimpleInput net $ Script is
                return $ ScriptHashInput inp rdm
            _ -> Nothing
    errMsg = "decodeInput: Could not decode script input"

-- | Like 'decodeInput' but decodes directly from a serialized script
-- 'ByteString'.
decodeInputBS :: Network -> ByteString -> Either String ScriptInput
decodeInputBS net = decodeInput net <=< decode

-- | Encode a standard input into a script.
encodeInput :: ScriptInput -> Script
encodeInput s = case s of
    RegularInput ri -> encodeSimpleInput ri
    ScriptHashInput i o -> Script $
        scriptOps (encodeSimpleInput i) ++ [opPushData $ encodeOutputBS o]

-- | Similar to 'encodeInput' but encodes directly to a serialized script
-- 'ByteString'.
encodeInputBS :: ScriptInput -> ByteString
encodeInputBS = encode . encodeInput

-- | Encode a standard 'SimpleInput' into opcodes as an input 'Script'.
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

-- | Sort the public keys of a multisig output in ascending order by comparing
-- their compressed serialized representations. Refer to BIP-67.
sortMulSig :: ScriptOutput -> ScriptOutput
sortMulSig out = case out of
    PayMulSig keys r -> PayMulSig (sortBy (compare `on` encode) keys) r
    _ -> error "Can only call orderMulSig on PayMulSig scripts"
