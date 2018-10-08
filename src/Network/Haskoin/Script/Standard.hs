{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Script.Standard
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Standard scripts like pay-to-public-key, pay-to-public-key-hash,
pay-to-script-hash, pay-to-multisig and corresponding SegWit variants.
-}
module Network.Haskoin.Script.Standard
( ScriptInput(..)
, SimpleInput(..)
, RedeemScript
, inputAddress
, outputAddress
, encodeInput
, encodeInputBS
, decodeInput
, decodeInputBS
, addressToScript
, addressToScriptBS
, addressToOutput
, payToScriptAddress
, payToWitnessScriptAddress
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
    = SpendPK { getInputSig :: !TxSignature
                  -- ^ transaction signature
               }
      -- | spend pay-to-public-key-hash output
    | SpendPKHash { getInputSig :: !TxSignature
                      -- ^ embedded signature
                  , getInputKey :: !PubKeyI
                      -- ^ public key
                   }
      -- | spend multisig output
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature]
                      -- ^ list of signatures
                   }
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

-- | Standard input script high-level representation.
data ScriptInput
    = RegularInput    {
        getRegularInput     :: SimpleInput
            -- ^ get wrapped simple input
        }
    | ScriptHashInput {
            getScriptHashInput  :: SimpleInput
                -- ^ get simple input associated with redeem script
            , getScriptHashRedeem :: RedeemScript
                -- ^ redeem script
            }
    deriving (Eq, Show)

instance NFData ScriptInput where
    rnf (RegularInput i)      = rnf i
    rnf (ScriptHashInput i o) = rnf i `seq` rnf o

-- | Compute a pay-to-script-hash address for an output script.
payToScriptAddress :: Network -> ScriptOutput -> Address
payToScriptAddress net out = p2shAddr net (addressHash (encodeOutputBS out))

-- | Compute a pay-to-witness-script-hash address for an output script. Only on
-- SegWit networks.
payToWitnessScriptAddress :: Network -> ScriptOutput -> Maybe Address
payToWitnessScriptAddress net out = p2wshAddr net (sha256 (encodeOutputBS out))

-- | Encode an output script from an address. Will fail if using a
-- pay-to-witness address on a non-SegWit network.
addressToOutput :: Address -> ScriptOutput
addressToOutput a
    | isPubKeyAddress a = PayPKHash (getAddrHash160 a)
    | isScriptAddress a = PayScriptHash (getAddrHash160 a)
    | isWitnessPubKeyAddress a = PayWitnessPKHash (getAddrHash160 a)
    | isWitnessScriptAddress a = PayWitnessScriptHash (getAddrHash256 a)
    | otherwise = undefined

-- | Get output script AST for an 'Address'.
addressToScript :: Address -> Script
addressToScript = encodeOutput . addressToOutput

-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Address -> ByteString
addressToScriptBS = encode . addressToScript

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
        PayPKHash h -> Right $ p2pkhAddr net h
        PayScriptHash h -> Right $ p2shAddr net h
        PayPK k -> Right $ pubKeyAddr net k
        PayWitnessPKHash h ->
            maybeToEither "outputAddress: segwit not supported in this network" $
            p2wpkhAddr net h
        PayWitnessScriptHash h ->
            maybeToEither "outputAddress: segwit not supported in this network" $
            p2wshAddr net h
        _ -> Left "outputAddress: bad output script type"

-- | Infer the address of a 'ScriptInput'
inputAddress :: Network -> ScriptInput -> Either String Address
inputAddress net s = case s of
    RegularInput (SpendPKHash _ key) -> return $ pubKeyAddr net key
    ScriptHashInput _ rdm -> return $ payToScriptAddress net rdm
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
