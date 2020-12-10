{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haskoin.Script.Standard
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Standard scripts like pay-to-public-key, pay-to-public-key-hash,
pay-to-script-hash, pay-to-multisig and corresponding SegWit variants.
-}
module Haskoin.Script.Standard
    ( -- * Standard Script Outputs
      ScriptOutput(..)
    , RedeemScript
    , isPayPK
    , isPayPKHash
    , isPayMulSig
    , isPayScriptHash
    , isPayWitnessPKHash
    , isPayWitnessScriptHash
    , isDataCarrier
    , encodeOutput
    , encodeOutputBS
    , decodeOutput
    , decodeOutputBS
    , toP2SH
    , toP2WSH
    , sortMulSig
    -- * Standard Script Inputs
    , ScriptInput(..)
    , SimpleInput(..)
    , encodeInput
    , encodeInputBS
    , decodeInput
    , decodeInputBS
    , isSpendPK
    , isSpendPKHash
    , isSpendMulSig
    , isScriptHashInput
    ) where

import           Control.Applicative    ((<|>))
import           Control.DeepSeq
import           Control.Monad          (guard, liftM2, (<=<))
import qualified Data.Aeson             as A
import qualified Data.Aeson.Encoding    as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Function          (on)
import           Data.Hashable
import           Data.List              (sortBy)
import           Data.Maybe             (fromJust, isJust)
import           Data.Serialize         as S
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Keys.Common
import           Haskoin.Script.Common
import           Haskoin.Script.SigHash
import           Haskoin.Util

-- | Data type describing standard transaction output scripts. Output scripts
-- provide the conditions that must be fulfilled for someone to spend the funds
-- in a transaction output.
data ScriptOutput
      -- | pay to public key
    = PayPK { getOutputPubKey :: !PubKeyI }
      -- | pay to public key hash
    | PayPKHash { getOutputHash :: !Hash160 }
      -- | multisig
    | PayMulSig { getOutputMulSigKeys     :: ![PubKeyI]
                , getOutputMulSigRequired :: !Int }
      -- | pay to a script hash
    | PayScriptHash { getOutputHash :: !Hash160 }
      -- | pay to witness public key hash
    | PayWitnessPKHash { getOutputHash :: !Hash160 }
      -- | pay to witness script hash
    | PayWitnessScriptHash { getScriptHash :: !Hash256 }
      -- | another pay to witness address
    | PayWitness { getWitnessVersion :: !Word8
                 , getWitnessData    :: !ByteString
                 }
      -- | provably unspendable data carrier
    | DataCarrier { getOutputData :: !ByteString }
    deriving (Eq, Show, Read, Generic, Hashable, NFData)

instance A.FromJSON ScriptOutput where
    parseJSON =
        A.withText "scriptoutput" $ \t ->
            either fail return $
            maybeToEither "scriptoutput not hex" (decodeHex t) >>=
            decodeOutputBS

instance A.ToJSON ScriptOutput where
    toJSON = A.String . encodeHex . encodeOutputBS
    toEncoding = A.text . encodeHex . encodeOutputBS

-- | Is script a pay-to-public-key output?
isPayPK :: ScriptOutput -> Bool
isPayPK (PayPK _) = True
isPayPK _         = False

-- | Is script a pay-to-pub-key-hash output?
isPayPKHash :: ScriptOutput -> Bool
isPayPKHash (PayPKHash _) = True
isPayPKHash _             = False

-- | Is script a pay-to-multi-sig output?
isPayMulSig :: ScriptOutput -> Bool
isPayMulSig (PayMulSig _ _) = True
isPayMulSig _               = False

-- | Is script a pay-to-script-hash output?
isPayScriptHash :: ScriptOutput -> Bool
isPayScriptHash (PayScriptHash _) = True
isPayScriptHash _                 = False

-- | Is script a pay-to-witness-pub-key-hash output?
isPayWitnessPKHash :: ScriptOutput -> Bool
isPayWitnessPKHash (PayWitnessPKHash _) = True
isPayWitnessPKHash _                    = False

-- | Is script a pay-to-witness-script-hash output?
isPayWitnessScriptHash :: ScriptOutput -> Bool
isPayWitnessScriptHash (PayWitnessScriptHash _) = True
isPayWitnessScriptHash _                        = False

-- | Is script paying to a different type of witness address?
isPayWitness :: ScriptOutput -> Bool
isPayWitness (PayWitness _ _) = True
isPayWitness _                = False

-- | Is script a data carrier output?
isDataCarrier :: ScriptOutput -> Bool
isDataCarrier (DataCarrier _) = True
isDataCarrier _               = False

-- | Tries to decode a 'ScriptOutput' from a 'Script'. This can fail if the
-- script is not recognized as any of the standard output types.
decodeOutput :: Script -> Either String ScriptOutput
decodeOutput s = case scriptOps s of
    -- Pay to PubKey
    [OP_PUSHDATA bs _, OP_CHECKSIG] -> PayPK <$> S.decode bs
    -- Pay to PubKey Hash
    [OP_DUP, OP_HASH160, OP_PUSHDATA bs _, OP_EQUALVERIFY, OP_CHECKSIG] ->
        PayPKHash <$> S.decode bs
    -- Pay to Script Hash
    [OP_HASH160, OP_PUSHDATA bs _, OP_EQUAL] ->
        PayScriptHash <$> S.decode  bs
    -- Pay to Witness
    [OP_0, OP_PUSHDATA bs OPCODE]
      | BS.length bs == 20 -> PayWitnessPKHash     <$> S.decode bs
      | BS.length bs == 32 -> PayWitnessScriptHash <$> S.decode bs
    -- Other Witness
    [ver, OP_PUSHDATA bs _]
      | isJust (opWitnessVersion ver)
        && BS.length bs >= 2
        && BS.length bs <= 40 ->
            Right $ PayWitness (fromJust (opWitnessVersion ver)) bs
    -- Provably unspendable data carrier output
    [OP_RETURN, OP_PUSHDATA bs _] -> Right $ DataCarrier bs
    -- Pay to MultiSig Keys
    _ -> matchPayMulSig s

witnessVersionOp :: Word8 -> Maybe ScriptOp
witnessVersionOp 0  = Just OP_0
witnessVersionOp 1  = Just OP_1
witnessVersionOp 2  = Just OP_2
witnessVersionOp 3  = Just OP_3
witnessVersionOp 4  = Just OP_4
witnessVersionOp 5  = Just OP_5
witnessVersionOp 6  = Just OP_6
witnessVersionOp 7  = Just OP_7
witnessVersionOp 8  = Just OP_8
witnessVersionOp 9  = Just OP_9
witnessVersionOp 10 = Just OP_10
witnessVersionOp 11 = Just OP_11
witnessVersionOp 12 = Just OP_12
witnessVersionOp 13 = Just OP_13
witnessVersionOp 14 = Just OP_14
witnessVersionOp 15 = Just OP_15
witnessVersionOp 16 = Just OP_16

opWitnessVersion :: ScriptOp -> Maybe Word8
opWitnessVersion OP_0  = Just 0
opWitnessVersion OP_1  = Just 1
opWitnessVersion OP_2  = Just 2
opWitnessVersion OP_3  = Just 3
opWitnessVersion OP_4  = Just 4
opWitnessVersion OP_5  = Just 5
opWitnessVersion OP_6  = Just 6
opWitnessVersion OP_7  = Just 7
opWitnessVersion OP_8  = Just 8
opWitnessVersion OP_9  = Just 9
opWitnessVersion OP_10 = Just 10
opWitnessVersion OP_11 = Just 11
opWitnessVersion OP_12 = Just 12
opWitnessVersion OP_13 = Just 13
opWitnessVersion OP_14 = Just 14
opWitnessVersion OP_15 = Just 15
opWitnessVersion OP_16 = Just 16
opWitnessVersion _ = Nothing


-- | Similar to 'decodeOutput' but decodes from a 'ByteString'.
decodeOutputBS :: ByteString -> Either String ScriptOutput
decodeOutputBS = decodeOutput <=< S.decode

-- | Computes a 'Script' from a standard 'ScriptOutput'.
encodeOutput :: ScriptOutput -> Script
encodeOutput s = Script $ case s of
    -- Pay to PubKey
    (PayPK k) -> [opPushData $ S.encode k, OP_CHECKSIG]
    -- Pay to PubKey Hash Address
    (PayPKHash h) ->
        [ OP_DUP
        , OP_HASH160
        , opPushData $ S.encode h
        , OP_EQUALVERIFY, OP_CHECKSIG
        ]
    -- Pay to MultiSig Keys
    (PayMulSig ps r)
      | r <= length ps ->
        let opM = intToScriptOp r
            opN = intToScriptOp $ length ps
            keys = map (opPushData . S.encode) ps
            in opM : keys ++ [opN, OP_CHECKMULTISIG]
      | otherwise -> error "encodeOutput: PayMulSig r must be <= than pkeys"
    -- Pay to Script Hash Address
    (PayScriptHash h) ->
        [ OP_HASH160, opPushData $ S.encode h, OP_EQUAL]
    -- Pay to Witness PubKey Hash Address
    (PayWitnessPKHash h) ->
        [ OP_0, opPushData $ S.encode h ]
    (PayWitnessScriptHash h) ->
        [ OP_0, opPushData $ S.encode h ]
    (PayWitness v h) ->
        [ case witnessVersionOp v of
              Nothing -> error "encodeOutput: invalid witness version"
              Just c  -> c
        , opPushData h ]
    -- Provably unspendable output
    (DataCarrier d) -> [OP_RETURN, opPushData d]

-- | Similar to 'encodeOutput' but encodes to a ByteString
encodeOutputBS :: ScriptOutput -> ByteString
encodeOutputBS = S.encode . encodeOutput

-- | Encode script as pay-to-script-hash script
toP2SH :: Script -> ScriptOutput
toP2SH = PayScriptHash . addressHash . S.encode

-- | Encode script as a pay-to-witness-script-hash script
toP2WSH :: Script -> ScriptOutput
toP2WSH = PayWitnessScriptHash . sha256 . S.encode

-- | Match @[OP_N, PubKey1, ..., PubKeyM, OP_M, OP_CHECKMULTISIG]@
matchPayMulSig :: Script -> Either String ScriptOutput
matchPayMulSig (Script ops) = case splitAt (length ops - 2) ops of
    (m:xs,[n,OP_CHECKMULTISIG]) -> do
        (intM,intN) <- liftM2 (,) (scriptOpToInt m) (scriptOpToInt n)
        if intM <= intN && length xs == intN
            then liftM2 PayMulSig (go xs) (return intM)
            else Left "matchPayMulSig: Invalid M or N parameters"
    _ -> Left "matchPayMulSig: script did not match output template"
  where
    go (OP_PUSHDATA bs _:xs) = liftM2 (:) (S.decode bs) (go xs)
    go []                    = return []
    go  _                    = Left "matchPayMulSig: invalid multisig opcode"

-- | Sort the public keys of a multisig output in ascending order by comparing
-- their compressed serialized representations. Refer to BIP-67.
sortMulSig :: ScriptOutput -> ScriptOutput
sortMulSig out = case out of
    PayMulSig keys r -> PayMulSig (sortBy (compare `on` encode) keys) r
    _ -> error "Can only call orderMulSig on PayMulSig scripts"

-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend, except in pay-to-witness-public-key-hash and
-- pay-to-script-hash transactions.
data SimpleInput = SpendPK
    { getInputSig :: !TxSignature
    -- ^ transaction signature
    }
    | SpendPKHash
    { getInputSig :: !TxSignature
    -- ^ embedded signature
    , getInputKey :: !PubKeyI
    -- ^ public key
    }
    | SpendMulSig
    { getInputMulSigKeys :: ![TxSignature]
    -- ^ list of signatures
    }
    deriving (Eq, Show, Generic, NFData)

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
data ScriptInput = RegularInput
    { getRegularInput :: !SimpleInput
    -- ^ get wrapped simple input
    }
    | ScriptHashInput
    { getScriptHashInput  :: !SimpleInput
    -- ^ get simple input associated with redeem script
    , getScriptHashRedeem :: !RedeemScript
    -- ^ redeem script
    }
    deriving (Eq, Show, Generic, NFData)

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

