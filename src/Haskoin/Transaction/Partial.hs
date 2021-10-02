{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-|
Module      : Haskoin.Transaction.Partial
Copyright   : No rights reserved
License     : MIT
Maintainer  : matt@bitnomial.com
Stability   : experimental
Portability : POSIX

Code related to PSBT parsing and serialization.
-}
module Haskoin.Transaction.Partial
    ( -- * Partially-Signed Transactions
      PartiallySignedTransaction (..)
    , Input (..)
    , Output (..)
    , UnknownMap (..)
    , Key (..)
    , merge
    , mergeInput
    , mergeOutput
    , complete
    , finalTransaction
    , emptyPSBT
    , emptyInput
    , emptyOutput
    ) where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq
import           Control.Monad              (guard, replicateM, void)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Bytes.Get             (runGetS)
import           Data.Bytes.Put             (runPutS)
import           Data.Bytes.Serial          (Serial (..))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Hashable              (Hashable)
import           Data.List                  (foldl')
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Serialize             (Get, Put, Serialize)
import qualified Data.Serialize             as S
import           GHC.Generics               (Generic)
import           GHC.Word                   (Word32, Word8)
import           Haskoin.Address            (Address (..), pubKeyAddr)
import           Haskoin.Keys               (Fingerprint, KeyIndex, PubKeyI)
import           Haskoin.Network            (VarInt (..), VarString (..),
                                             putVarInt)
import           Haskoin.Script             (Script (..), ScriptOp (..),
                                             ScriptOutput (..), SigHash,
                                             decodeOutput, decodeOutputBS,
                                             encodeOutputBS, isPayScriptHash,
                                             opPushData, toP2SH, toP2WSH)
import           Haskoin.Transaction.Common (Tx (..), TxOut, WitnessStack,
                                             outPointIndex, prevOutput,
                                             scriptInput, scriptOutput)
import           Haskoin.Util               (eitherToMaybe)

-- | PSBT data type as specified in
-- [BIP-174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki).
-- This contains an unsigned transaction, inputs and outputs, and unspecified
-- extra data. There is one input per input in the unsigned transaction, and one
-- output per output in the unsigned transaction. The inputs and outputs in the
-- 'PartiallySignedTransaction' line up by index with the inputs and outputs in
-- the unsigned transaction.
data PartiallySignedTransaction =
    PartiallySignedTransaction
    { unsignedTransaction :: Tx
    , globalUnknown       :: UnknownMap
    , inputs              :: [Input]
    , outputs             :: [Output]
    } deriving (Show, Eq, Generic)

instance NFData PartiallySignedTransaction

-- | Inputs contain all of the data needed to sign a transaction and all of the
-- resulting signature data after signing.
data Input = Input
    { nonWitnessUtxo     :: Maybe Tx
    , witnessUtxo        :: Maybe TxOut
    , partialSigs        :: HashMap PubKeyI ByteString
    , sigHashType        :: Maybe SigHash
    , inputRedeemScript  :: Maybe Script
    , inputWitnessScript :: Maybe Script
    , inputHDKeypaths    :: HashMap PubKeyI (Fingerprint, [KeyIndex])
    , finalScriptSig     :: Maybe Script
    , finalScriptWitness :: Maybe WitnessStack
    , inputUnknown       :: UnknownMap
    } deriving (Show, Eq, Generic)

instance NFData Input

-- | Outputs can contain information needed to spend the output at a later date.
data Output = Output
    { outputRedeemScript  :: Maybe Script
    , outputWitnessScript :: Maybe Script
    , outputHDKeypaths    :: HashMap PubKeyI (Fingerprint, [KeyIndex])
    , outputUnknown       :: UnknownMap
    } deriving (Show, Eq, Generic)

instance NFData Output

-- | A map of raw PSBT keys to byte strings for extra data. The 'keyType' field
-- cannot overlap with any of the reserved 'keyType' fields specified in the
-- PSBT specification.
newtype UnknownMap = UnknownMap { unknownMap :: HashMap Key ByteString }
    deriving (Show, Eq, Semigroup, Monoid, Generic)

instance NFData UnknownMap

-- | Raw keys for the map type used in PSBTs.
data Key = Key
    { keyType :: Word8
    , key     :: ByteString
    } deriving (Show, Eq, Generic)

instance NFData Key

instance Hashable Key

-- | Take two 'PartiallySignedTransaction's and merge them. The
-- 'unsignedTransaction' field in both must be the same.
merge :: PartiallySignedTransaction
      -> PartiallySignedTransaction
      -> Maybe PartiallySignedTransaction
merge psbt1 psbt2
    | unsignedTransaction psbt1 == unsignedTransaction psbt2
    = Just $ psbt1
        { globalUnknown = globalUnknown psbt1 <> globalUnknown psbt2
        , inputs = zipWith mergeInput (inputs psbt1) (inputs psbt2)
        , outputs = zipWith mergeOutput (outputs psbt1) (outputs psbt2)
        }
merge _ _ = Nothing

mergeInput :: Input -> Input -> Input
mergeInput a b = Input
    { nonWitnessUtxo =
            if isJust witUtx
            then Nothing
            else nonWitnessUtxo a <|> nonWitnessUtxo b
    , witnessUtxo =
            witUtx
    , sigHashType =
            sigHashType a <|> sigHashType b
    , partialSigs =
            partialSigs a <> partialSigs b
    , inputHDKeypaths =
            inputHDKeypaths a <> inputHDKeypaths b
    , inputUnknown =
            inputUnknown a <> inputUnknown b
    , inputRedeemScript =
            inputRedeemScript a <|> inputRedeemScript b
    , inputWitnessScript =
            inputWitnessScript a <|> inputWitnessScript b
    , finalScriptSig =
            finalScriptSig a <|> finalScriptSig b
    , finalScriptWitness =
            finalScriptWitness a <|> finalScriptWitness b
    }
  where
    witUtx = witnessUtxo a <|> witnessUtxo b

mergeOutput :: Output -> Output -> Output
mergeOutput a b = Output
    { outputRedeemScript =
            outputRedeemScript a <|> outputRedeemScript b
    , outputWitnessScript =
            outputWitnessScript a <|> outputWitnessScript b
    , outputHDKeypaths =
            outputHDKeypaths a <> outputHDKeypaths b
    , outputUnknown =
            outputUnknown a <> outputUnknown b
    }

-- | Take partial signatures from all of the 'Input's and finalize the signature.
complete :: PartiallySignedTransaction
         -> PartiallySignedTransaction
complete psbt =
    psbt
    {
        inputs = map
                 (completeInput . analyzeInputs)
                 (indexed $ inputs psbt)
    }
  where
    analyzeInputs (i, input) =
        (,)
        (outputScript =<< witnessUtxo input <|> nonWitScript)
        input
      where
        nonWitScript = getPrevOut i =<< nonWitnessUtxo input

    getPrevOut i tx =
       (txOut tx !!?) .
       fromIntegral .
       outPointIndex .
       prevOutput =<<
       txIn (unsignedTransaction psbt) !!? i
    xs !!? i = lookup i $ indexed xs

    outputScript = eitherToMaybe . decodeOutputBS . scriptOutput

    completeInput (Nothing, input)     = input
    completeInput (Just script, input) = completeSig input script

    indexed :: [a] -> [(Word32, a)]
    indexed = zip [0..]

completeSig :: Input -> ScriptOutput -> Input

completeSig input (PayPK k) =
    input
    { finalScriptSig =
            eitherToMaybe . runGetS deserialize =<<
            HashMap.lookup k (partialSigs input)
    }

completeSig input (PayPKHash h)
    | [(k, sig)] <- HashMap.toList (partialSigs input)
    , PubKeyAddress h == pubKeyAddr k =
            input
            {
                finalScriptSig =
                    Just $
                    Script
                    [
                        opPushData sig,
                        opPushData (runPutS (serialize k))
                    ]
            }

completeSig input (PayMulSig pubKeys m)
    | length sigs >= m =
          input { finalScriptSig = finalSig }
  where
    sigs = collectSigs m pubKeys input
    finalSig =
        Script .
        (OP_0 :) .
        (map opPushData sigs <>) .
        pure . opPushData . runPutS . serialize <$>
        inputRedeemScript input

completeSig input (PayScriptHash h)
    | Just rdmScript <- inputRedeemScript input
    , PayScriptHash h == toP2SH rdmScript
    , Right decodedScript <- decodeOutput rdmScript
    , not (isPayScriptHash decodedScript) =
            completeSig input decodedScript

completeSig input (PayWitnessPKHash h)
    | [(k, sig)] <- HashMap.toList (partialSigs input)
    , PubKeyAddress h == pubKeyAddr k =
            input
            {
                finalScriptWitness =
                    Just [sig, runPutS $ serialize k],
                finalScriptSig =
                    Script . pure . opPushData . runPutS . serialize <$>
                    inputRedeemScript input
            }
completeSig input (PayWitnessScriptHash h)
    | Just witScript <- inputWitnessScript input
    , PayWitnessScriptHash h == toP2WSH witScript
    , Right decodedScript <- decodeOutput witScript =
            completeWitnessSig input decodedScript

completeSig input _ = input

completeWitnessSig :: Input -> ScriptOutput -> Input
completeWitnessSig input script@(PayMulSig pubKeys m)
    | length sigs >= m =
          input
          {
              finalScriptWitness = Just finalWit,
              finalScriptSig = finalSig
          }
  where
    sigs = collectSigs m pubKeys input
    finalSig = Script . pure . opPushData . runPutS . serialize <$>
               inputRedeemScript input
    finalWit = mempty : sigs <> [encodeOutputBS script]

completeWitnessSig input _ = input

collectSigs :: Int -> [PubKeyI] -> Input -> [ByteString]
collectSigs m pubKeys input =
    take m . reverse $ foldl' lookupKey [] pubKeys
  where
    lookupKey sigs key =
        maybe sigs (: sigs) $
        HashMap.lookup key (partialSigs input)

-- | Take a finalized 'PartiallySignedTransaction' and produce the signed final
-- transaction. You may need to call 'complete' on the
-- 'PartiallySignedTransaction' before producing the final transaction.
finalTransaction :: PartiallySignedTransaction -> Tx
finalTransaction psbt =
    setInputs .
    foldl' finalizeInput ([], []) $
    zip (txIn tx) (inputs psbt)
  where
    tx = unsignedTransaction psbt
    hasWitness =
        any
        (isJust . finalScriptWitness)
        (inputs psbt)
    setInputs (ins, witData) =
        tx
        {
            txIn = reverse ins,
            txWitness = if hasWitness then reverse witData else []
        }
    finalizeInput (ins, witData) (txInput, psbtInput) =
        maybe finalWitness finalScript $
        finalScriptSig psbtInput
      where
        finalScript script =
            (
                txInput { scriptInput = runPutS $ serialize script } : ins,
                [] : witData
            )
        finalWitness =
            (
                ins,
                fromMaybe [] (finalScriptWitness psbtInput) : witData
            )

-- | Take an unsigned transaction and produce an empty
-- 'PartiallySignedTransaction'
emptyPSBT :: Tx -> PartiallySignedTransaction
emptyPSBT tx = PartiallySignedTransaction
    { unsignedTransaction = tx
    , globalUnknown = mempty
    , inputs = replicate (length (txIn tx)) emptyInput
    , outputs = replicate (length (txOut tx)) emptyOutput
    }

emptyInput :: Input
emptyInput = Input
    Nothing Nothing HashMap.empty Nothing
    Nothing Nothing HashMap.empty
    Nothing Nothing (UnknownMap HashMap.empty)

emptyOutput :: Output
emptyOutput = Output Nothing Nothing HashMap.empty (UnknownMap HashMap.empty)

instance Serialize PartiallySignedTransaction where
    get = do
        magic <- S.getBytes 4
        guard $ magic == "psbt"
        headerSep <- S.getWord8
        guard $ headerSep == 0xff

        keySize <- S.getWord8
        guard $ keySize == 1
        globalUnsignedTxType <- S.getWord8
        guard $ globalUnsignedTxType == 0x00
        unsignedTransaction <- getSizedBytes deserialize
        guard $ all (B.null . scriptInput) (txIn unsignedTransaction)
        guard $ null (txWitness unsignedTransaction)

        globalUnknown <- S.get
        globalEnd <- S.getWord8
        guard $ globalEnd == 0x00

        inputs <-
            replicateM
            (length (txIn unsignedTransaction))
            S.get
        outputs <-
            replicateM
            (length (txOut unsignedTransaction))
            S.get

        return
            PartiallySignedTransaction
            {
                unsignedTransaction,
                globalUnknown,
                inputs,
                outputs
            }

    put PartiallySignedTransaction
        {
            unsignedTransaction,
            globalUnknown,
            inputs,
            outputs
        } = do
        S.putByteString "psbt"
        S.putWord8 0xff -- Header separator

        S.putWord8 0x01 -- Key size
        S.putWord8 0x00 -- Unsigned Transaction type
        putSizedBytes $ serialize unsignedTransaction
        S.put globalUnknown
        S.putWord8 0x00 -- Global end

        mapM_ S.put inputs
        mapM_ S.put outputs

instance Serialize Key where
    get = do
        VarInt keySize <- deserialize
        guard $ keySize > 0
        t <- S.getWord8
        k <- S.getBytes (fromIntegral keySize - 1)
        return (Key t k)

    put (Key t k) = do
        putVarInt $ 1 + B.length k
        S.putWord8 t
        S.putByteString k

instance Serialize UnknownMap where
    get = go HashMap.empty
      where
        getItem m = do
            k <- S.get
            VarString v <- deserialize
            go $ HashMap.insert k v m
        go m = do
            isEnd <- S.lookAhead S.getWord8
            if isEnd == 0x00
                then return (UnknownMap m)
                else getItem m

    put (UnknownMap m) =
        void $
        HashMap.traverseWithKey
        (\k v -> S.put k >> serialize (VarString v))
        m

instance Serialize Input where
    get =
        getMap getInputItem setInputUnknown emptyInput
      where
        setInputUnknown f input =
            input
            {
                inputUnknown =
                    UnknownMap (f (unknownMap (inputUnknown input)))
            }

    put Input
        {
            nonWitnessUtxo,
            witnessUtxo,
            partialSigs,
            sigHashType,
            inputRedeemScript,
            inputWitnessScript,
            inputHDKeypaths,
            finalScriptSig,
            finalScriptWitness,
            inputUnknown
        } = do
        whenJust (putKeyValue InNonWitnessUtxo . serialize)
            nonWitnessUtxo
        whenJust (putKeyValue InWitnessUtxo . serialize)
            witnessUtxo
        putPartialSig partialSigs
        whenJust putSigHash
            sigHashType
        whenJust (putKeyValue InRedeemScript . serialize)
            inputRedeemScript
        whenJust (putKeyValue InWitnessScript . serialize)
            inputWitnessScript
        putHDPath InBIP32Derivation inputHDKeypaths
        whenJust (putKeyValue InFinalScriptSig . serialize)
            finalScriptSig
        whenJust (putKeyValue InFinalScriptWitness . serialize)
            finalScriptWitness
        S.put inputUnknown
        S.putWord8 0x00
      where
        putPartialSig =
            putPubKeyMap serialize InPartialSig . fmap VarString
        putSigHash sigHash = do
            putKey InSigHashType
            S.putWord8 0x04
            S.putWord32le (fromIntegral sigHash)

instance Serialize Output where
    get = getMap getOutputItem setOutputUnknown emptyOutput
      where
        setOutputUnknown f output =
            output
            {
                outputUnknown =
                    UnknownMap (f (unknownMap (outputUnknown output)))
            }

    put Output
        {
            outputRedeemScript,
            outputWitnessScript,
            outputHDKeypaths,
            outputUnknown
        } = do
        whenJust (putKeyValue OutRedeemScript . serialize)
            outputRedeemScript
        whenJust (putKeyValue OutWitnessScript . serialize)
            outputWitnessScript
        putHDPath OutBIP32Derivation
            outputHDKeypaths
        S.put outputUnknown
        S.putWord8 0x00

putSizedBytes :: Put -> Put
putSizedBytes f = do
    putVarInt (B.length bs)
    S.putByteString bs
  where
    bs = S.runPut f

getSizedBytes :: Get a -> Get a
getSizedBytes f =
    S.getNested
    (fromIntegral . getVarInt <$> deserialize)
    f

putKeyValue :: Enum t => t -> Put -> Put
putKeyValue t v = do
    putKey t
    putSizedBytes v

putKey :: Enum t => t -> Put
putKey t = do
    putVarInt (1 :: Word8)
    S.putWord8 (enumWord8 t)

getMap :: (Bounded t, Enum t)
       => (Int -> a -> t -> Get a)
       -> ((HashMap Key ByteString -> HashMap Key ByteString) -> a -> a)
       -> a -> Get a
getMap getMapItem setUnknown = go
  where
    getItem keySize m (Right t) =
        getMapItem (fromIntegral keySize - 1) m t >>= go
    getItem keySize m (Left t)  = do
        k <- S.getBytes (fromIntegral keySize - 1)
        VarString v <- deserialize
        go $ setUnknown (HashMap.insert (Key t k) v) m
    go m = do
        keySize <- getVarInt <$> deserialize
        if keySize == 0
            then return m
            else getItem keySize m =<< (word8Enum <$> S.getWord8)

data InputType
    = InNonWitnessUtxo
    | InWitnessUtxo
    | InPartialSig
    | InSigHashType
    | InRedeemScript
    | InWitnessScript
    | InBIP32Derivation
    | InFinalScriptSig
    | InFinalScriptWitness
    deriving (Show, Eq, Enum, Bounded, Generic)

instance NFData InputType

data OutputType
    = OutRedeemScript
    | OutWitnessScript
    | OutBIP32Derivation
    deriving (Show, Eq, Enum, Bounded, Generic)

instance NFData OutputType

getInputItem :: Int -> Input -> InputType -> Get Input

getInputItem 0 input@Input{nonWitnessUtxo = Nothing} InNonWitnessUtxo = do
    utxo <- getSizedBytes deserialize
    return input { nonWitnessUtxo = Just utxo }

getInputItem 0 input@Input{witnessUtxo = Nothing} InWitnessUtxo = do
    utxo <- getSizedBytes deserialize
    return input { witnessUtxo = Just utxo }

getInputItem keySize input InPartialSig = do
    (k, v) <- getPartialSig
    return
        input
        {
            partialSigs = HashMap.insert k v (partialSigs input)
        }
  where
    getPartialSig =
        (,)
        <$> S.isolate keySize deserialize
        <*> (getVarString <$> deserialize)

getInputItem 0 input@Input{sigHashType = Nothing} InSigHashType = do
    VarInt size <- deserialize
    guard $ size == 0x04
    sigHash <- fromIntegral <$> S.getWord32le
    return $ input { sigHashType = Just sigHash }

getInputItem 0 input@Input{inputRedeemScript = Nothing} InRedeemScript = do
    script <- getSizedBytes deserialize
    return $ input { inputRedeemScript = Just script }

getInputItem 0 input@Input{inputWitnessScript = Nothing} InWitnessScript = do
    script <- getSizedBytes deserialize
    return $ input { inputWitnessScript = Just script }

getInputItem keySize input InBIP32Derivation = do
    (k, v) <- getHDPath keySize
    return
        input
        {
            inputHDKeypaths = HashMap.insert k v (inputHDKeypaths input)
        }

getInputItem 0 input@Input{finalScriptSig = Nothing} InFinalScriptSig = do
    script <- getSizedBytes deserialize
    return $ input { finalScriptSig = Just script }

getInputItem 0 input@Input{finalScriptWitness = Nothing} InFinalScriptWitness = do
    scripts <- map getVarString <$> getVarIntList
    return $ input { finalScriptWitness = Just scripts }
  where
    getVarIntList = do
        VarInt n <- deserialize
        replicateM (fromIntegral n) deserialize

getInputItem keySize input inputType = fail $
    "Incorrect key size for input item or item already existed: " <>
    show (keySize, input, inputType)

getOutputItem :: Int -> Output -> OutputType -> Get Output

getOutputItem 0 output@Output{outputRedeemScript = Nothing} OutRedeemScript = do
    script <- getSizedBytes deserialize
    return $ output { outputRedeemScript = Just script }

getOutputItem 0 output@Output{outputWitnessScript = Nothing} OutWitnessScript = do
    script <- getSizedBytes deserialize
    return $ output { outputWitnessScript = Just script }

getOutputItem keySize output OutBIP32Derivation = do
    (k, v) <- getHDPath keySize
    return $ output { outputHDKeypaths = HashMap.insert k v (outputHDKeypaths output) }

getOutputItem keySize output outputType = fail $
    "Incorrect key size for output item or item already existed: " <>
    show (keySize, output, outputType)

getHDPath :: Int -> Get (PubKeyI, (Fingerprint, [KeyIndex]))
getHDPath keySize =
    (,)
    <$> S.isolate keySize deserialize
    <*> (unPSBTHDPath <$> S.get)

putHDPath :: Enum t => t -> HashMap PubKeyI (Fingerprint, [KeyIndex]) -> Put
putHDPath t = putPubKeyMap S.put t . fmap PSBTHDPath

newtype PSBTHDPath =
    PSBTHDPath { unPSBTHDPath :: (Fingerprint, [KeyIndex]) }
    deriving (Show, Eq, Generic)

instance NFData PSBTHDPath

instance Serialize PSBTHDPath where
    get = do
        VarInt valueSize <- deserialize
        guard $ valueSize `mod` 4 == 0
        let numIndices = (fromIntegral valueSize - 4) `div` 4
        PSBTHDPath <$>
            S.isolate
                (fromIntegral valueSize)
                ((,) <$> S.getWord32be <*> getKeyIndexList numIndices)
      where
        getKeyIndexList n = replicateM n S.getWord32le

    put (PSBTHDPath (fp, kis)) = do
        putVarInt (B.length bs)
        S.putByteString bs
      where
        bs = S.runPut $ S.putWord32be fp >> mapM_ S.putWord32le kis

putPubKeyMap :: Enum t => (a -> Put) -> t -> HashMap PubKeyI a -> Put
putPubKeyMap f t =
    void . HashMap.traverseWithKey putItem
  where
    putItem k v = do
        S.put $ Key (enumWord8 t) (runPutS (serialize k))
        f v

enumWord8 :: Enum a => a -> Word8
enumWord8 = fromIntegral . fromEnum

word8Enum :: forall a. (Bounded a, Enum a) => Word8 -> Either Word8 a
word8Enum n | n <= enumWord8 (maxBound :: a) = Right . toEnum $ fromIntegral n
word8Enum n = Left n

whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe (return ())
