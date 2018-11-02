{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Haskoin.Transaction.Partial
    ( PartiallySignedTransaction (..)
    , Input (..)
    , Output (..)
    , UnknownMap (..)
    , Key (..)
    , emptyInput
    , emptyOutput
    ) where

import           Control.Monad               (guard, replicateM, void)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Proxy                  (Proxy)
import           Data.Serialize              as S
import           GHC.Generics                (Generic)
import           GHC.Word                    (Word64, Word8)
import           Network.Haskoin.Keys        (Fingerprint, KeyIndex, PubKeyI)
import           Network.Haskoin.Network     (VarInt (..), VarString (..),
                                              putVarInt)
import           Network.Haskoin.Script      (Script, SigHash)
import           Network.Haskoin.Transaction (Tx (..), TxOut, scriptInput)

data PartiallySignedTransaction = PartiallySignedTransaction
    { unsignedTransaction :: Tx
    , globalUnknown       :: UnknownMap
    , inputs              :: [Input]
    , outputs             :: [Output]
    } deriving (Show, Eq)

data Input = Input
    { nonWitnessUtxo     :: Maybe Tx
    , witnessUtxo        :: Maybe TxOut
    , partialSigs        :: HashMap PubKeyI ByteString
    , sigHashType        :: Maybe SigHash
    , inputRedeemScript  :: Maybe Script
    , inputWitnessScript :: Maybe Script
    , inputHDKeypaths    :: HashMap PubKeyI (Fingerprint, [KeyIndex])
    , finalScriptSig     :: Maybe Script
    , finalScriptWitness :: Maybe [ByteString]
    , inputUnknown       :: UnknownMap
    } deriving (Show, Eq)

data Output = Output
    { outputRedeemScript  :: Maybe Script
    , outputWitnessScript :: Maybe Script
    , outputHDKeypaths    :: HashMap PubKeyI (Fingerprint, [KeyIndex])
    , outputUnknown       :: UnknownMap
    } deriving (Show, Eq)

newtype UnknownMap = UnknownMap { unknownMap :: HashMap Key ByteString }
    deriving (Show, Eq)

data Key = Key
    { keyType :: Word8
    , key     :: ByteString
    } deriving (Show, Eq, Generic, Hashable)

emptyInput :: Input
emptyInput = Input
    Nothing Nothing HashMap.empty Nothing
    Nothing Nothing HashMap.empty
    Nothing Nothing (UnknownMap HashMap.empty)

emptyOutput :: Output
emptyOutput = Output Nothing Nothing HashMap.empty (UnknownMap HashMap.empty)

instance Serialize PartiallySignedTransaction where
    get = do
        magic <- getBytes 4
        guard $ magic == "psbt"
        headerSep <- getWord8
        guard $ headerSep == 0xff

        keySize <- getWord8
        guard $ keySize == 1
        globalUnsignedTxType <- getWord8
        guard $ globalUnsignedTxType == 0x00
        unsignedTransaction <- getSizedBytes
        guard $ all (B.null . scriptInput) (txIn unsignedTransaction)
        guard $ null (txWitness unsignedTransaction)

        globalUnknown <- get
        globalEnd <- getWord8
        guard $ globalEnd == 0x00

        inputs <- replicateM (length $ txIn unsignedTransaction) (label "input" get)
        outputs <- replicateM (length $ txOut unsignedTransaction) (label "output" get)

        return PartiallySignedTransaction { unsignedTransaction, globalUnknown, inputs, outputs }

    put PartiallySignedTransaction{ unsignedTransaction, globalUnknown, inputs, outputs } = do
        putByteString "psbt"
        putWord8 0xff -- Header separator

        putWord8 0x01 -- Key size
        putWord8 0x00 -- Unsigned Transaction type
        putSizedBytes unsignedTransaction
        put globalUnknown
        putWord8 0x00 -- Global end

        mapM_ put inputs
        mapM_ put outputs

instance Serialize Key where
    get = do
        VarInt keySize <- get
        guard $ keySize > 0
        t <- getWord8
        k <- getBytes (fromIntegral keySize - 1)
        return (Key t k)

    put (Key t k) = do
        putVarInt $ 1 + B.length k
        putWord8 t
        putByteString k

instance Serialize UnknownMap where
    get = go HashMap.empty
      where
        getItem m = do
            k <- get
            VarString v <- get
            go $ HashMap.insert k v m
        go m = do
            isEnd <- lookAhead getWord8
            if isEnd == 0x00
                then return (UnknownMap m)
                else getItem m

    put (UnknownMap m) = void $ HashMap.traverseWithKey (\k v -> put k >> put (VarString v)) m

instance Serialize Input where
    get = getMap getInputItem setInputUnknown emptyInput
      where
        setInputUnknown f input = input { inputUnknown = UnknownMap $ f (unknownMap $ inputUnknown input) }

    put Input { nonWitnessUtxo, witnessUtxo, partialSigs, sigHashType
                  , inputRedeemScript, inputWitnessScript, inputHDKeypaths
                  , finalScriptSig, finalScriptWitness, inputUnknown
                  } = do
        whenJust (putKeyValue InNonWitnessUtxo) nonWitnessUtxo
        whenJust (putKeyValue InWitnessUtxo) witnessUtxo
        putPartialSig partialSigs
        whenJust putSigHash sigHashType
        whenJust (putKeyValue InRedeemScript) inputRedeemScript
        whenJust (putKeyValue InWitnessScript) inputWitnessScript
        putHDPath InBIP32Derivation inputHDKeypaths
        whenJust (putKeyValue InFinalScriptSig) finalScriptSig
        whenJust (putKeyValue InFinalScriptWitness) finalScriptWitness
        put inputUnknown
        putWord8 0x00
      where
        putPartialSig = putPubKeyMap InPartialSig . fmap VarString
        putSigHash sigHash = do
            putVarInt 1 >> putWord8 (enumWord8 InSigHashType)
            putWord8 0x04
            putWord32le (fromIntegral sigHash)

instance Serialize Output where
    get = getMap getOutputItem setOutputUnknown emptyOutput
      where
        setOutputUnknown f output = output { outputUnknown = UnknownMap $ f (unknownMap $ outputUnknown output) }

    put Output{ outputRedeemScript, outputWitnessScript, outputHDKeypaths, outputUnknown } = do
        whenJust (putKeyValue OutRedeemScript) outputRedeemScript
        whenJust (putKeyValue OutWitnessScript) outputWitnessScript
        putHDPath OutBIP32Derivation outputHDKeypaths
        put outputUnknown
        putWord8 0x00

putSizedBytes :: Serialize a => a -> Put
putSizedBytes a = putVarInt (B.length bs) >> putByteString bs
  where bs = encode a

getSizedBytes :: Serialize a => Get a
getSizedBytes = getNested (fromIntegral . getVarInt <$> get) get

putKeyValue :: (Enum t, Serialize v) => t -> v -> Put
putKeyValue t v = putVarInt 1 >> putWord8 (enumWord8 t) >> putSizedBytes v

getMap :: (Bounded t, Enum t)
       => (Int -> a -> t -> Get a)
       -> ((HashMap Key ByteString -> HashMap Key ByteString) -> a -> a)
       -> a -> Get a
getMap getMapItem setUnknown = go
  where
    getItem keySize m (Right t) = getMapItem (fromIntegral keySize - 1) m t >>= go
    getItem keySize m (Left t)  = do
        k <- getBytes (fromIntegral keySize - 1)
        VarString v <- get
        go $ setUnknown (HashMap.insert (Key t k) v) m
    go m = do
        keySize <- getVarInt <$> get
        if keySize == 0
            then return m
            else getItem keySize m =<< (word8Enum <$> getWord8)

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
    deriving (Show, Eq, Enum, Bounded)

data OutputType
    = OutRedeemScript
    | OutWitnessScript
    | OutBIP32Derivation
    deriving (Show, Eq, Enum, Bounded)

getInputItem :: Int -> Input -> InputType -> Get Input
getInputItem 0 input@Input{nonWitnessUtxo = Nothing} InNonWitnessUtxo = do
    utxo <- getSizedBytes
    return $ input { nonWitnessUtxo = Just utxo }
getInputItem 0 input@Input{witnessUtxo = Nothing} InWitnessUtxo = do
    utxo <- getSizedBytes
    return $ input { witnessUtxo = Just utxo }
getInputItem keySize input InPartialSig = do
    (k, v) <- getPartialSig keySize
    return $ input { partialSigs = HashMap.insert k v (partialSigs input) }
  where
    getPartialSig keySize = (,) <$> isolate keySize get <*> (getVarString <$> get)
getInputItem 0 input@Input{sigHashType = Nothing} InSigHashType = do
    VarInt size <- get
    guard $ size == 0x04
    sigHash <- fromIntegral <$> getWord32le
    return $ input { sigHashType = Just sigHash }
getInputItem 0 input@Input{inputRedeemScript = Nothing} InRedeemScript = do
    script <- getSizedBytes
    return $ input { inputRedeemScript = Just script }
getInputItem 0 input@Input{inputWitnessScript = Nothing} InWitnessScript = do
    script <- getSizedBytes
    return $ input { inputWitnessScript = Just script }
getInputItem keySize input InBIP32Derivation = do
    (k, v) <- getHDPath keySize
    return $ input { inputHDKeypaths = HashMap.insert k v (inputHDKeypaths input) }
getInputItem 0 input@Input{finalScriptSig = Nothing} InFinalScriptSig = do
    script <- getSizedBytes
    return $ input { finalScriptSig = Just script }
getInputItem 0 input@Input{finalScriptWitness = Nothing} InFinalScriptWitness = do
    scripts <- map getVarString <$> getVarIntList
    return $ input { finalScriptWitness = Just scripts }
  where
    getVarIntList = do
        VarInt n <- get
        replicateM (fromIntegral n) get
getInputItem keySize input inputType = fail $
    "Incorrect key size for input item or item already existed: " <>
    show (keySize, input, inputType)

getOutputItem :: Int -> Output -> OutputType -> Get Output
getOutputItem 0 output@Output{outputRedeemScript = Nothing} OutRedeemScript = do
    script <- getSizedBytes
    return $ output { outputRedeemScript = Just script }
getOutputItem 0 output@Output{outputWitnessScript = Nothing} OutWitnessScript = do
    script <- getSizedBytes
    return $ output { outputWitnessScript = Just script }
getOutputItem keySize output OutBIP32Derivation = do
    (k, v) <- getHDPath keySize
    return $ output { outputHDKeypaths = HashMap.insert k v (outputHDKeypaths output) }
getOutputItem keySize output outputType = fail $
    "Incorrect key size for output item or item already existed: " <>
    show (keySize, output, outputType)

getHDPath :: Int -> Get (PubKeyI, (Fingerprint, [KeyIndex]))
getHDPath keySize = (,) <$> isolate keySize get <*> (unPSBTHDPath <$> get)

putHDPath :: Enum t => t -> HashMap PubKeyI (Fingerprint, [KeyIndex]) -> Put
putHDPath t = putPubKeyMap t . fmap PSBTHDPath

newtype PSBTHDPath = PSBTHDPath { unPSBTHDPath :: (Fingerprint, [KeyIndex]) }
    deriving (Show, Eq)

instance Serialize PSBTHDPath where
    get = do
        VarInt valueSize <- get
        guard $ valueSize `mod` 4 == 0
        let numIndices = (fromIntegral valueSize - 4) `div` 4
        PSBTHDPath <$> isolate (fromIntegral valueSize) ((,) <$> getWord32le <*> getKeyIndexList numIndices)
      where
        getKeyIndexList n = replicateM n getWord32le

    put (PSBTHDPath (fp, kis)) = putVarInt (B.length bs) >> putByteString bs
      where bs = runPut $ putWord32le fp >> mapM_ putWord32le kis

putPubKeyMap :: (Serialize a, Enum t) => t -> HashMap PubKeyI a -> Put
putPubKeyMap t = void . HashMap.traverseWithKey putItem
  where
    putItem k v = put (Key (enumWord8 t) (encode k)) >> put v

enumWord8 :: Enum a => a -> Word8
enumWord8 = fromIntegral . fromEnum

word8Enum :: forall a. (Bounded a, Enum a) => Word8 -> Either Word8 a
word8Enum n | n <= enumWord8 (maxBound :: a) = Right . toEnum $ fromIntegral n
word8Enum n = Left n

whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe (return ())
