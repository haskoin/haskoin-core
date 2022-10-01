{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Code related to transactions parsing and serialization.
module Bitcoin.Transaction.Common (
    -- * Transactions
    Tx (..),
    TxIn (..),
    TxOut (..),
    OutPoint (..),
    TxHash (..),
    WitnessData,
    WitnessStack,
    WitnessStackItem,
    txHash,
    hexToTxHash,
    txHashToHex,
    nosigTxHash,
    nullOutPoint,
) where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (
    forM_,
    guard,
    liftM2,
    mzero,
    replicateM,
    unless,
    when,
    (<=<),
 )
import Data.Aeson as A
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (char7)
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..))
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Bitcoin.Crypto.Hash
import Bitcoin.Network.Common
import Bitcoin.Util
import Text.Read as R


-- | Transaction id: hash of transaction excluding witness data.
newtype TxHash = TxHash {getTxHash :: Hash256}
    deriving (Eq, Ord, Generic, Hashable, Serial, NFData)


instance Serialize TxHash where
    put = serialize
    get = deserialize


instance Binary TxHash where
    put = serialize
    get = deserialize


instance Show TxHash where
    showsPrec _ = shows . txHashToHex


instance Read TxHash where
    readPrec = do
        R.String str <- R.lexP
        maybe R.pfail return $ hexToTxHash $ cs str


instance IsString TxHash where
    fromString s =
        let e = error "Could not read transaction hash from hex string"
         in fromMaybe e $ hexToTxHash $ cs s


instance FromJSON TxHash where
    parseJSON =
        withText "txid" $
            maybe mzero return . hexToTxHash


instance ToJSON TxHash where
    toJSON = A.String . txHashToHex
    toEncoding h =
        unsafeToEncoding $
            char7 '"'
                <> hexBuilder (BL.reverse (runPutL (serialize h)))
                <> char7 '"'


-- | Transaction hash excluding signatures.
nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash $
        doubleSHA256 $
            runPutS $
                serialize tx{txIn = map clearInput $ txIn tx}
  where
    clearInput ti = ti{scriptInput = B.empty}


-- | Convert transaction hash to hex form, reversing bytes.
txHashToHex :: TxHash -> Text
txHashToHex (TxHash h) = encodeHex (B.reverse (runPutS (serialize h)))


-- | Convert transaction hash from hex, reversing bytes.
hexToTxHash :: Text -> Maybe TxHash
hexToTxHash hex = do
    bs <- B.reverse <$> decodeHex hex
    h <- either (const Nothing) Just (runGetS deserialize bs)
    return $ TxHash h


-- | Witness stack for SegWit transactions.
type WitnessData = [WitnessStack]


-- | Witness stack for SegWit transactions.
type WitnessStack = [WitnessStackItem]


-- | Witness stack item for SegWit transactions.
type WitnessStackItem = ByteString


-- | Data type representing a transaction.
data Tx = Tx
    { txVersion :: !Word32
    -- ^ transaction data format version
    , txIn :: ![TxIn]
    -- ^ list of transaction inputs
    , txOut :: ![TxOut]
    -- ^ list of transaction outputs
    , txWitness :: !WitnessData
    -- ^ witness data for the transaction
    , txLockTime :: !Word32
    -- ^ earliest mining height or time
    }
    deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)


-- | Compute transaction hash.
txHash :: Tx -> TxHash
txHash tx = TxHash . doubleSHA256 . runPutS $ serialize tx{txWitness = []}


instance IsString Tx where
    fromString =
        fromMaybe e . (eitherToMaybe . runGetS deserialize <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"


instance Serial Tx where
    deserialize =
        isWitnessTx >>= \w -> if w then parseWitnessTx else parseLegacyTx
    serialize tx
        | null (txWitness tx) = putLegacyTx tx
        | otherwise = putWitnessTx tx


instance Binary Tx where
    put = serialize
    get = deserialize


instance Serialize Tx where
    put = serialize
    get = deserialize


putInOut :: MonadPut m => Tx -> m ()
putInOut tx = do
    putVarInt $ length (txIn tx)
    forM_ (txIn tx) serialize
    putVarInt $ length (txOut tx)
    forM_ (txOut tx) serialize


-- | Non-SegWit transaction serializer.
putLegacyTx :: MonadPut m => Tx -> m ()
putLegacyTx tx = do
    putWord32le (txVersion tx)
    putInOut tx
    putWord32le (txLockTime tx)


-- | Witness transaciton serializer.
putWitnessTx :: MonadPut m => Tx -> m ()
putWitnessTx tx = do
    putWord32le (txVersion tx)
    putWord8 0x00
    putWord8 0x01
    putInOut tx
    putWitnessData (txWitness tx)
    putWord32le (txLockTime tx)


isWitnessTx :: MonadGet m => m Bool
isWitnessTx = lookAhead $ do
    _ <- getWord32le
    m <- getWord8
    f <- getWord8
    return (m == 0x00 && f == 0x01)


-- | Non-SegWit transaction deseralizer.
parseLegacyTx :: MonadGet m => m Tx
parseLegacyTx = do
    v <- getWord32le
    is <- replicateList =<< deserialize
    os <- replicateList =<< deserialize
    when (length is == 0x00 && length os == 0x01) $ fail "Witness transaction"
    l <- getWord32le
    return
        Tx
            { txVersion = v
            , txIn = is
            , txOut = os
            , txWitness = []
            , txLockTime = l
            }
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) deserialize


-- | Witness transaction deserializer.
parseWitnessTx :: MonadGet m => m Tx
parseWitnessTx = do
    v <- getWord32le
    m <- getWord8
    f <- getWord8
    unless (m == 0x00 && f == 0x01) $ fail "Not a witness transaction"
    is <- replicateList =<< deserialize
    os <- replicateList =<< deserialize
    w <- parseWitnessData $ length is
    l <- getWord32le
    return
        Tx{txVersion = v, txIn = is, txOut = os, txWitness = w, txLockTime = l}
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) deserialize


-- | Witness data deserializer. Requires count of inputs.
parseWitnessData :: MonadGet m => Int -> m WitnessData
parseWitnessData n = replicateM n parseWitnessStack
  where
    parseWitnessStack = do
        VarInt i <- deserialize
        replicateM (fromIntegral i) parseWitnessStackItem
    parseWitnessStackItem = do
        VarInt i <- deserialize
        getByteString $ fromIntegral i


-- | Witness data serializer.
putWitnessData :: MonadPut m => WitnessData -> m ()
putWitnessData = mapM_ putWitnessStack
  where
    putWitnessStack ws = do
        putVarInt $ length ws
        mapM_ putWitnessStackItem ws
    putWitnessStackItem bs = do
        putVarInt $ B.length bs
        putByteString bs


instance FromJSON Tx where
    parseJSON = withObject "Tx" $ \o ->
        Tx
            <$> o .: "version"
            <*> o .: "inputs"
            <*> o .: "outputs"
            <*> (mapM (mapM f) =<< o .: "witnessdata")
            <*> o .: "locktime"
      where
        f = maybe mzero return . decodeHex


instance ToJSON Tx where
    toJSON (Tx v i o w l) =
        object
            [ "version" .= v
            , "inputs" .= i
            , "outputs" .= o
            , "witnessdata" .= fmap (fmap encodeHex) w
            , "locktime" .= l
            ]
    toEncoding (Tx v i o w l) =
        pairs
            ( "version" .= v
                <> "inputs" .= i
                <> "outputs" .= o
                <> "witnessdata" .= fmap (fmap encodeHex) w
                <> "locktime" .= l
            )


-- | Data type representing a transaction input.
data TxIn = TxIn
    { prevOutput :: !OutPoint
    -- ^ output being spent
    , scriptInput :: !ByteString
    -- ^ signatures and redeem script
    , txInSequence :: !Word32
    -- ^ lock-time using sequence numbers (BIP-68)
    }
    deriving (Eq, Show, Read, Ord, Generic, Hashable, NFData)


instance Serial TxIn where
    deserialize =
        TxIn <$> deserialize <*> (readBS =<< deserialize) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len


    serialize (TxIn o s q) = do
        serialize o
        putVarInt $ B.length s
        putByteString s
        putWord32le q


instance Binary TxIn where
    get = deserialize
    put = serialize


instance Serialize TxIn where
    get = deserialize
    put = serialize


instance FromJSON TxIn where
    parseJSON =
        withObject "TxIn" $ \o ->
            TxIn
                <$> o .: "prevoutput"
                <*> (maybe mzero return . decodeHex =<< o .: "inputscript")
                <*> o .: "sequence"


instance ToJSON TxIn where
    toJSON (TxIn o s q) =
        object
            [ "prevoutput" .= o
            , "inputscript" .= encodeHex s
            , "sequence" .= q
            ]
    toEncoding (TxIn o s q) =
        pairs
            ( "prevoutput" .= o
                <> "inputscript" .= encodeHex s
                <> "sequence" .= q
            )


-- | Data type representing a transaction output.
data TxOut = TxOut
    { outValue :: !Word64
    -- ^ value of output is satoshi
    , scriptOutput :: !ByteString
    -- ^ pubkey script
    }
    deriving (Eq, Show, Read, Ord, Generic, Hashable, NFData)


instance Serial TxOut where
    deserialize = do
        val <- getWord64le
        VarInt len <- deserialize
        TxOut val <$> getByteString (fromIntegral len)


    serialize (TxOut o s) = do
        putWord64le o
        putVarInt $ B.length s
        putByteString s


instance Binary TxOut where
    put = serialize
    get = deserialize


instance Serialize TxOut where
    put = serialize
    get = deserialize


instance FromJSON TxOut where
    parseJSON =
        withObject "TxOut" $ \o ->
            TxOut
                <$> o .: "value"
                <*> (maybe mzero return . decodeHex =<< o .: "outputscript")


instance ToJSON TxOut where
    toJSON (TxOut o s) =
        object ["value" .= o, "outputscript" .= encodeHex s]
    toEncoding (TxOut o s) =
        pairs ("value" .= o <> "outputscript" .= encodeHex s)


-- | The 'OutPoint' refers to a transaction output being spent.
data OutPoint = OutPoint
    { outPointHash :: !TxHash
    -- ^ hash of previous transaction
    , outPointIndex :: !Word32
    -- ^ position of output in previous transaction
    }
    deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)


instance Serial OutPoint where
    deserialize = do
        (h, i) <- liftM2 (,) deserialize getWord32le
        return $ OutPoint h i
    serialize (OutPoint h i) = serialize h >> putWord32le i


instance Binary OutPoint where
    put = serialize
    get = deserialize


instance Serialize OutPoint where
    put = serialize
    get = deserialize


instance FromJSON OutPoint where
    parseJSON =
        withObject "OutPoint" $ \o ->
            OutPoint <$> o .: "txid" <*> o .: "index"


instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object ["txid" .= h, "index" .= i]
    toEncoding (OutPoint h i) = pairs ("txid" .= h <> "index" .= i)


-- | Outpoint used in coinbase transactions.
nullOutPoint :: OutPoint
nullOutPoint =
    OutPoint
        { outPointHash =
            "0000000000000000000000000000000000000000000000000000000000000000"
        , outPointIndex = maxBound
        }
