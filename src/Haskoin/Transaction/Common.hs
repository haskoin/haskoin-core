{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haskoin.Transaction.Common
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Code related to transactions parsing and serialization.
-}
module Haskoin.Transaction.Common
    ( -- * Transactions
      Tx(..)
    , TxIn(..)
    , TxOut(..)
    , OutPoint(..)
    , TxHash(..)
    , WitnessData
    , WitnessStack
    , WitnessStackItem
    , txHash
    , hexToTxHash
    , txHashToHex
    , nosigTxHash
    , nullOutPoint
    ) where

import           Control.Applicative     ((<|>))
import           Control.DeepSeq
import           Control.Monad           (forM_, guard, liftM2, mzero,
                                          replicateM, (<=<))
import           Data.Aeson              as A
import           Data.Aeson.Encoding     (unsafeToEncoding)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.ByteString.Builder (char7)
import           Data.Hashable           (Hashable)
import           Data.Maybe              (fromMaybe)
import           Data.Serialize          as S
import           Data.String             (IsString, fromString)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Word               (Word32, Word64)
import           GHC.Generics            (Generic)
import           Haskoin.Crypto.Hash
import           Haskoin.Network.Common
import           Haskoin.Util
import           Text.Read               as R

-- | Transaction id: hash of transaction excluding witness data.
newtype TxHash = TxHash { getTxHash :: Hash256 }
    deriving (Eq, Ord, Generic, Hashable, Serialize, NFData)

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
    parseJSON = withText "txid" $
        maybe mzero return . hexToTxHash

instance ToJSON TxHash where
    toJSON = A.String . txHashToHex
    toEncoding h =
        unsafeToEncoding $ char7 '"' <> hexBuilder (B.reverse (S.encode h)) <> char7 '"'

-- | Transaction hash excluding signatures.
nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash $ doubleSHA256 $ S.encode tx { txIn = map clearInput $ txIn tx }
  where
    clearInput ti = ti { scriptInput = B.empty }

-- | Convert transaction hash to hex form, reversing bytes.
txHashToHex :: TxHash -> Text
txHashToHex (TxHash h) = encodeHex (B.reverse (S.encode h))

-- | Convert transaction hash from hex, reversing bytes.
hexToTxHash :: Text -> Maybe TxHash
hexToTxHash hex = do
    bs <- B.reverse <$> decodeHex hex
    h <- either (const Nothing) Just (S.decode bs)
    return $ TxHash h

-- | Witness stack for SegWit transactions.
type WitnessData = [WitnessStack]
-- | Witness stack for SegWit transactions.
type WitnessStack = [WitnessStackItem]
-- | Witness stack item for SegWit transactions.
type WitnessStackItem = ByteString

-- | Data type representing a transaction.
data Tx = Tx
    { -- | transaction data format version
      txVersion  :: !Word32
      -- | list of transaction inputs
    , txIn       :: ![TxIn]
      -- | list of transaction outputs
    , txOut      :: ![TxOut]
      -- | witness data for the transaction
    , txWitness  :: !WitnessData
      -- | earliest mining height or time
    , txLockTime :: !Word32
    } deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

-- | Compute transaction hash.
txHash :: Tx -> TxHash
txHash tx = TxHash (doubleSHA256 (S.encode tx {txWitness = []}))

instance IsString Tx where
    fromString =
        fromMaybe e . (eitherToMaybe . S.decode <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"

instance Serialize Tx where
    get = parseWitnessTx <|> parseLegacyTx
    put tx
        | null (txWitness tx) = putLegacyTx tx
        | otherwise = putWitnessTx tx

putInOut :: Tx -> Put
putInOut tx = do
    putVarInt $ length (txIn tx)
    forM_ (txIn tx) put
    putVarInt $ length (txOut tx)
    forM_ (txOut tx) put

-- | Non-SegWit transaction serializer.
putLegacyTx :: Tx -> Put
putLegacyTx tx = do
    putWord32le (txVersion tx)
    putInOut tx
    putWord32le (txLockTime tx)

-- | Witness transaciton serializer.
putWitnessTx :: Tx -> Put
putWitnessTx tx = do
    putWord32le (txVersion tx)
    putWord8 0x00
    putWord8 0x01
    putInOut tx
    putWitnessData (txWitness tx)
    putWord32le (txLockTime tx)

-- | Non-SegWit transaction deseralizer.
parseLegacyTx :: Get Tx
parseLegacyTx = do
    v <- getWord32le
    is <- replicateList =<< S.get
    os <- replicateList =<< S.get
    l <- getWord32le
    return
        Tx
        {txVersion = v, txIn = is, txOut = os, txWitness = [], txLockTime = l}
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) S.get

-- | Witness transaction deserializer.
parseWitnessTx :: Get Tx
parseWitnessTx = do
    v <- getWord32le
    m <- getWord8
    f <- getWord8
    guard $ m == 0x00
    guard $ f == 0x01
    is <- replicateList =<< S.get
    os <- replicateList =<< S.get
    w <- parseWitnessData $ length is
    l <- getWord32le
    return
        Tx {txVersion = v, txIn = is, txOut = os, txWitness = w, txLockTime = l}
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) S.get

-- | Witness data deserializer. Requires count of inputs.
parseWitnessData :: Int -> Get WitnessData
parseWitnessData n = replicateM n parseWitnessStack
  where
    parseWitnessStack = do
        VarInt i <- S.get
        replicateM (fromIntegral i) parseWitnessStackItem
    parseWitnessStackItem = do
        VarInt i <- S.get
        getByteString $ fromIntegral i

-- | Witness data serializer.
putWitnessData :: WitnessData -> Put
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
        Tx <$> o .: "version"
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
data TxIn =
    TxIn {
           -- | output being spent
           prevOutput   :: !OutPoint
           -- | signatures and redeem script
         , scriptInput  :: !ByteString
           -- | lock-time using sequence numbers (BIP-68)
         , txInSequence :: !Word32
         } deriving (Eq, Show, Read, Ord, Generic, Hashable, NFData)

instance Serialize TxIn where
    get =
        TxIn <$> S.get <*> (readBS =<< S.get) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len

    put (TxIn o s q) = do
        put o
        putVarInt $ B.length s
        putByteString s
        putWord32le q

instance FromJSON TxIn where
    parseJSON =
        withObject "TxIn" $ \o ->
            TxIn <$> o .: "prevoutput"
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
data TxOut =
    TxOut {
            -- | value of output is satoshi
            outValue     :: !Word64
            -- | pubkey script
          , scriptOutput :: !ByteString
          } deriving (Eq, Show, Read, Ord, Generic, Hashable, NFData)

instance Serialize TxOut where
    get = do
        val <- getWord64le
        (VarInt len) <- S.get
        TxOut val <$> getByteString (fromIntegral len)

    put (TxOut o s) = do
        putWord64le o
        putVarInt $ B.length s
        putByteString s

instance FromJSON TxOut where
    parseJSON =
        withObject "TxOut" $ \o ->
            TxOut <$> o .: "value"
                  <*> (maybe mzero return . decodeHex =<< o .: "outputscript")

instance ToJSON TxOut where
    toJSON (TxOut o s) =
        object ["value" .= o, "outputscript" .= encodeHex s]
    toEncoding (TxOut o s) =
        pairs ("value" .= o <> "outputscript" .= encodeHex s)

-- | The 'OutPoint' refers to a transaction output being spent.
data OutPoint = OutPoint
    { -- | hash of previous transaction
      outPointHash  :: !TxHash
      -- | position of output in previous transaction
    , outPointIndex :: !Word32
    } deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

instance Serialize OutPoint where
    get = do
        (h,i) <- liftM2 (,) S.get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

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
