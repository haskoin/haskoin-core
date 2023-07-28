{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Transaction.Common
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Code related to transactions parsing and serialization.
module Haskoin.Transaction.Common
  ( -- * Transactions
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
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (forM_, guard, liftM2, mzero, replicateM, unless, when, (<=<))
import Data.Aeson as A
import Data.Aeson.Encoding qualified as E
import Data.Binary (Binary (..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (char7)
import Data.ByteString.Lazy qualified as BL
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
import Haskoin.Crypto.Hash
import Haskoin.Network.Common
import Haskoin.Util.Helpers
import Text.Read as R

-- | Transaction id: hash of transaction excluding witness data.
newtype TxHash = TxHash {get :: Hash256}
  deriving (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData)

instance Serial TxHash where
  serialize (TxHash h) = serialize h
  deserialize = TxHash <$> deserialize

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
  toEncoding = hexEncoding . BL.reverse . runPutL . serialize

-- | Transaction hash excluding signatures.
nosigTxHash :: Tx -> TxHash
nosigTxHash Tx {..} =
  TxHash . doubleSHA256 . runPutS $ serialize tx
  where
    tx = Tx {inputs = map clr inputs, ..}
    clr TxIn {..} = TxIn {script = B.empty, ..}

-- | Convert transaction hash to hex form, reversing bytes.
txHashToHex :: TxHash -> Text
txHashToHex (TxHash h) =
  encodeHex . B.reverse . runPutS $ serialize h

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
  { -- | transaction data format version
    version :: !Word32,
    -- | list of transaction inputs
    inputs :: ![TxIn],
    -- | list of transaction outputs
    outputs :: ![TxOut],
    -- | witness data for the transaction
    witness :: !WitnessData,
    -- | earliest mining height or time
    locktime :: !Word32
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

-- | Compute transaction hash.
txHash :: Tx -> TxHash
txHash tx = TxHash . doubleSHA256 . runPutS $ serialize tx {witness = []}

instance IsString Tx where
  fromString =
    fromMaybe e . (eitherToMaybe . runGetS deserialize <=< decodeHex) . cs
    where
      e = error "Could not read transaction from hex string"

instance Serial Tx where
  deserialize = isWitnessTx >>= bool parseLegacyTx parseWitnessTx
  serialize tx
    | null tx.witness = putLegacyTx tx
    | otherwise = putWitnessTx tx

instance Binary Tx where
  put = serialize
  get = deserialize

instance Serialize Tx where
  put = serialize
  get = deserialize

putInOut :: (MonadPut m) => Tx -> m ()
putInOut tx = do
  putVarInt $ length tx.inputs
  mapM_ serialize tx.inputs
  putVarInt $ length tx.outputs
  mapM_ serialize tx.outputs

-- | Non-SegWit transaction serializer.
putLegacyTx :: (MonadPut m) => Tx -> m ()
putLegacyTx tx = do
  putWord32le tx.version
  putInOut tx
  putWord32le tx.locktime

-- | Witness transaciton serializer.
putWitnessTx :: (MonadPut m) => Tx -> m ()
putWitnessTx tx = do
  putWord32le tx.version
  putWord8 0x00
  putWord8 0x01
  putInOut tx
  putWitnessData tx.witness
  putWord32le tx.locktime

isWitnessTx :: (MonadGet m) => m Bool
isWitnessTx = lookAhead $ do
  _ <- getWord32le
  m <- getWord8
  f <- getWord8
  return (m == 0x00 && f == 0x01)

-- | Non-SegWit transaction deseralizer.
parseLegacyTx :: (MonadGet m) => m Tx
parseLegacyTx = do
  version <- getWord32le
  inputs <- rl =<< deserialize
  outputs <- rl =<< deserialize
  when (length inputs == 0x00 && length outputs == 0x01) $
    fail "Witness transaction"
  locktime <- getWord32le
  return Tx {witness = [], ..}
  where
    rl (VarInt c) = replicateM (fromIntegral c) deserialize

-- | Witness transaction deserializer.
parseWitnessTx :: (MonadGet m) => m Tx
parseWitnessTx = do
  version <- getWord32le
  m <- getWord8
  f <- getWord8
  unless (m == 0x00 && f == 0x01) $ fail "Not a witness transaction"
  inputs <- replicateList =<< deserialize
  outputs <- replicateList =<< deserialize
  witness <- parseWitnessData $ length inputs
  locktime <- getWord32le
  return Tx {..}
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) deserialize

-- | Witness data deserializer. Requires count of inputs.
parseWitnessData :: (MonadGet m) => Int -> m WitnessData
parseWitnessData n = replicateM n parseWitnessStack
  where
    parseWitnessStack = do
      VarInt i <- deserialize
      replicateM (fromIntegral i) parseWitnessStackItem
    parseWitnessStackItem = do
      VarInt i <- deserialize
      getByteString $ fromIntegral i

-- | Witness data serializer.
putWitnessData :: (MonadPut m) => WitnessData -> m ()
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
      [ "version" .= v,
        "inputs" .= i,
        "outputs" .= o,
        "witnessdata" .= fmap (fmap encodeHex) w,
        "locktime" .= l
      ]
  toEncoding (Tx v i o w l) =
    pairs $
      mconcat
        [ "version" `E.pair` E.word32 v,
          "inputs" `E.pair` E.list toEncoding i,
          "outputs" `E.pair` E.list toEncoding o,
          "witnessdata" `E.pair` E.list (E.list f) w,
          "locktime" `E.pair` E.word32 l
        ]
    where
      f = hexEncoding . BL.fromStrict

-- | Data type representing a transaction input.
data TxIn = TxIn
  { -- | output being spent
    outpoint :: !OutPoint,
    -- | signatures and redeem script
    script :: !ByteString,
    -- | lock-time using sequence numbers (BIP-68)
    sequence :: !Word32
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
      [ "prevoutput" .= o,
        "inputscript" .= encodeHex s,
        "sequence" .= q
      ]
  toEncoding (TxIn o s q) =
    pairs $
      mconcat
        [ "prevoutput" `E.pair` toEncoding o,
          "inputscript" `E.pair` hexEncoding (BL.fromStrict s),
          "sequence" `E.pair` E.word32 q
        ]

-- | Data type representing a transaction output.
data TxOut = TxOut
  { -- | value of output is satoshi
    value :: !Word64,
    -- | pubkey script
    script :: !ByteString
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
    withObject "TxOut" $ \o -> do
      value <- o .: "value"
      t <- o .: "outputscript"
      script <- maybe mzero return (decodeHex t)
      return TxOut {..}

instance ToJSON TxOut where
  toJSON (TxOut o s) =
    object ["value" .= o, "outputscript" .= encodeHex s]
  toEncoding (TxOut o s) =
    pairs $
      mconcat
        [ "value" `E.pair` E.word64 o,
          "outputscript" `E.pair` hexEncoding (BL.fromStrict s)
        ]

-- | The 'OutPoint' refers to a transaction output being spent.
data OutPoint = OutPoint
  { -- | hash of previous transaction
    hash :: !TxHash,
    -- | position of output in previous transaction
    index :: !Word32
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
  toEncoding (OutPoint h i) =
    pairs $
      mconcat
        [ "txid" `E.pair` toEncoding h,
          "index" `E.pair` E.word32 i
        ]

-- | Outpoint used in coinbase transactions.
nullOutPoint :: OutPoint
nullOutPoint =
  OutPoint
    { hash = "0000000000000000000000000000000000000000000000000000000000000000",
      index = maxBound
    }
