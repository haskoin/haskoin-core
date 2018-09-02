{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.Haskoin.Transaction.Common
    ( Tx(..)
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
    , genesisTx
    ) where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData, rnf)
import           Control.Monad                  (forM_, guard, liftM2, mzero,
                                                 replicateM, (<=<))
import           Data.Aeson                     as A
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.Hashable                  (Hashable)
import           Data.Maybe                     (fromMaybe, maybe)
import           Data.Serialize                 as S
import           Data.Serialize.Get             as S
import           Data.Serialize.Put             as S
import           Data.String                    (IsString, fromString)
import           Data.String.Conversions        (cs)
import           Data.Word                      (Word32, Word64)
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Network.Common
import           Network.Haskoin.Script.Common
import           Network.Haskoin.Util
import           Text.Read                      as R

-- | Transaction id: hash of transaction excluding witness data.
newtype TxHash = TxHash { getTxHash :: Hash256 }
    deriving (Eq, Ord, NFData, Hashable, Serialize)

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
    parseJSON = withText "txid" $ \t ->
        maybe mzero return $ hexToTxHash $ cs t

instance ToJSON TxHash where
    toJSON = A.String . cs . txHashToHex

-- | Transaction hash excluding signatures.
nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash $ doubleSHA256 $ S.encode tx { txIn = map clearInput $ txIn tx }
  where
    clearInput ti = ti { scriptInput = B.empty }

-- | Convert transaction hash to hex form, reversing bytes.
txHashToHex :: TxHash -> ByteString
txHashToHex (TxHash h) = encodeHex (B.reverse (S.encode h))

-- | Convert transaction hash from hex, reversing bytes.
hexToTxHash :: ByteString -> Maybe TxHash
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
    } deriving (Eq, Ord)

-- | Compute transaction hash.
txHash :: Tx -> TxHash
txHash tx = TxHash (doubleSHA256 (S.encode tx {txWitness = []}))

instance Show Tx where
    showsPrec _ = shows . encodeHex . S.encode

instance Read Tx where
    readPrec = do
        R.String str <- R.lexP
        maybe R.pfail return $ (eitherToMaybe . S.decode) =<< decodeHex (cs str)

instance IsString Tx where
    fromString =
        fromMaybe e . (eitherToMaybe . S.decode <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"

instance NFData Tx where
    rnf (Tx v i o w l) = rnf v `seq` rnf i `seq` rnf o `seq` rnf w `seq` rnf l

instance Serialize Tx where
    get = parseWitnessTx <|> parseLegacyTx
    put tx
        | null (txWitness tx) = putLegacyTx tx
        | otherwise = putWitnessTx tx

-- | Non-SegWit transaction serializer.
putLegacyTx :: Tx -> Put
putLegacyTx (Tx v is os _ l) = do
    putWord32le v
    put $ VarInt $ fromIntegral $ length is
    forM_ is put
    put $ VarInt $ fromIntegral $ length os
    forM_ os put
    putWord32le l

-- | Witness transaciton serializer.
putWitnessTx :: Tx -> Put
putWitnessTx (Tx v is os w l) = do
    putWord32le v
    putWord8 0x00
    putWord8 0x01
    put $ VarInt $ fromIntegral $ length is
    forM_ is put
    put $ VarInt $ fromIntegral $ length os
    forM_ os put
    putWitnessData w
    putWord32le l

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
        put $ VarInt $ fromIntegral $ length ws
        mapM_ putWitnessStackItem ws
    putWitnessStackItem bs = do
        put $ VarInt $ fromIntegral $ B.length bs
        putByteString bs

instance FromJSON Tx where
    parseJSON = withText "Tx" $
        maybe mzero return . (eitherToMaybe . S.decode <=< decodeHex) . cs

instance ToJSON Tx where
    toJSON = A.String . cs . encodeHex . S.encode

-- | Data type representing a transaction input.
data TxIn =
    TxIn {
           -- | output being spent
           prevOutput   :: !OutPoint
           -- | signatures and redeem script
         , scriptInput  :: !ByteString
           -- | lock-time using sequence numbers (BIP-68)
         , txInSequence :: !Word32
         } deriving (Eq, Show, Ord)

instance NFData TxIn where
    rnf (TxIn p i s) = rnf p `seq` rnf i `seq` rnf s

instance Serialize TxIn where
    get =
        TxIn <$> S.get <*> (readBS =<< S.get) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len

    put (TxIn o s q) = do
        put o
        put $ VarInt $ fromIntegral $ B.length s
        putByteString s
        putWord32le q

-- | Data type representing a transaction output.
data TxOut =
    TxOut {
            -- | value of output is satoshi
            outValue     :: !Word64
            -- | pubkey script
          , scriptOutput :: !ByteString
          } deriving (Eq, Show, Ord)

instance NFData TxOut where
    rnf (TxOut v o) = rnf v `seq` rnf o

instance Serialize TxOut where
    get = do
        val <- getWord64le
        (VarInt len) <- S.get
        TxOut val <$> getByteString (fromIntegral len)

    put (TxOut o s) = do
        putWord64le o
        put $ VarInt $ fromIntegral $ B.length s
        putByteString s

-- | The 'OutPoint' refers to a transaction output being spent.
data OutPoint = OutPoint
    { -- | hash of previous transaction
      outPointHash  :: !TxHash
      -- | position of output in previous transaction
    , outPointIndex :: !Word32
    } deriving (Show, Read, Eq, Ord)

instance NFData OutPoint where
    rnf (OutPoint h i) = rnf h `seq` rnf i

instance FromJSON OutPoint where
    parseJSON = withText "OutPoint" $
        maybe mzero return . (eitherToMaybe . S.decode <=< decodeHex) . cs

instance ToJSON OutPoint where
    toJSON = A.String . cs . encodeHex . S.encode

instance Serialize OutPoint where
    get = do
        (h,i) <- liftM2 (,) S.get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

-- | Outpoint used in coinbase transactions.
nullOutPoint :: OutPoint
nullOutPoint =
    OutPoint
    { outPointHash =
          "0000000000000000000000000000000000000000000000000000000000000000"
    , outPointIndex = maxBound
    }

-- | Transaction from Genesis block.
genesisTx :: Tx
genesisTx =
    Tx version [txin] [txout] [] locktime
  where
    version = 1
    txin = TxIn outpoint inputBS maxBound
    txout = TxOut 5000000000 (encodeOutputBS output)
    locktime = 0
    outpoint = OutPoint z maxBound
    Just inputBS = decodeHex $ fromString $
        "04ffff001d0104455468652054696d65732030332f4a616e2f323030392043686" ++
        "16e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f" ++
        "757420666f722062616e6b73"
    output = PayPK $ fromString $
        "04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb" ++
        "649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"
    z = "0000000000000000000000000000000000000000000000000000000000000000"
