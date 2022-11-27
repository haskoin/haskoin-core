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

import Bitcoin.Crypto.Hash (Hash256, doubleSHA256, doubleSHA256L)
import Bitcoin.Network.Common (VarInt (VarInt), putVarInt)
import Bitcoin.Util (
    decodeHex,
    eitherToMaybe,
    encodeHex,
    hexBuilder,
 )
import qualified Bitcoin.Util as U
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
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
import Data.Binary (Binary (..), Get, Put)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (char7)
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import qualified Text.Read as R


-- | Transaction id: hash of transaction excluding witness data.
newtype TxHash = TxHash {getTxHash :: Hash256}
    deriving (Eq, Ord, Generic, Hashable, Binary, NFData)


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


-- | Transaction hash excluding signatures.
nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash
        . doubleSHA256L
        $ Bin.encode
            tx{txIn = map clearInput $ txIn tx}
  where
    clearInput ti = ti{scriptInput = BS.empty}


-- | Convert transaction hash to hex form, reversing bytes.
txHashToHex :: TxHash -> Text
txHashToHex (TxHash h) = encodeHex . BS.reverse $ U.encodeS h


-- | Convert transaction hash from hex, reversing bytes.
hexToTxHash :: Text -> Maybe TxHash
hexToTxHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- either (const Nothing) Just . U.decode $ BSL.fromStrict bs
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
txHash tx = TxHash . doubleSHA256L $ Bin.encode tx{txWitness = []}


instance IsString Tx where
    fromString =
        fromMaybe e . (eitherToMaybe . U.decode . BSL.fromStrict <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"


instance Binary Tx where
    get =
        isWitnessTx >>= \w -> if w then parseWitnessTx else parseLegacyTx
    put tx
        | null (txWitness tx) = putLegacyTx tx
        | otherwise = putWitnessTx tx


putInOut :: Tx -> Put
putInOut tx = do
    putVarInt $ length (txIn tx)
    forM_ (txIn tx) put
    putVarInt $ length (txOut tx)
    forM_ (txOut tx) put


-- | Non-SegWit transaction serialization.
putLegacyTx :: Tx -> Put
putLegacyTx tx = do
    Put.putWord32le (txVersion tx)
    putInOut tx
    Put.putWord32le (txLockTime tx)


-- | Witness transaciton serialization.
putWitnessTx :: Tx -> Put
putWitnessTx tx = do
    Put.putWord32le (txVersion tx)
    Put.putWord8 0x00
    Put.putWord8 0x01
    putInOut tx
    putWitnessData (txWitness tx)
    Put.putWord32le (txLockTime tx)


isWitnessTx :: Get Bool
isWitnessTx = Get.lookAhead $ do
    _ <- Get.getWord32le
    m <- Get.getWord8
    f <- Get.getWord8
    return (m == 0x00 && f == 0x01)


-- | Non-SegWit transaction deseralizer.
parseLegacyTx :: Get Tx
parseLegacyTx = do
    v <- Get.getWord32le
    is <- replicateList =<< get
    os <- replicateList =<< get
    when (length is == 0x00 && length os == 0x01) $ fail "Witness transaction"
    l <- Get.getWord32le
    return
        Tx
            { txVersion = v
            , txIn = is
            , txOut = os
            , txWitness = []
            , txLockTime = l
            }
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) get


-- | Witness transaction getr.
parseWitnessTx :: Get Tx
parseWitnessTx = do
    v <- Get.getWord32le
    m <- Get.getWord8
    f <- Get.getWord8
    unless (m == 0x00 && f == 0x01) $ fail "Not a witness transaction"
    is <- replicateList =<< get
    os <- replicateList =<< get
    w <- parseWitnessData $ length is
    l <- Get.getWord32le
    return
        Tx{txVersion = v, txIn = is, txOut = os, txWitness = w, txLockTime = l}
  where
    replicateList (VarInt c) = replicateM (fromIntegral c) get


-- | Witness data getr. Requires count of inputs.
parseWitnessData :: Int -> Get WitnessData
parseWitnessData n = replicateM n parseWitnessStack
  where
    parseWitnessStack = do
        VarInt i <- get
        replicateM (fromIntegral i) parseWitnessStackItem
    parseWitnessStackItem = do
        VarInt i <- get
        Get.getByteString $ fromIntegral i


-- | Witness data serialization.
putWitnessData :: WitnessData -> Put
putWitnessData = mapM_ putWitnessStack
  where
    putWitnessStack ws = do
        putVarInt $ length ws
        mapM_ putWitnessStackItem ws
    putWitnessStackItem bs = do
        putVarInt $ BS.length bs
        Put.putByteString bs


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


instance Binary TxIn where
    get =
        TxIn <$> get <*> (readBS =<< get) <*> Get.getWord32le
      where
        readBS (VarInt len) = Get.getByteString $ fromIntegral len


    put (TxIn o s q) = do
        put o
        putVarInt $ BS.length s
        Put.putByteString s
        Put.putWord32le q


-- | Data type representing a transaction output.
data TxOut = TxOut
    { outValue :: !Word64
    -- ^ value of output is satoshi
    , scriptOutput :: !ByteString
    -- ^ pubkey script
    }
    deriving (Eq, Show, Read, Ord, Generic, Hashable, NFData)


instance Binary TxOut where
    get = do
        val <- Get.getWord64le
        VarInt len <- get
        TxOut val <$> Get.getByteString (fromIntegral len)


    put (TxOut o s) = do
        Put.putWord64le o
        putVarInt $ BS.length s
        Put.putByteString s


-- | The 'OutPoint' refers to a transaction output being spent.
data OutPoint = OutPoint
    { outPointHash :: !TxHash
    -- ^ hash of previous transaction
    , outPointIndex :: !Word32
    -- ^ position of output in previous transaction
    }
    deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)


instance Binary OutPoint where
    get = do
        (h, i) <- liftM2 (,) get Get.getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> Put.putWord32le i


-- | Outpoint used in coinbase transactions.
nullOutPoint :: OutPoint
nullOutPoint =
    OutPoint
        { outPointHash =
            "0000000000000000000000000000000000000000000000000000000000000000"
        , outPointIndex = maxBound
        }
