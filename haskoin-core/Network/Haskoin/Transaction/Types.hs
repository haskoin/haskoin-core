module Network.Haskoin.Transaction.Types
( Tx
, createTx
, txVersion
, txIn
, txOut
, txLockTime
, txHash
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, TxHash(..)
, hexToTxHash
, txHashToHex
, nosigTxHash
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2, replicateM, forM_, mzero, (<=<))

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Word (Word32, Word64)
import Data.Serialize (Serialize, get, put, encode)
import Data.Serialize.Get
    ( getWord32le
    , getWord64le
    , getByteString
    , remaining
    , lookAhead
    )
import Data.Serialize.Put
    ( putWord32le
    , putWord64le
    , putByteString
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( length
    , empty
    , reverse
    )
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Text.Read (readPrec, parens, lexP, pfail)
import qualified Text.Read as Read (Lexeme(Ident, String))
import Network.Haskoin.Util
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Node.Types

newtype TxHash = TxHash { getTxHash :: Hash256 }
    deriving (Eq, Ord)

instance NFData TxHash where
    rnf  = rnf . getHash256 . getTxHash

instance Read TxHash where
    readPrec = parens $ do
        Read.Ident "TxHash" <- lexP
        Read.String str <- lexP
        maybe pfail return $ hexToTxHash $ cs str

instance Show TxHash where
    showsPrec d h = showParen (d > 10) $
        showString "TxHash " . shows (txHashToHex h)

instance IsString TxHash where
    fromString =
        TxHash . fromMaybe e . bsToHash256
               . BS.reverse . fromMaybe e' . decodeHex . cs
      where
        e = error "Could not read transaction hash from decoded hex string"
        e' = error "Colud not decode hex string with transaction hash"

instance Serialize TxHash where
    get = TxHash <$> get
    put = put . getTxHash

nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash $ doubleHash256 $ encode tx{ _txIn = map clearInput $ txIn tx }
  where
    clearInput ti = ti{ scriptInput = BS.empty }

txHashToHex :: TxHash -> ByteString
txHashToHex (TxHash h) = encodeHex $ BS.reverse $ getHash256 h

hexToTxHash :: ByteString -> Maybe TxHash
hexToTxHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- bsToHash256 bs
    return $ TxHash h

instance FromJSON TxHash where
    parseJSON = withText "Transaction id" $ \t ->
        maybe mzero return $ hexToTxHash $ cs t

instance ToJSON TxHash where
    toJSON = String . cs . txHashToHex

-- | Data type representing a bitcoin transaction
data Tx = Tx
    { -- | Transaction data format version
      _txVersion  :: !Word32
      -- | List of transaction inputs
    , _txIn       :: ![TxIn]
      -- | List of transaction outputs
    , _txOut      :: ![TxOut]
      -- | The block number of timestamp at which this transaction is locked
    , _txLockTime :: !Word32
     -- | Hash of the transaction
    , _txHash     :: !TxHash
    } deriving (Eq)

txVersion :: Tx -> Word32
txVersion = _txVersion

txIn :: Tx -> [TxIn]
txIn = _txIn

txOut :: Tx -> [TxOut]
txOut = _txOut

txLockTime :: Tx -> Word32
txLockTime = _txLockTime

txHash :: Tx -> TxHash
txHash = _txHash

createTx :: Word32 -> [TxIn] -> [TxOut] -> Word32 -> Tx
createTx v is os l =
    Tx { _txVersion  = v
       , _txIn       = is
       , _txOut      = os
       , _txLockTime = l
       , _txHash     = TxHash $ doubleHash256 $ encode tx
       }
  where
    tx = Tx { _txVersion  = v
            , _txIn       = is
            , _txOut      = os
            , _txLockTime = l
            , _txHash     = fromString $ replicate 64 '0'
            }

instance Show Tx where
    showsPrec d tx = showParen (d > 10) $
        showString "Tx " . shows (encodeHex $ encode tx)

instance Read Tx where
    readPrec = parens $ do
        Read.Ident "Tx" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe =<< decodeHex (cs str)

instance IsString Tx where
    fromString =
        fromMaybe e . (decodeToMaybe <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"

instance NFData Tx where
    rnf (Tx v i o l t) = rnf v `seq` rnf i `seq` rnf o `seq` rnf l `seq` rnf t

instance Serialize Tx where
    get = do
        start <- remaining
        (v, is, os, l, end) <- lookAhead $ do
            v  <- getWord32le
            is <- replicateList =<< get
            os <- replicateList =<< get
            l  <- getWord32le
            end <- remaining
            return (v, is, os, l, end)
        bs <- getByteString $ fromIntegral $ start - end
        return $ Tx { _txVersion  = v
                    , _txIn       = is
                    , _txOut      = os
                    , _txLockTime = l
                    , _txHash     = TxHash $ doubleHash256 bs
                    }
      where
        replicateList (VarInt c) = replicateM (fromIntegral c) get

    put (Tx v is os l _) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length is
        forM_ is put
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le l

instance FromJSON Tx where
    parseJSON = withText "Tx" $
        maybe mzero return . (decodeToMaybe <=< decodeHex) . cs

instance ToJSON Tx where
    toJSON = String . cs . encodeHex . encode

-- | Data type representing a transaction input.
data TxIn =
    TxIn {
           -- | Reference the previous transaction output (hash + position)
           prevOutput   :: !OutPoint
           -- | Script providing the requirements of the previous transaction
           -- output to spend those coins.
         , scriptInput  :: !ByteString
           -- | Transaction version as defined by the sender of the
           -- transaction. The intended use is for replacing transactions with
           -- new information before the transaction is included in a block.
         , txInSequence :: !Word32
         } deriving (Eq, Show, Read)

instance NFData TxIn where
    rnf (TxIn p i s) = rnf p `seq` rnf i `seq` rnf s

instance Serialize TxIn where
    get =
        TxIn <$> get <*> (readBS =<< get) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len

    put (TxIn o s q) = do
        put o
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s
        putWord32le q

-- | Data type representing a transaction output.
data TxOut =
    TxOut {
            -- | Transaction output value.
            outValue     :: !Word64
            -- | Script specifying the conditions to spend this output.
          , scriptOutput :: !ByteString
          } deriving (Eq, Show, Read)

instance NFData TxOut where
    rnf (TxOut v o) = rnf v `seq` rnf o

instance Serialize TxOut where
    get = do
        val <- getWord64le
        (VarInt len) <- get
        TxOut val <$> (getByteString $ fromIntegral len)

    put (TxOut o s) = do
        putWord64le o
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s

-- | The OutPoint is used inside a transaction input to reference the previous
-- transaction output that it is spending.
data OutPoint = OutPoint
    { -- | The hash of the referenced transaction.
      outPointHash  :: !TxHash
      -- | The position of the specific output in the transaction.
      -- The first output position is 0.
    , outPointIndex :: !Word32
    } deriving (Read, Show, Eq)

instance NFData OutPoint where
    rnf (OutPoint h i) = rnf h `seq` rnf i

instance FromJSON OutPoint where
    parseJSON = withText "OutPoint" $
        maybe mzero return . (decodeToMaybe <=< decodeHex) . cs

instance ToJSON OutPoint where
    toJSON = String . cs . encodeHex . encode

instance Serialize OutPoint where
    get = do
        (h,i) <- liftM2 (,) get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

