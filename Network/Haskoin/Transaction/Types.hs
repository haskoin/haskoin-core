module Network.Haskoin.Transaction.Types
( Tx(..)
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, CoinbaseTx(..)
, TxHash(..)
, txHash
, hexToTxHash
, txHashToHex
, nosigTxHash
, cbHash
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2, replicateM, forM_, unless, mzero, (<=<))

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Word (Word32, Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get
    ( getWord32le
    , getWord64le
    , getByteString
    )
import Data.Binary.Put
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
    fromString = TxHash . fromMaybe e . bsToHash256
               . BS.reverse . fromMaybe e' . decodeHex . cs
      where
        e = error "Could not read transaction hash from decoded hex string"
        e' = error "Colud not decode hex string with transaction hash"

instance Binary TxHash where
    get = TxHash <$> get
    put = put . getTxHash

-- | Computes the hash of a transaction.
txHash :: Tx -> TxHash
txHash = TxHash . doubleHash256 . encode'

nosigTxHash :: Tx -> TxHash
nosigTxHash tx = txHash tx{ txIn = map clearInput $ txIn tx } where
    clearInput ti = ti{ scriptInput = BS.empty }

-- | Computes the hash of a coinbase transaction.
cbHash :: CoinbaseTx -> TxHash
cbHash = TxHash . doubleHash256 . encode'

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
    toJSON h = String $ cs $ txHashToHex h

-- | Data type representing a bitcoin transaction
data Tx =
    Tx { -- | Transaction data format version
         txVersion  :: !Word32
         -- | List of transaction inputs
       , txIn       :: ![TxIn]
         -- | List of transaction outputs
       , txOut      :: ![TxOut]
         -- | The block number of timestamp at which this transaction is locked
       , txLockTime :: !Word32
       } deriving (Eq)

instance Show Tx where
    showsPrec d tx = showParen (d > 10) $
        showString "Tx " . shows (encodeHex $ encode' tx)

instance Read Tx where
    readPrec = parens $ do
        Read.Ident "Tx" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe =<< decodeHex (cs str)

instance IsString Tx where
    fromString = fromMaybe e . (decodeToMaybe <=< decodeHex) . cs where
        e = error "Could not read transaction from hex string"

instance NFData Tx where
    rnf (Tx v i o l) = rnf v `seq` rnf i `seq` rnf o `seq` rnf l

instance Binary Tx where
    get = Tx <$> getWord32le
             <*> (replicateList =<< get)
             <*> (replicateList =<< get)
             <*> getWord32le
      where
        replicateList (VarInt c) = replicateM (fromIntegral c) get

    put (Tx v is os l) = do
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
    toJSON = String . cs . encodeHex . encode'

-- | Data type representing the coinbase transaction of a 'Block'. Coinbase
-- transactions are special types of transactions which are created by miners
-- when they find a new block. Coinbase transactions have no inputs. They have
-- outputs sending the newly generated bitcoins together with all the block's
-- fees to a bitcoin address (usually the miners address). Data can be embedded
-- in a Coinbase transaction which can be chosen by the miner of a block. This
-- data also typically contains some randomness which is used, together with
-- the nonce, to find a partial hash collision on the block's hash.
data CoinbaseTx =
    CoinbaseTx {
                 -- | Transaction data format version.
                 cbVersion    :: !Word32
                 -- | Previous outpoint. This is ignored for
                 -- coinbase transactions but preserved for computing
                 -- the correct txid.
               , cbPrevOutput :: !OutPoint
                 -- | Data embedded inside the coinbase transaction.
               , cbData       :: !ByteString
                 -- | Transaction sequence number. This is ignored for
                 -- coinbase transactions but preserved for computing
                 -- the correct txid.
               , cbInSequence :: !Word32
                 -- | List of transaction outputs.
               , cbOut        :: ![TxOut]
                 -- | The block number of timestamp at which this
                 -- transaction is locked.
               , cbLockTime   :: !Word32
               } deriving (Eq, Show, Read)

instance NFData CoinbaseTx where
    rnf (CoinbaseTx v p d i o l) =
        rnf v `seq` rnf p `seq` rnf d `seq` rnf i `seq` rnf o `seq` rnf l

instance Binary CoinbaseTx where

    get = do
        v <- getWord32le
        (VarInt len) <- get
        unless (len == 1) $ fail "CoinbaseTx get: Input size is not 1"
        op <- get
        (VarInt cbLen) <- get
        cb <- getByteString (fromIntegral cbLen)
        sq <- getWord32le
        (VarInt oLen) <- get
        os <- replicateM (fromIntegral oLen) get
        lt <- getWord32le
        return $ CoinbaseTx v op cb sq os lt

    put (CoinbaseTx v op cb sq os lt) = do
        putWord32le v
        put $ VarInt 1
        put op
        put $ VarInt $ fromIntegral $ BS.length cb
        putByteString cb
        putWord32le sq
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le lt

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

instance Binary TxIn where

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

instance Binary TxOut where

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
data OutPoint =
    OutPoint {
               -- | The hash of the referenced transaction.
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
    toJSON = String . cs . encodeHex . encode'

instance Binary OutPoint where
    get = do
        (h,i) <- liftM2 (,) get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

