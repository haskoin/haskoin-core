module Network.Haskoin.Protocol.Tx 
( Tx(..) 
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, CoinbaseTx(..)
, txid
, cbid
, encodeTxid
, decodeTxid
) where

import Control.Monad (replicateM, forM_, liftM2, unless)
import Control.Applicative ((<$>),(<*>))

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
import qualified Data.ByteString as BS 
    ( ByteString
    , length
    , reverse
    )

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.Script
import Network.Haskoin.Crypto.Hash (Hash256, doubleHash256)
import Network.Haskoin.Util 
    ( bsToHex
    , hexToBS
    , encode'
    , decodeToMaybe
    )

-- | Data type representing a bitcoin transaction
data Tx = 
    Tx { 
         -- | Transaction data format version
         txVersion  :: !Word32
         -- | List of transaction inputs
       , txIn       :: ![TxIn]
         -- | List of transaction outputs
       , txOut      :: ![TxOut]
         -- | The block number of timestamp at which this transaction is locked
       , txLockTime :: !Word32
       } deriving (Eq, Show)

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
               , cbData       :: !BS.ByteString
                 -- | Transaction sequence number. This is ignored for
                 -- coinbase transactions but preserved for computing
                 -- the correct txid.
               , cbInSequence :: !Word32
                 -- | List of transaction outputs.
               , cbOut        :: ![TxOut]
                 -- | The block number of timestamp at which this 
                 -- transaction is locked.
               , cbLockTime   :: !Word32
               } deriving (Eq, Show)

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
         , scriptInput  :: !Script
           -- | Transaction version as defined by the sender of the
           -- transaction. The intended use is for replacing transactions with
           -- new information before the transaction is included in a block.
         , txInSequence :: !Word32
         } deriving (Eq, Show)

instance Binary TxIn where
    get = TxIn <$> get <*> get <*> getWord32le
    put (TxIn o s q) = put o >> put s >> putWord32le q

-- | Data type representing a transaction output.
data TxOut = 
    TxOut { 
            -- | Transaction output value.
            outValue     :: !Word64
            -- | Script specifying the conditions to spend this output.
          , scriptOutput :: !Script
          } deriving (Eq, Show)

instance Binary TxOut where
    get = do
        val <- getWord64le
        unless (val <= 2100000000000000) $ fail $
            "Invalid TxOut value: " ++ (show val)
        TxOut val <$> get
    put (TxOut o s) = putWord64le o >> put s

-- | The OutPoint is used inside a transaction input to reference the previous
-- transaction output that it is spending.
data OutPoint = 
    OutPoint { 
               -- | The hash of the referenced transaction.
               outPointHash  :: !Hash256
               -- | The position of the specific output in the transaction.
               -- The first output position is 0.
             , outPointIndex :: !Word32
             } deriving Eq

instance Show OutPoint where
    show (OutPoint h i) = show ("txid = " ++ h',"index = " ++ (show i))
      where 
        h' = encodeTxid h

instance Binary OutPoint where
    get = do
        (h,i) <- liftM2 (,) get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

-- | Computes the hash of a transaction.
txid :: Tx -> Hash256
txid = doubleHash256 . encode' 

-- | Computes the hash of a coinbase transaction.
cbid :: CoinbaseTx -> Hash256
cbid = doubleHash256 . encode' 

-- | Encodes a transaction hash as little endian in HEX format.
-- This is mostly used for displaying transaction ids. Internally, these ids
-- are handled as big endian but are transformed to little endian when
-- displaying them.
encodeTxid :: Hash256 -> String
encodeTxid = bsToHex . BS.reverse .  encode' 

-- | Decodes a little endian transaction hash in HEX format. 
decodeTxid :: String -> Maybe Hash256
decodeTxid = (decodeToMaybe . BS.reverse =<<) . hexToBS

