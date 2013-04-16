module Bitcoin.Type.Tx 
( Tx(..) 
, TxIn(..)
, TxOut(..)
, OutPoint(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad
import Control.Applicative

import qualified Data.ByteString as BS

import Bitcoin.Type.Hash
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

data Tx = Tx {
    txVersion  :: Word32,
    txIn       :: [TxIn],
    txOut      :: [TxOut],
    txLockTime :: Word32
} deriving (Read, Show)

instance Bitcoin.Type Tx where
    get = Tx <$> Bitcoin.getWord32
             <*> (readList =<< Bitcoin.get)
             <*> (readList =<< Bitcoin.get)
             <*> Bitcoin.getWord32
        where readList (VarInt len) = replicateM (fromIntegral len) Bitcoin.get

    put (Tx v is os l) = do
        Bitcoin.putWord32 v
        Bitcoin.put $ (VarInt . fromIntegral . length) is
        forM_ is Bitcoin.put
        Bitcoin.put $ (VarInt . fromIntegral . length) os
        forM_ os Bitcoin.put
        Bitcoin.putWord32 l

data TxIn = TxIn {
    prevOutput     :: OutPoint,
    sigScript      :: BS.ByteString,
    txInSequence   :: Word32
} deriving (Read, Show)

instance Bitcoin.Type TxIn where
    get = TxIn <$> Bitcoin.get
               <*> (readBS =<< Bitcoin.get)
               <*> Bitcoin.getWord32
        where readBS (VarInt len) = Bitcoin.getByteString (fromIntegral len)

    put (TxIn o bs s) = do
        Bitcoin.put o
        Bitcoin.put $ (VarInt . fromIntegral . BS.length) bs
        Bitcoin.putByteString bs 
        Bitcoin.putWord32 s

data TxOut = TxOut {
    outValue     :: Word64,
    scriptPubKey :: BS.ByteString
} deriving (Read, Show)

instance Bitcoin.Type TxOut where
    get = TxOut <$> Bitcoin.getWord64
                <*> (readBS =<< Bitcoin.get)
        where readBS (VarInt len) = Bitcoin.getByteString (fromIntegral len)

    put (TxOut o bs) = do
        Bitcoin.putWord64 o
        Bitcoin.put $ (VarInt . fromIntegral . BS.length) bs
        Bitcoin.putByteString bs

data OutPoint = OutPoint {
    outPointHash  :: Hash,
    outPointIndex :: Word32
} deriving (Read, Show)

instance Bitcoin.Type OutPoint where
    get = OutPoint <$> Bitcoin.get
                   <*> Bitcoin.getWord32

    put (OutPoint h i) = do
        Bitcoin.put       h
        Bitcoin.putWord32 i


