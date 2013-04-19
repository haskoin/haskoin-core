module Bitcoin.Protocol.Tx 
( Tx(..) 
, TxIn(..)
, TxOut(..)
, OutPoint(..)
) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt

import qualified Data.ByteString as BS

data Tx = Tx {
    txVersion  :: Word32,
    txIn       :: [TxIn],
    txOut      :: [TxOut],
    txLockTime :: Word32
} deriving (Read, Show)

instance BitcoinProtocol Tx where

    bitcoinGet = Tx <$> getWord32le
                    <*> (readList =<< bitcoinGet)
                    <*> (readList =<< bitcoinGet)
                    <*> getWord32le
        where readList (VarInt c) = replicateM (fromIntegral c) bitcoinGet

    bitcoinPut (Tx v is os l) = do
        putWord32le v
        bitcoinPut $ lengthFromList is
        forM_ is bitcoinPut
        bitcoinPut $ lengthFromList os
        forM_ os bitcoinPut
        putWord32le l

data TxIn = TxIn {
    prevOutput   :: OutPoint,
    sigScript    :: BS.ByteString,
    txInSequence :: Word32
} deriving (Read, Show)

instance BitcoinProtocol TxIn where

    bitcoinGet = TxIn <$> bitcoinGet
                      <*> (readBS =<< bitcoinGet)
                      <*> getWord32le
        where readBS (VarInt len) = getByteString (fromIntegral len)

    bitcoinPut (TxIn o bs s) = do
        bitcoinPut o
        bitcoinPut $ lengthFromBS bs
        putByteString bs 
        putWord32le s

data TxOut = TxOut {
    outValue     :: Word64,
    scriptPubKey :: BS.ByteString
} deriving (Read, Show)

instance BitcoinProtocol TxOut where

    bitcoinGet = TxOut <$> getWord64le
                       <*> (readBS =<< bitcoinGet)
        where readBS (VarInt len) = getByteString (fromIntegral len)

    bitcoinPut (TxOut o bs) = do
        putWord64le o
        bitcoinPut $ lengthFromBS bs
        putByteString bs

data OutPoint = OutPoint {
    outPointHash  :: Word256,
    outPointIndex :: Word32
} deriving (Read, Show)

instance BitcoinProtocol OutPoint where

    bitcoinGet = OutPoint <$> getWord256be
                          <*> getWord32le

    bitcoinPut (OutPoint h i) = do
        putWord256be h
        putWord32le  i

