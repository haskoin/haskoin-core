module Bitcoin.Protocol.Tx 
( Tx(..) 
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, txCurrentVersion
) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.Script

import qualified Data.ByteString as BS

txCurrentVersion :: Word32
txCurrentVersion = 1

data Tx = Tx {
    txVersion  :: Word32,
    txIn       :: [TxIn],
    txOut      :: [TxOut],
    txLockTime :: Word32
} deriving (Eq, Read, Show)

instance BitcoinProtocol Tx where

    bitcoinGet = Tx <$> getWord32le
                    <*> (replicateList =<< bitcoinGet)
                    <*> (replicateList =<< bitcoinGet)
                    <*> getWord32le
        where replicateList (VarInt c) = replicateM (fromIntegral c) bitcoinGet

    bitcoinPut (Tx v is os l) = do
        putWord32le v
        bitcoinPut $ lengthFromList is
        forM_ is bitcoinPut
        bitcoinPut $ lengthFromList os
        forM_ os bitcoinPut
        putWord32le l

data TxIn = TxIn {
    prevOutput   :: OutPoint,
    sigScript    :: Script,
    txInSequence :: Word32
} deriving (Eq, Read, Show)

instance BitcoinProtocol TxIn where

    bitcoinGet = TxIn <$> bitcoinGet
                      <*> bitcoinGet
                      <*> getWord32le

    bitcoinPut (TxIn o s seq) = do
        bitcoinPut  o
        bitcoinPut  s
        putWord32le seq

data TxOut = TxOut {
    outValue     :: Word64,
    scriptPubKey :: Script
} deriving (Eq, Read, Show)

instance BitcoinProtocol TxOut where
    bitcoinGet = TxOut <$> getWord64le <*> bitcoinGet
    bitcoinPut (TxOut o s) = putWord64le o >> bitcoinPut s

data OutPoint = OutPoint {
    outPointHash  :: Word256,
    outPointIndex :: Word32
} deriving (Eq, Read, Show)

instance BitcoinProtocol OutPoint where
    bitcoinGet = OutPoint <$> getWord256be <*> getWord32le
    bitcoinPut (OutPoint h i) = putWord256be h >> putWord32le i

