module Bitcoin.Protocol.Block 
( Block(..)
, blockHash
) where

import Control.Monad
import Control.Applicative

import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.BlockHeader
import Bitcoin.Protocol.Script

import qualified Data.ByteString as BS

data Block = Block {
    blockHeader     :: !BlockHeader,
    blockCoinbaseTx :: !CoinbaseTx,
    blockTxns       :: ![Tx]
} deriving (Eq, Read, Show)

instance BitcoinProtocol Block where

    bitcoinGet = do
        head       <- bitcoinGet
        (VarInt c) <- bitcoinGet
        cb         <- bitcoinGet
        txs        <- replicateM (fromIntegral (c-1)) bitcoinGet
        return $ Block head cb txs

    bitcoinPut (Block h cb txs) = do
        bitcoinPut h
        bitcoinPut $ VarInt $ fromIntegral $ (length txs) + 1
        bitcoinPut cb
        forM_ txs bitcoinPut

blockHash :: Block -> Word256
blockHash = blockHeaderHash . blockHeader


