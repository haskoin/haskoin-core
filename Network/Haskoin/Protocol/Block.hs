module Network.Haskoin.Protocol.Block (Block(..)) where

import Control.Monad (replicateM, forM_)

import Data.Binary (Binary, get, put)

import Network.Haskoin.Protocol.Tx
import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.BlockHeader

-- | Data type describing a block in the bitcoin protocol. Blocks are sent in
-- response to 'GetData' messages that are requesting information from a
-- block hash.
data Block = 
    Block {
            -- | Header information for this block.
            blockHeader     :: !BlockHeader
            -- | Coinbase transaction of this block.
          , blockCoinbaseTx :: !CoinbaseTx
            -- | List of transactions pertaining to this block.
          , blockTxns       :: ![Tx]
          } deriving (Eq, Show)

instance Binary Block where

    get = do
        header     <- get
        (VarInt c) <- get
        cb         <- get
        txs        <- replicateM (fromIntegral (c-1)) get
        return $ Block header cb txs

    put (Block h cb txs) = do
        put h
        put $ VarInt $ fromIntegral $ (length txs) + 1
        put cb
        forM_ txs put

