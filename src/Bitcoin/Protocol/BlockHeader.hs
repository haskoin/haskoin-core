module Bitcoin.Protocol.BlockHeader ( BlockHeader(..) ) where

import Bitcoin.Protocol
import Control.Applicative

data BlockHeader = BlockHeader {
    blockVersion   :: Word32,
    prevBlock      :: Word256,
    merkleRoot     :: Word256,
    blockTimestamp :: Word32,
    blockBits      :: Word32,
    nonce          :: Word32
} deriving (Read, Show)

instance BitcoinProtocol BlockHeader where

    bitcoinGet = BlockHeader <$> getWord32le
                             <*> getWord256be
                             <*> getWord256be
                             <*> getWord32le
                             <*> getWord32le
                             <*> getWord32le

    bitcoinPut (BlockHeader v p m bt bb n) = do
        putWord32le  v
        putWord256be p
        putWord256be m
        putWord32le  bt
        putWord32le  bb
        putWord32le  n 

