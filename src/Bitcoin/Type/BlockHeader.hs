module Bitcoin.Type.BlockHeader ( BlockHeader(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import Bitcoin.Type.Hash

import qualified Bitcoin.Type as Bitcoin

data BlockHeader = BlockHeader {
    blockVersion   :: Word32,
    prevBlock      :: Hash,
    merkleRoot     :: Hash,
    blockTimestamp :: Word32,
    blockBits      :: Word32,
    nonce          :: Word32
} deriving (Read, Show)

instance Bitcoin.Type BlockHeader where
    get = BlockHeader <$> Bitcoin.getWord32
                      <*> Bitcoin.getHash
                      <*> Bitcoin.getHash
                      <*> Bitcoin.getWord32
                      <*> Bitcoin.getWord32
                      <*> Bitcoin.getWord32

    put (BlockHeader v p m bt bb n) = do
        Bitcoin.putWord32 v
        Bitcoin.putHash   p
        Bitcoin.putHash   m
        Bitcoin.putWord32 bt
        Bitcoin.putWord32 bb
        Bitcoin.putWord32 n 

