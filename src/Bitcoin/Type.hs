module Bitcoin.Type 
( Type(..) 
, hasMore
) where

import Data.Word
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

hasMore :: Get a -> Get a -> Get a
hasMore t f = isEmpty >>= (\e -> if e then f else t)

class Type a where
    get :: Get a
    put :: a -> Put

instance Type Bool where
    get = go =<< getWord8
        where go 0 = return False
              go _ = return True
    put True  = putWord8 1
    put False = putWord8 0

instance Type Word8 where
    get = getWord8
    put = putWord8

instance Type Word16 where
    get = getWord16le
    put = putWord16le

instance Type Word32 where
    get = getWord32le
    put = putWord32le

instance Type Word64 where
    get = getWord64le
    put = putWord64le

