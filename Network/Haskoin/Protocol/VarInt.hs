module Network.Haskoin.Protocol.VarInt ( VarInt(..) ) where
 
import Data.Word (Word64)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord8
    , getWord16le
    , getWord32le
    , getWord64le
    )
import Data.Binary.Put 
    ( putWord8
    , putWord16le
    , putWord32le
    , putWord64le
    )

-- | Data type representing a variable length integer. The 'VarInt' type
-- usually precedes an array or a string that can vary in length. 
newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Show)
    
instance Binary VarInt where

    get = VarInt <$> ( getWord8 >>= go )
      where 
        go 0xff = getWord64le
        go 0xfe = fromIntegral <$> getWord32le
        go 0xfd = fromIntegral <$> getWord16le
        go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0xfd = 
            putWord8 $ fromIntegral x
        | x <= 0xffff = do
            putWord8 0xfd
            putWord16le $ fromIntegral x
        | x <= 0xffffffff = do
            putWord8 0xfe
            putWord32le $ fromIntegral x
        | otherwise = do
            putWord8 0xff
            putWord64le x

