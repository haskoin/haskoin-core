module Bitcoin.Type.VarInt ( VarInt(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative

newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Ord, Bounded, Show, Read)
    
instance Binary VarInt where
    get = VarInt <$> ( go =<< getWord8 )
        where go 0xff = fromIntegral <$> getWord64le
              go 0xfe = fromIntegral <$> getWord32le
              go 0xfd = fromIntegral <$> getWord16le
              go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0 = error "VarInt cannot be negative"
        | x < 0xfd = putWord8 $ fromIntegral x
        | x <= 0xffff = do
            putWord8 0xfd 
            putWord16le $ fromIntegral x
        | x <= 0xffffffff = do
            putWord8 0xfe 
            putWord32le $ fromIntegral x
        | x <= 0xffffffffffffffff = do
            putWord8 0xff 
            putWord64le $ fromIntegral x
        | otherwise = error "VarInt cannot be larger that 8 bytes"

