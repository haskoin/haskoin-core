module Bitcoin.Type.VarInt ( VarInt(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Ord, Bounded, Show, Read)
    
instance Bitcoin.Type VarInt where
    get = VarInt <$> ( getWord8 >>= go )
        where go 0xff = Bitcoin.getWord64 
              go 0xfe = fromIntegral <$> Bitcoin.getWord32
              go 0xfd = fromIntegral <$> Bitcoin.getWord16
              go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0xfd = 
            Bitcoin.putWord8 $ fromIntegral x
        | x <= 0xffff = do
            Bitcoin.putWord8 0xfd
            Bitcoin.putWord16 $ fromIntegral x
        | x <= 0xffffffff = do
            Bitcoin.putWord8 0xfe
            Bitcoin.putWord32 $ fromIntegral x
        | otherwise = do
            Bitcoin.putWord8 0xff
            Bitcoin.putWord64 x

