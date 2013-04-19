module Bitcoin.Protocol.VarInt 
( VarInt(..)
, lengthFromList
, lengthFromBS
) where

import Control.Applicative
import Bitcoin.Protocol

import qualified Data.ByteString as BS

newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Ord, Bounded, Show, Read)
    
instance BitcoinProtocol VarInt where

    bitcoinGet = VarInt <$> ( getWord8 >>= go )
        where go 0xff = getWord64le
              go 0xfe = fromIntegral <$> getWord32le
              go 0xfd = fromIntegral <$> getWord16le
              go x    = fromIntegral <$> return x

    bitcoinPut (VarInt x)
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

lengthFromList :: [a] -> VarInt
lengthFromList = VarInt . fromIntegral . length

lengthFromBS :: BS.ByteString -> VarInt
lengthFromBS = VarInt . fromIntegral . BS.length

