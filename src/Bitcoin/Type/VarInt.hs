module Bitcoin.Type.VarInt ( VarInt(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Bitcoin.Type.UInt as B

newtype VarInt = VarInt B.UInt64
    deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Show, Read)

instance Binary VarInt where

    get = do
        tag <- (get :: Get B.UInt8)
        case tag of
            0xff -> (get :: Get B.UInt64) >>= result
            0xfe -> (get :: Get B.UInt32) >>= result
            0xfd -> (get :: Get B.UInt16) >>= result
            _    -> result tag
        where
            result :: Integral a => a -> Get VarInt
            result = return . VarInt . fromIntegral

    put (VarInt w)
        | w < 0xfd = put . B.UInt8 . fromIntegral $ w
        | w <= 0xffff = do
            (put . B.UInt8) 0xfd
            (put . B.UInt16 . fromIntegral) w
        | w <= 0xffffff = do
            (put . B.UInt8) 0xfe
            (put . B.UInt32 . fromIntegral) w
        | otherwise = do
            (put . B.UInt8) 0xff
            (put . B.UInt64 . fromIntegral) w


