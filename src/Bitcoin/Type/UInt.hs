module Bitcoin.Type.UInt
( UInt8(..)
, UInt16(..)
, UInt32(..)
, UInt64(..)
) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

newtype UInt8 = UInt8 Word8
    deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Show, Read)

newtype UInt16 = UInt16 Word16
    deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Show, Read)

newtype UInt32 = UInt32 Word32
    deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Show, Read)

newtype UInt64 = UInt64 Word64
    deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded, Show, Read)

instance Binary UInt8 where
    get = getWord8 >>= (\x -> return $ UInt8 x)
    put (UInt8 x) = putWord8 x

instance Binary UInt16 where
    get = getWord16le >>= (\x -> return $ UInt16 x)
    put (UInt16 x) = putWord16le x

instance Binary UInt32 where
    get = getWord32le >>= (\x -> return $ UInt32 x)
    put (UInt32 x) = putWord32le x

instance Binary UInt64 where
    get = getWord64le >>= (\x -> return $ UInt64 x)
    put (UInt64 x) = putWord64le x

