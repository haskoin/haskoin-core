module Bitcoin.Types 
( VarInt(..) 
, VarString(..)
, NetworkAddress(..) 
, Version(..)
) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative
import qualified Data.ByteString.Lazy as BL

newtype VarInt = VarInt Word64
    deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
    
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

newtype VarString = VarString BL.ByteString
    deriving (Eq, Show, Read)

instance Binary VarString where
    
    get = do
        size <- (get :: Get VarInt)
        VarString <$> getLazyByteString (fromIntegral size)
        
    put (VarString x) = do
        put . VarInt . fromIntegral . BL.length $ x
        putLazyByteString x

data NetworkAddress = NetworkAddress {
    naServices :: Word64,
    naAddress  :: BL.ByteString,
    naPort     :: Word16
} deriving (Eq, Show, Read)

instance Binary NetworkAddress where

    get = do
        services <- getWord64le
        address  <- getLazyByteString 16
        port     <- getWord16be 
        return $ NetworkAddress services address port

    put (NetworkAddress services address port) = do
        putWord64le services
        putLazyByteString address
        putWord16be port

data Version = Version {
    vVersion     :: Word32,
    vServices    :: Word64,
    vTimestamp   :: Word64,
    vAddrRecv    :: NetworkAddress,
    vAddrSend    :: NetworkAddress,
    vNonce       :: Word64,
    vStartHeight :: Word32,
    vRelay       :: Bool
} deriving (Show, Read)

instance Binary Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> getWord32le
                  <*> get

    put (Version v s t ar as n sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        putWord32le sh
        put         r

