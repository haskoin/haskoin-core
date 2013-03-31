module Bitcoin.Types 
( VarInt(..) 
, VarString(..)
, NetworkAddress(..) 
, Version(..)
, MessageHeader(..)
, encodeMessage
, readMessage
) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Char --for ord

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Crypto.Hash.SHA256 as H

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

newtype VarString = VarString BS.ByteString
    deriving (Eq, Show, Read)

instance Binary VarString where
    
    get = do
        (VarInt size) <- (get :: Get VarInt)
        VarString <$> getByteString (fromIntegral size)
        
    put (VarString x) = do
        put . VarInt . fromIntegral . BS.length $ x
        putByteString x

data NetworkAddress = NetworkAddress {
    naServices :: Word64,
    naAddress  :: BS.ByteString,
    naPort     :: Word16
} deriving (Eq, Show, Read)

instance Binary NetworkAddress where

    get = NetworkAddress <$> getWord64le
                         <*> getByteString 16
                         <*> getWord16be 

    put (NetworkAddress s a p) = do
        putWord64le       s
        putByteString     a
        putWord16be       p

data Version = Version {
    vVersion     :: Word32,
    vServices    :: Word64,
    vTimestamp   :: Word64,
    vAddrRecv    :: NetworkAddress,
    vAddrSend    :: NetworkAddress,
    vNonce       :: Word64,
    vUserAgent   :: VarString,
    vStartHeight :: Word32,
    vRelay       :: Bool
} deriving (Show, Read)

class Binary a => ProtocolMessage a where
    command :: a -> String

instance Binary Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> get
                  <*> getWord32le
                  <*> get

    put (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        put         ua
        putWord32le sh
        put         r

instance ProtocolMessage Version where
    command _ = "version" ++ (take 5 $ repeat '\NUL')

data MessageHeader = MessageHeader {
    mhMagic    :: BS.ByteString,
    mhCommand  :: BS.ByteString,
    mhLength   :: Word32,
    mhChecksum :: BS.ByteString
} deriving (Show, Read)

instance Binary MessageHeader where

    get = MessageHeader <$> getByteString 4
                        <*> getByteString 12
                        <*> getWord32le
                        <*> getByteString 4

    put (MessageHeader m c l cs) = do
        putByteString m
        putByteString c
        putWord32le l
        putByteString cs

doubleSHA256 :: BS.ByteString -> BS.ByteString
doubleSHA256 = H.hash . H.hash

checksum :: BS.ByteString -> BS.ByteString
checksum = (BS.take 4) . doubleSHA256

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

encodeMessage :: ProtocolMessage a => a -> BS.ByteString
encodeMessage x = let payload = toStrict . encode $ x
                      cmd = BS.pack $ map (fromIntegral . ord) (command x)
                      magic = BS.pack [11,17,9,7]
                      size = fromIntegral $ BS.length payload
                      chksum = checksum payload
                      header = toStrict . encode $ 
                          MessageHeader magic cmd size chksum
                      in header `BS.append` payload

readMessage :: BL.ByteString -> MessageHeader
readMessage payload = decode payload

