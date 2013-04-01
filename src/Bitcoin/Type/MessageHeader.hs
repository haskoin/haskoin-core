module Bitcoin.Type.MessageHeader 
( MessageHeader(..) 
, packCommand
, unpackCommand
) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Control.Applicative

import qualified Data.ByteString as BS

data MessageHeader = MessageHeader {
    magic       :: BS.ByteString,
    command     :: BS.ByteString,
    payloadSize :: Word32,
    chksum      :: BS.ByteString
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

-- Helper function to create a ByteString command from a String
packCommand :: String -> BS.ByteString
packCommand s 
    | length s <= 12 =
        let fixed = s ++ (take (12 - (length s)) (repeat '\NUL'))
            in BS.pack $  map (fromIntegral . ord) fixed
    | otherwise = error "packCommand: command string is too long"

-- Helper function to get a String from a command ByteString
unpackCommand :: BS.ByteString -> String
unpackCommand b 
    | BS.length b <= 12 = 
        takeWhile (\x -> x /= '\NUL') $ 
            map (chr . fromIntegral) (BS.unpack b)
    | otherwise = error "unpackCommand: command bytestring is too long"

