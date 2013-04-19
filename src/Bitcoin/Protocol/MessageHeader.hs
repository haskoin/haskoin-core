module Bitcoin.Protocol.MessageHeader 
( MessageHeader(..) 
, packCommand
, unpackCommand
) where

import Data.Char
import Bitcoin.Protocol
import Control.Applicative

import qualified Data.ByteString as BS

data MessageHeader = MessageHeader {
    headMagic       :: Word32,
    headCmd         :: String,
    headPayloadSize :: Word32,
    headChecksum    :: Word32
} deriving (Eq, Show, Read)

instance BitcoinProtocol MessageHeader where

    bitcoinGet = MessageHeader <$> getWord32be
                               <*> (unpackCommand <$> getByteString 12)
                               <*> getWord32le
                               <*> getWord32be

    bitcoinPut (MessageHeader m c l cs) = do
        putWord32be m
        putByteString $ packCommand c
        putWord32le l
        putWord32be cs

packCommand :: String -> BS.ByteString
packCommand s = BS.pack $ take 12 buildList
    where buildList = (map (fromIntegral . ord) s) ++ (repeat 0)

unpackCommand :: BS.ByteString -> String
unpackCommand b = takeWhile (/= '\NUL') buildString
    where buildString = map (chr . fromIntegral) (BS.unpack b)

