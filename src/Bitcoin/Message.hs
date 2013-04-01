module Bitcoin.Message 
( Message(..)
, Version(..)
) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Type.Version
import Bitcoin.Type.MessageHeader
import Bitcoin.Crypto
import Bitcoin.Util

data Message = MVersion Version
    deriving (Show, Read)

encodeMessage :: Message -> (String, BS.ByteString)
encodeMessage m = 
    case m of
         (MVersion v) -> ("version", serialize v)
         _ -> error "messageCommand : Invalid protocol message"
         where serialize = toStrict . encode

commandMessage :: String -> BL.ByteString -> Message
commandMessage "version" = MVersion . decode
commandMessage _ = error "commandMessage: Invalid command string"

putMessage :: Message -> BS.ByteString
putMessage m = 
    let (cmd, payload) = encodeMessage m
        head = toStrict . encode $ 
            MessageHeader
                (BS.pack [11,17,9,7])
                (packCommand cmd)
                (fromIntegral $ BS.length payload)
                (checksum payload)
        in head `BS.append` payload

getMessage :: BL.ByteString -> Message
getMessage b =
    let head = decode b :: MessageHeader
        cmd = unpackCommand $ command head
        in commandMessage cmd b


