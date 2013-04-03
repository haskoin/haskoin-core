module Bitcoin.Message 
( Message(..)
, Version(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.ByteString as BS

import Bitcoin.Type.Version
import Bitcoin.Type.MessageHeader
import Bitcoin.Crypto
import Bitcoin.Util

import qualified Bitcoin.Type as Bitcoin

testnetMagic = BS.pack [11,17,9,7]

data Message = MVersion Version | MVerAck
    deriving (Show, Read)

instance Bitcoin.Type Message where
    get = Bitcoin.get >>= commandToMessage . unpackCommand . command
    put m = let (cmd, mPut) = messageToCommand m 
                payload = toStrict . runPut $ mPut
                head = MessageHeader
                           testnetMagic
                           (packCommand cmd)
                           (fromIntegral $ BS.length payload)
                           (checksum payload)
                in Bitcoin.put head >> putByteString payload

commandToMessage :: String -> Get Message
commandToMessage "version" = MVersion <$> Bitcoin.get
commandToMessage "verack"  = pure MVerAck 
commandToMessage _ = error "commandMessage: Invalid command string"

messageToCommand :: Message -> (String, Put)
messageToCommand m = case m of 
    (MVersion v) -> ("version", Bitcoin.put v)
    MVerAck      -> ("verack", Bitcoin.putByteString BS.empty)

