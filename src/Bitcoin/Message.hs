module Bitcoin.Message 
( Message(..)
, Version(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Type.Version
import Bitcoin.Type.MessageHeader
import Bitcoin.Crypto
import Bitcoin.Util

import qualified Bitcoin.Type as Bitcoin

testnetMagic = BS.pack [11,17,9,7]

data Message = MVersion Version | MVerAck
    deriving (Show, Read)

instance Bitcoin.Type Message where
    get = Bitcoin.get >>= parseHeader
    put m = let (cmd, mPut) = messageToCommand m 
                payload = toStrict . runPut $ mPut
                head = MessageHeader
                           testnetMagic
                           (packCommand cmd)
                           (fromIntegral $ BS.length payload)
                           (checksum payload)
                in Bitcoin.put head >> putByteString payload

-- We have to run the Message decoder on a bytestring of the right length
parseHeader :: MessageHeader -> Get Message
parseHeader (MessageHeader mag cmd len chk) = do
    payload <- Bitcoin.getByteString $ fromIntegral len
    return $ commandToMessage (unpackCommand cmd) payload

commandToMessage :: String -> BS.ByteString -> Message
commandToMessage cmd payload = case cmd of
    "version" -> MVersion $ runGet Bitcoin.get (BL.fromChunks [payload])
    "verack"  -> MVerAck
    _         -> error $ "commandMessage: Invalid command string " ++ cmd

messageToCommand :: Message -> (String, Put)
messageToCommand m = case m of 
    (MVersion v) -> ("version", Bitcoin.put v)
    MVerAck      -> ("verack", Bitcoin.putByteString BS.empty)

