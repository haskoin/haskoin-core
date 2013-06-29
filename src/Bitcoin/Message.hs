module Bitcoin.Message 
( Message(..)
, Version(..)
, toMessage
, fromMessages
, getSerializeSize
) where

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Protocol
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.Addr
import Bitcoin.Protocol.MessageHeader
import Bitcoin.Protocol.Inv
import Bitcoin.Protocol.GetData
import Bitcoin.Protocol.NotFound
import Bitcoin.Protocol.GetBlocks
import Bitcoin.Protocol.GetHeaders
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.Headers
import Bitcoin.Protocol.Ping
import Bitcoin.Protocol.Alert
import Bitcoin.Crypto
import Bitcoin.Util
import Bitcoin.Constants

import qualified Text.Show.Pretty as Pr

import Data.Char

data Message = 
    MVersion Version | 
    MVerAck | 
    MAddr Addr | 
    MInv Inv |
    MGetData GetData |
    MNotFound NotFound |
    MGetBlocks GetBlocks |
    MGetHeaders GetHeaders |
    MTx Tx |
    MBlock Block |
    MHeaders Headers |
    MGetAddr |
    MPing Ping |
    MPong Pong |
    MAlert Alert
    deriving (Show, Read)

-- Conduit transforming streams of bytestrings into messages
toMessage :: MonadIO m => C.Conduit BS.ByteString m Message
toMessage = do
    headBytes <- CB.take 24 -- Header of a message is always 24 bytes
    let (MessageHeader _ cmd len _) = runGet bitcoinGet headBytes
    payloadBytes <- if len > 0
                        then CB.take $ fromIntegral len
                        else return BL.empty
    C.yield $ getMessage cmd payloadBytes
    toMessage

getMessage :: String -> BL.ByteString -> Message
getMessage cmd payload = case cmd of
    "version"    -> MVersion    $ runGet bitcoinGet payload
    "verack"     -> MVerAck
    "addr"       -> MAddr       $ runGet bitcoinGet payload
    "inv"        -> MInv        $ runGet bitcoinGet payload
    "getdata"    -> MGetData    $ runGet bitcoinGet payload
    "notfound"   -> MNotFound   $ runGet bitcoinGet payload
    "getblocks"  -> MGetBlocks  $ runGet bitcoinGet payload
    "getheaders" -> MGetHeaders $ runGet bitcoinGet payload
    "tx"         -> MTx         $ runGet bitcoinGet payload
    "block"      -> MBlock      $ runGet bitcoinGet payload
    "headers"    -> MHeaders    $ runGet bitcoinGet payload
    "getaddr"    -> MGetAddr
    "ping"       -> MPing       $ runGet bitcoinGet payload
    "pong"       -> MPong       $ runGet bitcoinGet payload
    "alert"      -> MAlert      $ runGet bitcoinGet payload
    _            -> error $ "getMessage: Invalid command string " ++ cmd

-- Conduit transforming streams of messages into bytestrings
fromMessages :: Monad m => C.Conduit [Message] m BS.ByteString
fromMessages = C.awaitForever $ \msgs -> forM_ msgs go
    where 
        go msg = do
            let (cmd, mPut) = putMessage msg
                payload = toStrictBS $ runPut mPut
                chksum = doubleSHA256CheckSum payload
                header = toStrictBS . runPut . bitcoinPut $ 
                    MessageHeader
                        testnetMagic
                        cmd
                        (fromIntegral $ BS.length payload)
                        chksum
            C.yield $ header `BS.append` payload

putMessage :: Message -> (String, BitcoinPut)
putMessage m = case m of 
    (MVersion v)     -> ("version", bitcoinPut v)
    MVerAck          -> ("verack", return ())
    (MAddr a)        -> ("addr", bitcoinPut a)
    (MInv i)         -> ("inv", bitcoinPut i)
    (MGetData gd)    -> ("getdata", bitcoinPut gd)
    (MNotFound nf)   -> ("notfound", bitcoinPut nf)
    (MGetBlocks gb)  -> ("getblocks", bitcoinPut gb)
    (MGetHeaders gh) -> ("getheaders", bitcoinPut gh)
    (MTx t)          -> ("tx", bitcoinPut t)
    (MBlock b)       -> ("block", bitcoinPut b)
    (MHeaders h)     -> ("headers", bitcoinPut h)
    MGetAddr         -> ("getaddr", return ())
    (MPing p)        -> ("ping", bitcoinPut p)
    (MPong p)        -> ("pong", bitcoinPut p)
    (MAlert a)       -> ("alert", bitcoinPut a)

getSerializeSize :: Message -> Int
getSerializeSize m = BS.length (payload m) + 24
    where payload = toStrictBS . runPut . snd . putMessage

