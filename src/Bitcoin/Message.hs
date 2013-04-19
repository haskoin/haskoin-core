module Bitcoin.Message 
( Message(..)
, Version(..)
, iterMessage
, enumMessage
) where

import Control.Applicative

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import Data.Enumerator ( (>>==), ($$) )

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
import Bitcoin.Crypto
import Bitcoin.Util

testnetMagic = 0x0b110907

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
    MPong Pong
    deriving (Show, Read)

iterMessage :: Monad m => E.Iteratee BS.ByteString m Message
iterMessage = do
    headBytes <- EB.take 24
    let (MessageHeader _ cmd length _) = runGet bitcoinGet headBytes
    payloadBytes <- EB.take $ fromIntegral length
    return $ getMessage cmd payloadBytes

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
    _            -> error $ "getMessage: Invalid command string " ++ cmd

enumMessage :: Monad m => Message -> E.Enumerator BS.ByteString m b
enumMessage msg (E.Continue k) =
    let (cmd, mPut) = putMessage msg
        payload = toStrictBS $ runPut mPut
        chksum = doubleSHA256CheckSum payload
        head = toStrictBS . runPut . bitcoinPut $ 
            MessageHeader
                testnetMagic
                cmd
                (fromIntegral $ BS.length payload)
                chksum
        in k $ E.Chunks [head, payload]
enumMessage msg step = E.returnI step

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
    (MPing pi)       -> ("ping", bitcoinPut pi)
    (MPong po)       -> ("pong", bitcoinPut po)

