module Bitcoin.Message 
( Message(..)
, Version(..)
, iterMessage
, enumMessage
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import Data.Enumerator ( (>>==), ($$) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Type.Version
import Bitcoin.Type.Addr
import Bitcoin.Type.MessageHeader
import Bitcoin.Type.Inv
import Bitcoin.Type.GetData
import Bitcoin.Type.NotFound
import Bitcoin.Type.GetBlocks
import Bitcoin.Type.GetHeaders
import Bitcoin.Type.Tx
import Bitcoin.Crypto
import Bitcoin.Util

import qualified Bitcoin.Type as Bitcoin

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
    MTx Tx
    deriving (Show, Read)

iterMessage :: Monad m => E.Iteratee BS.ByteString m Message
iterMessage = do
    headBytes <- EB.take 24
    let head   = runGet Bitcoin.get headBytes :: MessageHeader
        length = payloadSize head
        cmd    = command head
    payloadBytes <- EB.take $ fromIntegral length
    return $ getMessage (unpackCommand cmd) payloadBytes

getMessage :: String -> BL.ByteString -> Message
getMessage cmd payload = case cmd of
    "version"    -> MVersion    $ runGet Bitcoin.get payload
    "verack"     -> MVerAck
    "addr"       -> MAddr       $ runGet Bitcoin.get payload
    "inv"        -> MInv        $ runGet Bitcoin.get payload
    "getdata"    -> MGetData    $ runGet Bitcoin.get payload
    "notfound"   -> MNotFound   $ runGet Bitcoin.get payload
    "getblocks"  -> MGetBlocks  $ runGet Bitcoin.get payload
    "getheaders" -> MGetHeaders $ runGet Bitcoin.get payload
    "tx"         -> MTx         $ runGet Bitcoin.get payload
    _            -> error $ "getMessage: Invalid command string " ++ cmd

enumMessage :: Monad m => Message -> E.Enumerator BS.ByteString m b
enumMessage msg (E.Continue k) =
    let (cmd, mPut) = putMessage msg
        payload = toStrict $ runPut $ mPut
        head = toStrict . runPut . Bitcoin.put $ MessageHeader
            testnetMagic
            (packCommand cmd)
            (fromIntegral $ BS.length payload)
            (checksum payload)
        in k (E.Chunks [head `BS.append` payload])
enumMessage msg step = E.returnI step

putMessage :: Message -> (String, Put)
putMessage m = case m of 
    (MVersion v)     -> ("version", Bitcoin.put v)
    MVerAck          -> ("verack", return ())
    (MAddr a)        -> ("addr", Bitcoin.put a)
    (MInv i)         -> ("inv", Bitcoin.put i)
    (MGetData gd)    -> ("getdata", Bitcoin.put gd)
    (MNotFound nf)   -> ("notfound", Bitcoin.put nf)
    (MGetBlocks gb)  -> ("getblocks", Bitcoin.put gb)
    (MGetHeaders gh) -> ("getheaders", Bitcoin.put gh)
    (MTx t)          -> ("tx", Bitcoin.put t)

