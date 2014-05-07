module Network.Haskoin.Protocol.Message ( Message(..) ) where

import Control.Monad (unless)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( lookAhead
    , getByteString
    )
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as BS 
    ( length 
    , append
    , empty
    )

import Network.Haskoin.Protocol.MessageHeader
import Network.Haskoin.Protocol.Version
import Network.Haskoin.Protocol.Addr
import Network.Haskoin.Protocol.Inv
import Network.Haskoin.Protocol.GetData
import Network.Haskoin.Protocol.NotFound
import Network.Haskoin.Protocol.GetBlocks
import Network.Haskoin.Protocol.GetHeaders
import Network.Haskoin.Protocol.Tx
import Network.Haskoin.Protocol.Block
import Network.Haskoin.Protocol.Headers
import Network.Haskoin.Protocol.BloomFilter
import Network.Haskoin.Protocol.Ping
import Network.Haskoin.Protocol.Alert

import Network.Haskoin.Util (isolate, encode')
import Network.Haskoin.Crypto (chksum32)

networkMagic :: Word32
networkMagic = 0xf9beb4d9

-- | The 'Message' type is used to identify all the valid messages that can be
-- sent between bitcoin peers. Only values of type 'Message' will be accepted
-- by other bitcoin peers as bitcoin protocol messages need to be correctly
-- serialized with message headers. Serializing a 'Message' value will
-- include the 'MessageHeader' with the correct checksum value automatically.
-- No need to add the 'MessageHeader' separately.
data Message 
    = MVersion Version 
    | MVerAck 
    | MAddr Addr 
    | MInv Inv 
    | MGetData GetData 
    | MNotFound NotFound 
    | MGetBlocks GetBlocks 
    | MGetHeaders GetHeaders 
    | MTx Tx 
    | MBlock Block 
    | MHeaders Headers 
    | MGetAddr 
    | MFilterLoad FilterLoad
    | MFilterAdd FilterAdd
    | MFilterClear
    | MPing Ping 
    | MPong Pong 
    | MAlert Alert
    deriving (Eq, Show)

instance Binary Message where

    get = do
        (MessageHeader mgc cmd len chk) <- get
        bs <- lookAhead $ getByteString $ fromIntegral len
        unless (mgc == networkMagic)
            (fail $ "get: Invalid network magic bytes: " ++ (show mgc))
        unless (chksum32 bs == chk) 
            (fail $ "get: Invalid message checksum: " ++ (show chk))
        if len > 0 
            then isolate (fromIntegral len) $ case cmd of
                MCVersion     -> MVersion <$> get
                MCAddr        -> MAddr <$> get
                MCInv         -> MInv <$> get
                MCGetData     -> MGetData <$> get
                MCNotFound    -> MNotFound <$> get
                MCGetBlocks   -> MGetBlocks <$> get
                MCGetHeaders  -> MGetHeaders <$> get
                MCTx          -> MTx <$> get
                MCBlock       -> MBlock <$> get
                MCHeaders     -> MHeaders <$> get
                MCFilterLoad  -> MFilterLoad <$> get
                MCFilterAdd   -> MFilterAdd <$> get
                MCPing        -> MPing <$> get
                MCPong        -> MPong <$> get
                MCAlert       -> MAlert <$> get
                _             -> fail $ "get: Invalid command " ++ (show cmd)
            else case cmd of
                MCGetAddr     -> return MGetAddr 
                MCVerAck      -> return MVerAck
                MCFilterClear -> return MFilterClear
                _             -> fail $ "get: Invalid command " ++ (show cmd)

    put msg = do
        let (cmd, payload) = case msg of
                (MVersion m)    -> (MCVersion, encode' m)
                (MVerAck)       -> (MCVerAck, BS.empty)
                (MAddr m)       -> (MCAddr, encode' m)
                (MInv m)        -> (MCInv, encode' m)
                (MGetData m)    -> (MCGetData, encode' m)
                (MNotFound m)   -> (MCNotFound, encode' m)
                (MGetBlocks m)  -> (MCGetBlocks, encode' m)
                (MGetHeaders m) -> (MCGetHeaders, encode' m)
                (MTx m)         -> (MCTx, encode' m)
                (MBlock m)      -> (MCBlock, encode' m)
                (MHeaders m)    -> (MCHeaders, encode' m)
                (MGetAddr)      -> (MCGetAddr, BS.empty)
                (MFilterLoad m) -> (MCFilterLoad, encode' m)
                (MFilterAdd m)  -> (MCFilterAdd, encode' m)
                (MFilterClear)  -> (MCFilterClear, BS.empty)
                (MPing m)       -> (MCPing, encode' m)
                (MPong m)       -> (MCPong, encode' m)
                (MAlert m)      -> (MCAlert, encode' m)
            chk = chksum32 payload
            len = fromIntegral $ BS.length payload
            header = MessageHeader networkMagic cmd len chk
        putByteString $ (encode' header) `BS.append` payload
        
