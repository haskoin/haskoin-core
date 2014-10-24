module Network.Haskoin.Protocol.Message 
( Message(..) 
, MessageHeader(..) 
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (unless)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( lookAhead
    , getByteString
    , getWord32le
    , getWord32be
    )
import Data.Binary.Put 
    ( putByteString
    , putWord32le
    , putWord32be
    )
import qualified Data.ByteString as BS 
    ( length 
    , append
    , empty
    )

import Network.Haskoin.Protocol.Types
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle
import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Crypto.Bloom
import Network.Haskoin.Constants
import Network.Haskoin.Util 

-- | Data type representing the header of a 'Message'. All messages sent between
-- nodes contain a message header.
data MessageHeader = 
    MessageHeader {
                  -- | Network magic bytes. It is used to differentiate 
                  -- messages meant for different bitcoin networks, such as
                  -- prodnet and testnet.
                    headMagic       :: !Word32
                  -- | Message command identifying the type of message.
                  -- included in the payload.
                  , headCmd         :: !MessageCommand
                  -- | Byte length of the payload.
                  , headPayloadSize :: !Word32
                  -- | Checksum of the payload. 
                  , headChecksum    :: !CheckSum32
                  } deriving (Eq, Show, Read)

instance NFData MessageHeader where
    rnf (MessageHeader m c p s) = rnf m `seq` rnf c `seq` rnf p `seq` rnf s

instance Binary MessageHeader where

    get = MessageHeader <$> getWord32be
                        <*> get
                        <*> getWord32le
                        <*> get

    put (MessageHeader m c l chk) = do
        putWord32be m
        put         c
        putWord32le l
        put         chk

-- | The 'Message' type is used to identify all the valid messages that can be
-- sent between bitcoin peers. Only values of type 'Message' will be accepted
-- by other bitcoin peers as bitcoin protocol messages need to be correctly
-- serialized with message headers. Serializing a 'Message' value will
-- include the 'MessageHeader' with the correct checksum value automatically.
-- No need to add the 'MessageHeader' separately.
data Message 
    = MVersion !Version 
    | MVerAck 
    | MAddr !Addr 
    | MInv !Inv 
    | MGetData !GetData 
    | MNotFound !NotFound 
    | MGetBlocks !GetBlocks 
    | MGetHeaders !GetHeaders 
    | MTx !Tx 
    | MBlock !Block 
    | MMerkleBlock !MerkleBlock 
    | MHeaders !Headers 
    | MGetAddr 
    | MFilterLoad !FilterLoad
    | MFilterAdd !FilterAdd
    | MFilterClear
    | MPing !Ping 
    | MPong !Pong 
    | MAlert !Alert
    | MReject Reject
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
                MCMerkleBlock -> MMerkleBlock <$> get
                MCHeaders     -> MHeaders <$> get
                MCFilterLoad  -> MFilterLoad <$> get
                MCFilterAdd   -> MFilterAdd <$> get
                MCPing        -> MPing <$> get
                MCPong        -> MPong <$> get
                MCAlert       -> MAlert <$> get
                MCReject      -> MReject <$> get
                _             -> fail $ "get: Invalid command " ++ (show cmd)
            else case cmd of
                MCGetAddr     -> return MGetAddr 
                MCVerAck      -> return MVerAck
                MCFilterClear -> return MFilterClear
                _             -> fail $ "get: Invalid command " ++ (show cmd)

    put msg = do
        let (cmd, payload) = case msg of
                MVersion m     -> (MCVersion, encode' m)
                MVerAck        -> (MCVerAck, BS.empty)
                MAddr m        -> (MCAddr, encode' m)
                MInv m         -> (MCInv, encode' m)
                MGetData m     -> (MCGetData, encode' m)
                MNotFound m    -> (MCNotFound, encode' m)
                MGetBlocks m   -> (MCGetBlocks, encode' m)
                MGetHeaders m  -> (MCGetHeaders, encode' m)
                MTx m          -> (MCTx, encode' m)
                MBlock m       -> (MCBlock, encode' m)
                MMerkleBlock m -> (MCMerkleBlock, encode' m)
                MHeaders m     -> (MCHeaders, encode' m)
                MGetAddr       -> (MCGetAddr, BS.empty)
                MFilterLoad m  -> (MCFilterLoad, encode' m)
                MFilterAdd m   -> (MCFilterAdd, encode' m)
                MFilterClear   -> (MCFilterClear, BS.empty)
                MPing m        -> (MCPing, encode' m)
                MPong m        -> (MCPong, encode' m)
                MAlert m       -> (MCAlert, encode' m)
                MReject m      -> (MCReject, encode' m)
            chk = chksum32 payload
            len = fromIntegral $ BS.length payload
            header = MessageHeader networkMagic cmd len chk
        putByteString $ (encode' header) `BS.append` payload
        
