{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Peer-to-peer network message serialization.
module Bitcoin.Network.Message (
    -- * Network Message
    Message (..),
    MessageHeader (..),
    msgType,
    putMessage,
    getMessage,
) where

import Bitcoin.Block.Common (
    Block,
    GetBlocks,
    GetHeaders,
    Headers,
 )
import Bitcoin.Block.Merkle (MerkleBlock)
import Bitcoin.Crypto.Hash (CheckSum32, checkSum32)
import Bitcoin.Data (Network (getNetworkMagic))
import Bitcoin.Network.Bloom (FilterAdd, FilterLoad)
import Bitcoin.Network.Common (
    Addr,
    Alert,
    GetData,
    Inv,
    MessageCommand (..),
    NotFound,
    Ping,
    Pong,
    Reject,
    Version,
 )
import Bitcoin.Transaction.Common (Tx)
import qualified Bitcoin.Util as U
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Binary (Binary (..))
import qualified Data.Binary as Bin
import Data.Binary.Get (Get, getByteString, getLazyByteString, getWord32be, getWord32le, lookAhead)
import Data.Binary.Put (Put, putLazyByteString, putWord32be, putWord32le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word32)
import GHC.Generics (Generic)


-- | Data type representing the header of a 'Message'. All messages sent between
-- nodes contain a message header.
data MessageHeader = MessageHeader
    { headMagic :: !Word32
    -- ^ magic bytes identify network
    , headCmd :: !MessageCommand
    -- ^ message type
    , headPayloadSize :: !Word32
    -- ^ length of payload
    , headChecksum :: !CheckSum32
    -- ^ checksum of payload
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary MessageHeader where
    get =
        MessageHeader
            <$> getWord32be
            <*> get
            <*> getWord32le
            <*> get


    put (MessageHeader m c l chk) = do
        putWord32be m
        put c
        putWord32le l
        put chk


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
    | MMempool
    | MReject !Reject
    | MSendHeaders
    | MOther !ByteString !ByteString
    deriving (Eq, Show, Generic, NFData)


-- | Get 'MessageCommand' assocated with a message.
msgType :: Message -> MessageCommand
msgType (MVersion _) = MCVersion
msgType MVerAck = MCVerAck
msgType (MAddr _) = MCAddr
msgType (MInv _) = MCInv
msgType (MGetData _) = MCGetData
msgType (MNotFound _) = MCNotFound
msgType (MGetBlocks _) = MCGetBlocks
msgType (MGetHeaders _) = MCGetHeaders
msgType (MTx _) = MCTx
msgType (MBlock _) = MCBlock
msgType (MMerkleBlock _) = MCMerkleBlock
msgType (MHeaders _) = MCHeaders
msgType (MFilterLoad _) = MCFilterLoad
msgType (MFilterAdd _) = MCFilterAdd
msgType MFilterClear = MCFilterClear
msgType (MPing _) = MCPing
msgType (MPong _) = MCPong
msgType (MAlert _) = MCAlert
msgType MMempool = MCMempool
msgType (MReject _) = MCReject
msgType MSendHeaders = MCSendHeaders
msgType MGetAddr = MCGetAddr
msgType (MOther c _) = MCOther c


-- | Deserializer for network messages.
getMessage :: Network -> Get Message
getMessage net = do
    (MessageHeader mgc cmd len chk) <- get
    bs <- getLazyByteString $ fromIntegral len
    unless
        (mgc == getNetworkMagic net)
        (fail $ "get: Invalid network magic bytes: " ++ show mgc)
    unless
        (checkSum32 bs == chk)
        (fail $ "get: Invalid message checksum: " ++ show chk)
    if len > 0
        then do
            let f = case cmd of
                    MCVersion -> MVersion <$> get
                    MCAddr -> MAddr <$> get
                    MCInv -> MInv <$> get
                    MCGetData -> MGetData <$> get
                    MCNotFound -> MNotFound <$> get
                    MCGetBlocks -> MGetBlocks <$> get
                    MCGetHeaders -> MGetHeaders <$> get
                    MCTx -> MTx <$> get
                    MCBlock -> MBlock <$> get
                    MCMerkleBlock -> MMerkleBlock <$> get
                    MCHeaders -> MHeaders <$> get
                    MCFilterLoad -> MFilterLoad <$> get
                    MCFilterAdd -> MFilterAdd <$> get
                    MCPing -> MPing <$> get
                    MCPong -> MPong <$> get
                    MCAlert -> MAlert <$> get
                    MCReject -> MReject <$> get
                    MCOther c -> MOther c <$> getByteString (fromIntegral len)
                    _ ->
                        fail $
                            "get: command "
                                ++ show cmd
                                ++ " should not carry a payload"
            either fail return $ U.runGet f bs
        else case cmd of
            MCGetAddr -> return MGetAddr
            MCVerAck -> return MVerAck
            MCFilterClear -> return MFilterClear
            MCMempool -> return MMempool
            MCSendHeaders -> return MSendHeaders
            MCOther c -> return (MOther c BS.empty)
            _ ->
                fail $
                    "get: command "
                        ++ show cmd
                        ++ " is expected to carry a payload"


-- | Serializer for network messages.
putMessage :: Network -> Message -> Put
putMessage net msg = do
    let (cmd, payload) =
            case msg of
                MVersion m -> (MCVersion, Bin.encode m)
                MVerAck -> (MCVerAck, BSL.empty)
                MAddr m -> (MCAddr, Bin.encode m)
                MInv m -> (MCInv, Bin.encode m)
                MGetData m -> (MCGetData, Bin.encode m)
                MNotFound m -> (MCNotFound, Bin.encode m)
                MGetBlocks m -> (MCGetBlocks, Bin.encode m)
                MGetHeaders m -> (MCGetHeaders, Bin.encode m)
                MTx m -> (MCTx, Bin.encode m)
                MBlock m -> (MCBlock, Bin.encode m)
                MMerkleBlock m -> (MCMerkleBlock, Bin.encode m)
                MHeaders m -> (MCHeaders, Bin.encode m)
                MGetAddr -> (MCGetAddr, BSL.empty)
                MFilterLoad m -> (MCFilterLoad, Bin.encode m)
                MFilterAdd m -> (MCFilterAdd, Bin.encode m)
                MFilterClear -> (MCFilterClear, BSL.empty)
                MPing m -> (MCPing, Bin.encode m)
                MPong m -> (MCPong, Bin.encode m)
                MAlert m -> (MCAlert, Bin.encode m)
                MMempool -> (MCMempool, BSL.empty)
                MReject m -> (MCReject, Bin.encode m)
                MSendHeaders -> (MCSendHeaders, BSL.empty)
                MOther c p -> (MCOther c, BSL.fromStrict p)
        chk = checkSum32 payload
        len = fromIntegral $ BSL.length payload
        header = MessageHeader (getNetworkMagic net) cmd len chk
    put header
    putLazyByteString payload
