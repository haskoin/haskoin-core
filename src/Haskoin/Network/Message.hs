{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Haskoin.Network.Message
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Peer-to-peer network message serialization.
-}
module Haskoin.Network.Message (
    -- * Network Message
    Message (..),
    MessageHeader (..),
    msgType,
    putMessage,
    getMessage,
) where

import Control.DeepSeq
import Control.Monad (unless)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Serialize (Serialize (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Haskoin.Block.Common
import Haskoin.Block.Merkle
import Haskoin.Crypto.Hash
import Haskoin.Data
import Haskoin.Network.Bloom
import Haskoin.Network.Common
import Haskoin.Transaction.Common

{- | Data type representing the header of a 'Message'. All messages sent between
 nodes contain a message header.
-}
data MessageHeader = MessageHeader
    { -- | magic bytes identify network
      headMagic :: !Word32
    , -- | message type
      headCmd :: !MessageCommand
    , -- | length of payload
      headPayloadSize :: !Word32
    , -- | checksum of payload
      headChecksum :: !CheckSum32
    }
    deriving (Eq, Show, Generic, NFData)

instance Serial MessageHeader where
    deserialize =
        MessageHeader
            <$> getWord32be
            <*> deserialize
            <*> getWord32le
            <*> deserialize

    serialize (MessageHeader m c l chk) = do
        putWord32be m
        serialize c
        putWord32le l
        serialize chk

instance Binary MessageHeader where
    put = serialize
    get = deserialize

instance Serialize MessageHeader where
    put = serialize
    get = deserialize

{- | The 'Message' type is used to identify all the valid messages that can be
 sent between bitcoin peers. Only values of type 'Message' will be accepted
 by other bitcoin peers as bitcoin protocol messages need to be correctly
 serialized with message headers. Serializing a 'Message' value will
 include the 'MessageHeader' with the correct checksum value automatically.
 No need to add the 'MessageHeader' separately.
-}
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
getMessage :: MonadGet m => Network -> m Message
getMessage net = do
    (MessageHeader mgc cmd len chk) <- deserialize
    bs <- lookAhead $ getByteString $ fromIntegral len
    unless
        (mgc == getNetworkMagic net)
        (fail $ "get: Invalid network magic bytes: " ++ show mgc)
    unless
        (checkSum32 bs == chk)
        (fail $ "get: Invalid message checksum: " ++ show chk)
    if len > 0
        then do
            bs <- ensure (fromIntegral len)
            let f = case cmd of
                    MCVersion -> MVersion <$> deserialize
                    MCAddr -> MAddr <$> deserialize
                    MCInv -> MInv <$> deserialize
                    MCGetData -> MGetData <$> deserialize
                    MCNotFound -> MNotFound <$> deserialize
                    MCGetBlocks -> MGetBlocks <$> deserialize
                    MCGetHeaders -> MGetHeaders <$> deserialize
                    MCTx -> MTx <$> deserialize
                    MCBlock -> MBlock <$> deserialize
                    MCMerkleBlock -> MMerkleBlock <$> deserialize
                    MCHeaders -> MHeaders <$> deserialize
                    MCFilterLoad -> MFilterLoad <$> deserialize
                    MCFilterAdd -> MFilterAdd <$> deserialize
                    MCPing -> MPing <$> deserialize
                    MCPong -> MPong <$> deserialize
                    MCAlert -> MAlert <$> deserialize
                    MCReject -> MReject <$> deserialize
                    MCOther c -> MOther c <$> getByteString (fromIntegral len)
                    _ ->
                        fail $
                            "get: command " ++ show cmd
                                ++ " should not carry a payload"
            either fail return (runGetS f bs)
        else case cmd of
            MCGetAddr -> return MGetAddr
            MCVerAck -> return MVerAck
            MCFilterClear -> return MFilterClear
            MCMempool -> return MMempool
            MCSendHeaders -> return MSendHeaders
            MCOther c -> return (MOther c BS.empty)
            _ ->
                fail $
                    "get: command " ++ show cmd
                        ++ " is expected to carry a payload"

-- | Serializer for network messages.
putMessage :: MonadPut m => Network -> Message -> m ()
putMessage net msg = do
    let (cmd, payload) =
            case msg of
                MVersion m -> (MCVersion, runPutS $ serialize m)
                MVerAck -> (MCVerAck, BS.empty)
                MAddr m -> (MCAddr, runPutS $ serialize m)
                MInv m -> (MCInv, runPutS $ serialize m)
                MGetData m -> (MCGetData, runPutS $ serialize m)
                MNotFound m -> (MCNotFound, runPutS $ serialize m)
                MGetBlocks m -> (MCGetBlocks, runPutS $ serialize m)
                MGetHeaders m -> (MCGetHeaders, runPutS $ serialize m)
                MTx m -> (MCTx, runPutS $ serialize m)
                MBlock m -> (MCBlock, runPutS $ serialize m)
                MMerkleBlock m -> (MCMerkleBlock, runPutS $ serialize m)
                MHeaders m -> (MCHeaders, runPutS $ serialize m)
                MGetAddr -> (MCGetAddr, BS.empty)
                MFilterLoad m -> (MCFilterLoad, runPutS $ serialize m)
                MFilterAdd m -> (MCFilterAdd, runPutS $ serialize m)
                MFilterClear -> (MCFilterClear, BS.empty)
                MPing m -> (MCPing, runPutS $ serialize m)
                MPong m -> (MCPong, runPutS $ serialize m)
                MAlert m -> (MCAlert, runPutS $ serialize m)
                MMempool -> (MCMempool, BS.empty)
                MReject m -> (MCReject, runPutS $ serialize m)
                MSendHeaders -> (MCSendHeaders, BS.empty)
                MOther c p -> (MCOther c, p)
        chk = checkSum32 payload
        len = fromIntegral $ BS.length payload
        header = MessageHeader (getNetworkMagic net) cmd len chk
    serialize header
    putByteString payload
