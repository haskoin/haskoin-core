{-|
Module      : Network.Haskoin.Network.Message
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Peer-to-peer network message serialization.
-}
module Network.Haskoin.Network.Message
    ( -- * Network Message
      Message(..)
    , MessageHeader(..)
    , msgType
    , putMessage
    , getMessage
    ) where

import           Control.Monad                      (unless)
import qualified Data.ByteString                    as BS
import           Data.Serialize                     (Serialize, encode, get,
                                                     put)
import           Data.Serialize.Get                 (Get, getByteString,
                                                     getWord32be, getWord32le,
                                                     isolate, lookAhead)
import           Data.Serialize.Put                 (Putter, putByteString,
                                                     putWord32be, putWord32le)
import           Data.Word                          (Word32)
import           Network.Haskoin.Block.Common
import           Network.Haskoin.Block.Merkle
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Network.Bloom
import           Network.Haskoin.Network.Common
import           Network.Haskoin.Transaction.Common

-- | Data type representing the header of a 'Message'. All messages sent between
-- nodes contain a message header.
data MessageHeader = MessageHeader
    { -- | magic bytes identify network
      headMagic       :: !Word32
      -- | message type
    , headCmd         :: !MessageCommand
      -- | length of payload
    , headPayloadSize :: !Word32
      -- | checksum of payload
    , headChecksum    :: !CheckSum32
    } deriving (Eq, Show)

instance Serialize MessageHeader where

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
    | MMempool
    | MReject !Reject
    | MSendHeaders
    deriving (Eq, Show)

-- | Get 'MessageCommand' assocated with a message.
msgType :: Message -> MessageCommand
msgType (MVersion _)     = MCVersion
msgType MVerAck          = MCVerAck
msgType (MAddr _)        = MCAddr
msgType (MInv _)         = MCInv
msgType (MGetData _)     = MCGetData
msgType (MNotFound _)    = MCNotFound
msgType (MGetBlocks _)   = MCGetBlocks
msgType (MGetHeaders _)  = MCGetHeaders
msgType (MTx _)          = MCTx
msgType (MBlock _)       = MCBlock
msgType (MMerkleBlock _) = MCMerkleBlock
msgType (MHeaders _)     = MCHeaders

msgType (MFilterLoad _)  = MCFilterLoad
msgType (MFilterAdd _)   = MCFilterAdd
msgType MFilterClear     = MCFilterClear
msgType (MPing _)        = MCPing
msgType (MPong _)        = MCPong
msgType (MAlert _)       = MCAlert
msgType MMempool         = MCMempool
msgType (MReject _)      = MCReject
msgType MSendHeaders     = MCSendHeaders
msgType MGetAddr         = MCGetAddr

-- | Deserializer for network messages.
getMessage :: Network -> Get Message
getMessage net = do
    (MessageHeader mgc cmd len chk) <- get
    bs <- lookAhead $ getByteString $ fromIntegral len
    unless
        (mgc == getNetworkMagic net)
        (fail $ "get: Invalid network magic bytes: " ++ show mgc)
    unless
        (checkSum32 bs == chk)
        (fail $ "get: Invalid message checksum: " ++ show chk)
    if len > 0
        then isolate (fromIntegral len) $
             case cmd of
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
                 _             -> fail $ "get: Invalid command " ++ show cmd
        else case cmd of
                 MCGetAddr     -> return MGetAddr
                 MCVerAck      -> return MVerAck
                 MCFilterClear -> return MFilterClear
                 MCMempool     -> return MMempool
                 MCSendHeaders -> return MSendHeaders
                 _             -> fail $ "get: Invalid command " ++ show cmd

-- | Serializer for network messages.
putMessage :: Network -> Putter Message
putMessage net msg = do
    let (cmd, payload) =
            case msg of
                MVersion m     -> (MCVersion, encode m)
                MVerAck        -> (MCVerAck, BS.empty)
                MAddr m        -> (MCAddr, encode m)
                MInv m         -> (MCInv, encode m)
                MGetData m     -> (MCGetData, encode m)
                MNotFound m    -> (MCNotFound, encode m)
                MGetBlocks m   -> (MCGetBlocks, encode m)
                MGetHeaders m  -> (MCGetHeaders, encode m)
                MTx m          -> (MCTx, encode m)
                MBlock m       -> (MCBlock, encode m)
                MMerkleBlock m -> (MCMerkleBlock, encode m)
                MHeaders m     -> (MCHeaders, encode m)
                MGetAddr       -> (MCGetAddr, BS.empty)
                MFilterLoad m  -> (MCFilterLoad, encode m)
                MFilterAdd m   -> (MCFilterAdd, encode m)
                MFilterClear   -> (MCFilterClear, BS.empty)
                MPing m        -> (MCPing, encode m)
                MPong m        -> (MCPong, encode m)
                MAlert m       -> (MCAlert, encode m)
                MMempool       -> (MCMempool, BS.empty)
                MReject m      -> (MCReject, encode m)
                MSendHeaders   -> (MCSendHeaders, BS.empty)
        chk = checkSum32 payload
        len = fromIntegral $ BS.length payload
        header = MessageHeader (getNetworkMagic net) cmd len chk
    put header
    putByteString payload
