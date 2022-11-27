{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Common functions and data types related to peer-to-peer network.
module Bitcoin.Network.Common (
    -- * Network Data Types
    Addr (..),
    NetworkAddressTime,
    Alert (..),
    GetData (..),
    Inv (..),
    InvVector (..),
    InvType (..),
    NetworkAddress (..),
    NotFound (..),
    Ping (..),
    Pong (..),
    Reject (..),
    RejectCode (..),
    VarInt (..),
    VarString (..),
    Version (..),
    MessageCommand (..),
    reject,
    nodeNone,
    nodeNetwork,
    nodeGetUTXO,
    nodeBloom,
    nodeWitness,
    nodeXThin,
    commandToString,
    stringToCommand,
    putVarInt,
) where

import Bitcoin.Crypto.Hash (Hash256)
import qualified Bitcoin.Util as U
import Control.DeepSeq (NFData)
import Control.Monad (forM_, liftM2, replicateM, unless)
import Data.Binary (Binary (..), Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Network.Socket (HostAddress6, SockAddr (..))
import Text.Read (Lexeme (String))
import qualified Text.Read as R


-- | Network address with a timestamp.
type NetworkAddressTime = (Word32, NetworkAddress)


-- | Provides information about known nodes in the bitcoin network. An 'Addr'
-- type is sent inside a 'Message' as a response to a 'GetAddr' message.
newtype Addr = Addr
    { -- List of addresses of other nodes on the network with timestamps.
      addrList :: [NetworkAddressTime]
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary Addr where
    get = Addr <$> (repList =<< get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) action
        action = liftM2 (,) Get.getWord32le get


    put (Addr xs) = do
        putVarInt $ length xs
        forM_ xs $ \(a, b) -> Put.putWord32le a >> put b


-- | Data type describing signed messages that can be sent between bitcoin
-- nodes to display important notifications to end users about the health of
-- the network.
data Alert = Alert
    { alertPayload :: !VarString
    -- ^ Alert payload.
    , alertSignature :: !VarString
    -- ^ ECDSA signature of the payload
    }
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary Alert where
    get = Alert <$> get <*> get
    put (Alert p s) = put p >> put s


-- | The 'GetData' type is used to retrieve information on a specific object
-- ('Block' or 'Tx') identified by the objects hash. The payload of a 'GetData'
-- request is a list of 'InvVector' which represent all the hashes of objects
-- that a node wants. The response to a 'GetBlock' message will be either a
-- 'Block' or a 'Tx' message depending on the type of the object referenced by
-- the hash. Usually, 'GetData' messages are sent after a node receives an 'Inv'
-- message that contains unknown object hashes.
newtype GetData = GetData
    { getDataList :: [InvVector]
    -- ^ list of object hashes
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary GetData where
    get = GetData <$> (repList =<< get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) get


    put (GetData xs) = do
        putVarInt $ length xs
        forM_ xs put


-- | 'Inv' messages are used by nodes to advertise their knowledge of new
-- objects by publishing a list of hashes to a peer. 'Inv' messages can be sent
-- unsolicited or in response to a 'GetBlocks' message.
newtype Inv = Inv
    { invList :: [InvVector]
    -- ^ inventory
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary Inv where
    get = Inv <$> (repList =<< get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) get


    put (Inv xs) = do
        putVarInt $ length xs
        forM_ xs put


-- | Data type identifying the type of an inventory vector. SegWit types are
-- only used in 'GetData' messages, not 'Inv'.
data InvType
    = -- | error
      InvError
    | -- | transaction
      InvTx
    | -- | block
      InvBlock
    | -- | filtered block
      InvMerkleBlock
    | -- | segwit transaction
      InvWitnessTx
    | -- | segwit block
      InvWitnessBlock
    | -- | segwit filtered block
      InvWitnessMerkleBlock
    | -- | unknown inv type
      InvType Word32
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary InvType where
    get = go =<< Get.getWord32le
      where
        go x =
            case x of
                0 -> return InvError
                1 -> return InvTx
                2 -> return InvBlock
                3 -> return InvMerkleBlock
                _
                    | x == 1 `shiftL` 30 + 1 -> return InvWitnessTx
                    | x == 1 `shiftL` 30 + 2 -> return InvWitnessBlock
                    | x == 1 `shiftL` 30 + 3 -> return InvWitnessMerkleBlock
                    | otherwise -> return (InvType x)
    put x =
        Put.putWord32le $
            case x of
                InvError -> 0
                InvTx -> 1
                InvBlock -> 2
                InvMerkleBlock -> 3
                InvWitnessTx -> 1 `shiftL` 30 + 1
                InvWitnessBlock -> 1 `shiftL` 30 + 2
                InvWitnessMerkleBlock -> 1 `shiftL` 30 + 3
                InvType w -> w


-- | Invectory vectors represent hashes identifying objects such as a 'Block' or
-- a 'Tx'. They notify other peers about new data or data they have otherwise
-- requested.
data InvVector = InvVector
    { invType :: !InvType
    -- ^ type of object
    , invHash :: !Hash256
    -- ^ 256-bit hash of object
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary InvVector where
    get = InvVector <$> get <*> get
    put (InvVector t h) = put t >> put h


-- | Data type describing a bitcoin network address. Addresses are stored in
-- IPv6 format. IPv4 addresses are mapped to IPv6 using IPv4 mapped IPv6
-- addresses: <http://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses>.
data NetworkAddress = NetworkAddress
    { naServices :: !Word64
    -- ^ bitmask of services available for this address
    , naAddress :: !HostAddress6
    -- ^ address and port information
    }
    deriving (Eq, Show, Generic, NFData)


putHostAddr6 :: HostAddress6 -> Put
putHostAddr6 (a, b, c, d) = do
    Put.putWord32be a
    Put.putWord32be b
    Put.putWord32be c
    Put.putWord32be d


getHostAddr6 :: Get HostAddress6
getHostAddr6 =
    (,,,)
        <$> Get.getWord32be
        <*> Get.getWord32be
        <*> Get.getWord32be
        <*> Get.getWord32be


instance Binary NetworkAddress where
    get =
        NetworkAddress
            <$> Get.getWord64le
            <*> getHostAddr6
    put (NetworkAddress s a) =
        Put.putWord64le s >> putHostAddr6 a


-- | A 'NotFound' message is returned as a response to a 'GetData' message
-- whe one of the requested objects could not be retrieved. This could happen,
-- for example, if a tranasaction was requested and was not available in the
-- memory pool of the receiving node.
newtype NotFound = NotFound
    { notFoundList :: [InvVector]
    -- ^ Inventory vectors related to this request
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary NotFound where
    get = NotFound <$> (repList =<< get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) get


    put (NotFound xs) = do
        putVarInt $ length xs
        forM_ xs put


-- | A 'Ping' message is sent to bitcoin peers to check if a connection is still
-- open.
newtype Ping = Ping
    { pingNonce :: Word64
    -- ^ A random nonce used to identify the recipient of the ping
    -- request once a Pong response is received.
    }
    deriving (Eq, Show, Read, Generic, NFData)


-- | A Pong message is sent as a response to a ping message.
newtype Pong = Pong
    { pongNonce :: Word64
    -- ^ nonce from corresponding 'Ping'
    }
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary Ping where
    get = Ping <$> Get.getWord64le
    put (Ping n) = Put.putWord64le n


instance Binary Pong where
    get = Pong <$> Get.getWord64le
    put (Pong n) = Put.putWord64le n


-- | The 'Reject' message is sent when messages are rejected by a peer.
data Reject = Reject
    { rejectMessage :: !MessageCommand
    -- ^ type of message rejected
    , rejectCode :: !RejectCode
    -- ^ rejection code
    , rejectReason :: !VarString
    -- ^ text reason for rejection
    , rejectData :: !ByteString
    -- ^ extra data such as block or tx hash
    }
    deriving (Eq, Show, Read, Generic, NFData)


-- | Rejection code associated to the 'Reject' message.
data RejectCode
    = RejectMalformed
    | RejectInvalid
    | RejectObsolete
    | RejectDuplicate
    | RejectNonStandard
    | RejectDust
    | RejectInsufficientFee
    | RejectCheckpoint
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary RejectCode where
    get =
        Get.getWord8 >>= \case
            0x01 -> return RejectMalformed
            0x10 -> return RejectInvalid
            0x11 -> return RejectObsolete
            0x12 -> return RejectDuplicate
            0x40 -> return RejectNonStandard
            0x41 -> return RejectDust
            0x42 -> return RejectInsufficientFee
            0x43 -> return RejectCheckpoint
            code ->
                fail $
                    unwords
                        [ "Reject get: Invalid code"
                        , show code
                        ]


    put =
        Put.putWord8 . \case
            RejectMalformed -> 0x01
            RejectInvalid -> 0x10
            RejectObsolete -> 0x11
            RejectDuplicate -> 0x12
            RejectNonStandard -> 0x40
            RejectDust -> 0x41
            RejectInsufficientFee -> 0x42
            RejectCheckpoint -> 0x43


-- | Convenience function to build a 'Reject' message.
reject :: MessageCommand -> RejectCode -> ByteString -> Reject
reject cmd code reason =
    Reject cmd code (VarString reason) BS.empty


instance Binary Reject where
    get =
        get >>= \(VarString bs) ->
            Reject (stringToCommand bs)
                <$> get
                <*> get
                <*> maybeData
      where
        maybeData =
            Get.isEmpty >>= \done ->
                if done
                    then return BS.empty
                    else Get.getByteString 32
    put (Reject cmd code reason dat) = do
        put $ VarString $ commandToString cmd
        put code
        put reason
        unless (BS.null dat) $ Put.putByteString dat


-- | Data type representing a variable-length integer. The 'VarInt' type
-- usually precedes an array or a string that can vary in length.
newtype VarInt = VarInt {getVarInt :: Word64}
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary VarInt where
    get = VarInt <$> (Get.getWord8 >>= go)
      where
        go 0xff = Get.getWord64le
        go 0xfe = fromIntegral <$> Get.getWord32le
        go 0xfd = fromIntegral <$> Get.getWord16le
        go x = return $ fromIntegral x


    put (VarInt x)
        | x < 0xfd =
            Put.putWord8 $ fromIntegral x
        | x <= 0xffff = do
            Put.putWord8 0xfd
            Put.putWord16le $ fromIntegral x
        | x <= 0xffffffff = do
            Put.putWord8 0xfe
            Put.putWord32le $ fromIntegral x
        | otherwise = do
            Put.putWord8 0xff
            Put.putWord64le x


putVarInt :: Integral a => a -> Put
putVarInt = put . VarInt . fromIntegral


-- | Data type for serialization of variable-length strings.
newtype VarString = VarString {getVarString :: ByteString}
    deriving (Eq, Show, Read, Generic, NFData)


instance Binary VarString where
    get = VarString <$> (readBS =<< get)
      where
        readBS (VarInt len) = Get.getByteString (fromIntegral len)


    put (VarString bs) = do
        putVarInt $ BS.length bs
        Put.putByteString bs


-- | When a bitcoin node creates an outgoing connection to another node,
-- the first message it will send is a 'Version' message. The other node
-- will similarly respond with it's own 'Version' message.
data Version = Version
    { version :: !Word32
    -- ^ protocol version
    , services :: !Word64
    -- ^ features supported by this connection
    , timestamp :: !Word64
    -- ^ unix timestamp
    , addrRecv :: !NetworkAddress
    -- ^ network address of remote node
    , addrSend :: !NetworkAddress
    -- ^ network address of sending node
    , verNonce :: !Word64
    -- ^ random nonce to detect connection to self
    , userAgent :: !VarString
    -- ^ user agent string
    , startHeight :: !Word32
    -- ^ height of the last block in sending node
    , relay :: !Bool
    -- ^ relay transactions flag (BIP-37)
    }
    deriving (Eq, Show, Generic, NFData)


instance Binary Version where
    get =
        Version
            <$> Get.getWord32le
            <*> Get.getWord64le
            <*> Get.getWord64le
            <*> get
            <*> get
            <*> Get.getWord64le
            <*> get
            <*> Get.getWord32le
            <*> (go =<< Get.isEmpty)
      where
        go True = return True
        go False = getBool


    put (Version v s t ar as n ua sh r) = do
        Put.putWord32le v
        Put.putWord64le s
        Put.putWord64le t
        put ar
        put as
        Put.putWord64le n
        put ua
        Put.putWord32le sh
        putBool r


-- | 0x00 is 'False', anything else is 'True'.
getBool :: Get Bool
getBool = go =<< Get.getWord8
  where
    go 0 = return False
    go _ = return True


putBool :: Bool -> Put
putBool True = Put.putWord8 1
putBool False = Put.putWord8 0


-- | A 'MessageCommand' is included in a 'MessageHeader' in order to identify
-- the type of message present in the payload. This allows the message
-- de-serialization code to know how to decode a particular message payload.
-- Every valid 'Message' constructor has a corresponding 'MessageCommand'
-- constructor.
data MessageCommand
    = MCVersion
    | MCVerAck
    | MCAddr
    | MCInv
    | MCGetData
    | MCNotFound
    | MCGetBlocks
    | MCGetHeaders
    | MCTx
    | MCBlock
    | MCMerkleBlock
    | MCHeaders
    | MCGetAddr
    | MCFilterLoad
    | MCFilterAdd
    | MCFilterClear
    | MCPing
    | MCPong
    | MCAlert
    | MCMempool
    | MCReject
    | MCSendHeaders
    | MCOther ByteString
    deriving (Eq, Generic, NFData)


instance Show MessageCommand where
    showsPrec _ = shows . commandToString


instance Read MessageCommand where
    readPrec = do
        String str <- R.lexP
        return (stringToCommand (cs str))


instance Binary MessageCommand where
    get = go <$> Get.getByteString 12
      where
        go bs =
            let str = unpackCommand bs
             in stringToCommand str
    put = Put.putByteString . packCommand . commandToString


instance IsString MessageCommand where
    fromString str = stringToCommand (cs str)


-- | Read a 'MessageCommand' from its string representation.
stringToCommand :: ByteString -> MessageCommand
stringToCommand str = case str of
    "version" -> MCVersion
    "verack" -> MCVerAck
    "addr" -> MCAddr
    "inv" -> MCInv
    "getdata" -> MCGetData
    "notfound" -> MCNotFound
    "getblocks" -> MCGetBlocks
    "getheaders" -> MCGetHeaders
    "tx" -> MCTx
    "block" -> MCBlock
    "merkleblock" -> MCMerkleBlock
    "headers" -> MCHeaders
    "getaddr" -> MCGetAddr
    "filterload" -> MCFilterLoad
    "filteradd" -> MCFilterAdd
    "filterclear" -> MCFilterClear
    "ping" -> MCPing
    "pong" -> MCPong
    "alert" -> MCAlert
    "mempool" -> MCMempool
    "reject" -> MCReject
    "sendheaders" -> MCSendHeaders
    _ -> MCOther str


-- | Convert a 'MessageCommand' to its string representation.
commandToString :: MessageCommand -> ByteString
commandToString mc = case mc of
    MCVersion -> "version"
    MCVerAck -> "verack"
    MCAddr -> "addr"
    MCInv -> "inv"
    MCGetData -> "getdata"
    MCNotFound -> "notfound"
    MCGetBlocks -> "getblocks"
    MCGetHeaders -> "getheaders"
    MCTx -> "tx"
    MCBlock -> "block"
    MCMerkleBlock -> "merkleblock"
    MCHeaders -> "headers"
    MCGetAddr -> "getaddr"
    MCFilterLoad -> "filterload"
    MCFilterAdd -> "filteradd"
    MCFilterClear -> "filterclear"
    MCPing -> "ping"
    MCPong -> "pong"
    MCAlert -> "alert"
    MCMempool -> "mempool"
    MCReject -> "reject"
    MCSendHeaders -> "sendheaders"
    MCOther c -> c


-- | Pack a string 'MessageCommand' so that it is exactly 12-bytes long.
packCommand :: ByteString -> ByteString
packCommand s =
    BS.take 12 $
        s `mappend` C.replicate 12 '\NUL'


-- | Undo packing done by 'packCommand'.
unpackCommand :: ByteString -> ByteString
unpackCommand = BS.takeWhile (/= 0)


-- | Node offers no services.
nodeNone :: Word64
nodeNone = 0


-- | Services indicate node is a full node that can serve full blocks.
nodeNetwork :: Word64
nodeNetwork = 1


-- | Services indicate node allows to request 'UTXO' set.
nodeGetUTXO :: Word64
nodeGetUTXO = 1 `shiftL` 1


-- | Services indicate node accepts bloom filters.
nodeBloom :: Word64
nodeBloom = 1 `shiftL` 2


-- | Services indicate SegWit-capable node.
nodeWitness :: Word64
nodeWitness = 1 `shiftL` 3


-- | Services indicate Xtreme Thinblocks compatibility.
nodeXThin :: Word64
nodeXThin = 1 `shiftL` 4
