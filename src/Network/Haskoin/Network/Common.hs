{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Network.Common
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Common functions and data types related to peer-to-peer network.
-}
module Network.Haskoin.Network.Common
    ( -- * Network Data Types
      Addr(..)
    , NetworkAddressTime
    , Alert(..)
    , GetData(..)
    , Inv(..)
    , InvVector(..)
    , InvType(..)
    , NetworkAddress(..)
    , NotFound(..)
    , Ping(..)
    , Pong(..)
    , Reject(..)
    , RejectCode(..)
    , VarInt(..)
    , VarString(..)
    , Version(..)
    , MessageCommand(..)
      -- ** Useful Functions
    , reject
    , nodeNone
    , nodeNetwork
    , nodeGetUTXO
    , nodeBloom
    , nodeWitness
    , nodeXThin
    , commandToString
    , stringToCommand
    , putVarInt
    ) where

import           Control.Monad               (forM_, liftM2, replicateM, unless)
import           Data.Bits                   (shiftL)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.ByteString.Char8       as C (replicate)
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.Serialize              as S
import           Data.String
import           Data.String.Conversions     (cs)
import           Data.Word                   (Word32, Word64)
import           Network.Haskoin.Crypto.Hash
import           Network.Socket              (SockAddr (..))
import           Text.Read                   as R

-- | Network address with a timestamp.
type NetworkAddressTime = (Word32, NetworkAddress)

-- | Provides information about known nodes in the bitcoin network. An 'Addr'
-- type is sent inside a 'Message' as a response to a 'GetAddr' message.
newtype Addr =
    Addr { -- List of addresses of other nodes on the network with timestamps.
           addrList :: [NetworkAddressTime]
         }
    deriving (Eq, Show)

instance Serialize Addr where

    get = Addr <$> (repList =<< S.get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) action
        action             = liftM2 (,) getWord32le S.get

    put (Addr xs) = do
        putVarInt $ length xs
        forM_ xs $ \(a,b) -> putWord32le a >> put b

-- | Data type describing signed messages that can be sent between bitcoin
-- nodes to display important notifications to end users about the health of
-- the network.
data Alert =
    Alert {
          -- | Alert payload.
            alertPayload   :: !VarString
          -- | ECDSA signature of the payload
          , alertSignature :: !VarString
          } deriving (Eq, Show, Read)

instance Serialize Alert where
    get = Alert <$> S.get <*> S.get
    put (Alert p s) = put p >> put s

-- | The 'GetData' type is used to retrieve information on a specific object
-- ('Block' or 'Tx') identified by the objects hash. The payload of a 'GetData'
-- request is a list of 'InvVector' which represent all the hashes of objects
-- that a node wants. The response to a 'GetBlock' message will be either a
-- 'Block' or a 'Tx' message depending on the type of the object referenced by
-- the hash. Usually, 'GetData' messages are sent after a node receives an 'Inv'
-- message that contains unknown object hashes.
newtype GetData =
    GetData { -- | list of object hashes
              getDataList :: [InvVector]
            } deriving (Eq, Show)

instance Serialize GetData where

    get = GetData <$> (repList =<< S.get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) S.get

    put (GetData xs) = do
        putVarInt $ length xs
        forM_ xs put

-- | 'Inv' messages are used by nodes to advertise their knowledge of new
-- objects by publishing a list of hashes to a peer. 'Inv' messages can be sent
-- unsolicited or in response to a 'GetBlocks' message.
newtype Inv =
    Inv {
        -- | inventory
          invList :: [InvVector]
        } deriving (Eq, Show)

instance Serialize Inv where

    get = Inv <$> (repList =<< S.get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) S.get

    put (Inv xs) = do
        putVarInt $ length xs
        forM_ xs put

-- | Data type identifying the type of an inventory vector. SegWit types are
-- only used in 'GetData' messages, not 'Inv'.
data InvType
    = InvError -- ^ error
    | InvTx -- ^ transaction
    | InvBlock -- ^ block
    | InvMerkleBlock -- ^ filtered block
    | InvWitnessTx -- ^ segwit transaction
    | InvWitnessBlock -- ^ segwit block
    | InvWitnessMerkleBlock -- ^ segwit filtere block
    deriving (Eq, Show, Read)

instance Serialize InvType where
    get = go =<< getWord32le
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
                    | otherwise -> fail "bitcoinGet InvType: Invalid Type"
    put x =
        putWord32le $
        case x of
            InvError              -> 0
            InvTx                 -> 1
            InvBlock              -> 2
            InvMerkleBlock        -> 3
            InvWitnessTx          -> 1 `shiftL` 30 + 1
            InvWitnessBlock       -> 1 `shiftL` 30 + 2
            InvWitnessMerkleBlock -> 1 `shiftL` 30 + 3

-- | Invectory vectors represent hashes identifying objects such as a 'Block' or
-- a 'Tx'. They notify other peers about new data or data they have otherwise
-- requested.
data InvVector =
    InvVector {
                -- | type of object
                invType :: !InvType
                -- | 256-bit hash of object
              , invHash :: !Hash256
              } deriving (Eq, Show)

instance Serialize InvVector where
    get = InvVector <$> S.get <*> S.get
    put (InvVector t h) = put t >> put h

-- | Data type describing a bitcoin network address. Addresses are stored in
-- IPv6 format. IPv4 addresses are mapped to IPv6 using IPv4 mapped IPv6
-- addresses: <http://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses>.
data NetworkAddress =
    NetworkAddress { -- | bitmask of services available for this address
                     naServices :: !Word64
                     -- | address and port information
                   , naAddress  :: !SockAddr
                   } deriving (Eq, Show)

instance Serialize NetworkAddress where

    get = NetworkAddress <$> getWord64le
                         <*> getAddrPort
      where
        getAddrPort = do
            a <- getWord32be
            b <- getWord32be
            c <- getWord32be
            if a == 0x00000000 && b == 0x00000000 && c == 0x0000ffff
              then do
                d <- getWord32host
                p <- getWord16be
                return $ SockAddrInet (fromIntegral p) d
              else do
                d <- getWord32be
                p <- getWord16be
                return $ SockAddrInet6 (fromIntegral p) 0 (a,b,c,d) 0

    put (NetworkAddress s (SockAddrInet6 p _ (a,b,c,d) _)) = do
        putWord64le s
        putWord32be a
        putWord32be b
        putWord32be c
        putWord32be d
        putWord16be (fromIntegral p)

    put (NetworkAddress s (SockAddrInet p a)) = do
        putWord64le s
        putWord32be 0x00000000
        putWord32be 0x00000000
        putWord32be 0x0000ffff
        putWord32host a
        putWord16be (fromIntegral p)

    put _ = error "NetworkAddress can onle be IPv4 or IPv6"

-- | A 'NotFound' message is returned as a response to a 'GetData' message
-- whe one of the requested objects could not be retrieved. This could happen,
-- for example, if a tranasaction was requested and was not available in the
-- memory pool of the receiving node.
newtype NotFound =
    NotFound { -- | Inventory vectors related to this request
               notFoundList :: [InvVector]
             } deriving (Eq, Show)

instance Serialize NotFound where

    get = NotFound <$> (repList =<< S.get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) S.get

    put (NotFound xs) = do
        putVarInt $ length xs
        forM_ xs put

-- | A 'Ping' message is sent to bitcoin peers to check if a connection is still
-- open.
newtype Ping =
    Ping { -- | A random nonce used to identify the recipient of the ping
           -- request once a Pong response is received.
           pingNonce :: Word64
         } deriving (Eq, Show, Read)

-- | A Pong message is sent as a response to a ping message.
newtype Pong =
    Pong {
           -- | nonce from corresponding 'Ping'
           pongNonce :: Word64
         } deriving (Eq, Show, Read)

instance Serialize Ping where
    get = Ping <$> getWord64le
    put (Ping n) = putWord64le n

instance Serialize Pong where
    get = Pong <$> getWord64le
    put (Pong n) = putWord64le n

-- | The 'Reject' message is sent when messages are rejected by a peer.
data Reject =
    Reject {
             -- | type of message rejected
             rejectMessage :: !MessageCommand
             -- | rejection code
           , rejectCode    :: !RejectCode
             -- | text reason for rejection
           , rejectReason  :: !VarString
             -- | extra data such as block or tx hash
           , rejectData    :: !ByteString
           } deriving (Eq, Show, Read)

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
    deriving (Eq, Show, Read)

instance Serialize RejectCode where

    get = getWord8 >>= \code -> case code of
        0x01 -> return RejectMalformed
        0x10 -> return RejectInvalid
        0x11 -> return RejectObsolete
        0x12 -> return RejectDuplicate
        0x40 -> return RejectNonStandard
        0x41 -> return RejectDust
        0x42 -> return RejectInsufficientFee
        0x43 -> return RejectCheckpoint
        _    -> fail $ unwords
            [ "Reject get: Invalid code"
            , show code
            ]

    put code = putWord8 $ case code of
        RejectMalformed       -> 0x01
        RejectInvalid         -> 0x10
        RejectObsolete        -> 0x11
        RejectDuplicate       -> 0x12
        RejectNonStandard     -> 0x40
        RejectDust            -> 0x41
        RejectInsufficientFee -> 0x42
        RejectCheckpoint      -> 0x43

-- | Convenience function to build a 'Reject' message.
reject :: MessageCommand -> RejectCode -> ByteString -> Reject
reject cmd code reason =
    Reject cmd code (VarString reason) B.empty

instance Serialize Reject where

    get = S.get >>= \(VarString bs) -> case stringToCommand bs of
        Just cmd -> Reject cmd <$> S.get <*> S.get <*> maybeData
        _ -> fail $ unwords
            ["Reason get: Invalid message command" ,cs bs]
      where
        maybeData = isEmpty >>= \done ->
            if done then return B.empty else getByteString 32

    put (Reject cmd code reason dat) = do
        put $ VarString $ commandToString cmd
        put code
        put reason
        unless (B.null dat) $ putByteString dat

-- | Data type representing a variable-length integer. The 'VarInt' type
-- usually precedes an array or a string that can vary in length.
newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Show, Read)

instance Serialize VarInt where

    get = VarInt <$> ( getWord8 >>= go )
      where
        go 0xff = getWord64le
        go 0xfe = fromIntegral <$> getWord32le
        go 0xfd = fromIntegral <$> getWord16le
        go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0xfd =
            putWord8 $ fromIntegral x
        | x <= 0xffff = do
            putWord8 0xfd
            putWord16le $ fromIntegral x
        | x <= 0xffffffff = do
            putWord8 0xfe
            putWord32le $ fromIntegral x
        | otherwise = do
            putWord8 0xff
            putWord64le x

putVarInt :: Integral a => a -> Put
putVarInt = put . VarInt . fromIntegral

-- | Data type for serialization of variable-length strings.
newtype VarString = VarString { getVarString :: ByteString }
    deriving (Eq, Show, Read)

instance Serialize VarString where

    get = VarString <$> (readBS =<< S.get)
      where
        readBS (VarInt len) = getByteString (fromIntegral len)

    put (VarString bs) = do
        putVarInt $ B.length bs
        putByteString bs

-- | When a bitcoin node creates an outgoing connection to another node,
-- the first message it will send is a 'Version' message. The other node
-- will similarly respond with it's own 'Version' message.
data Version =
    Version {
              -- | protocol version
              version     :: !Word32
              -- | features supported by this connection
            , services    :: !Word64
              -- | unix timestamp
            , timestamp   :: !Word64
              -- | network address of remote node
            , addrRecv    :: !NetworkAddress
              -- | network address of sending node
            , addrSend    :: !NetworkAddress
              -- | random nonce to detect connection to self
            , verNonce    :: !Word64
              -- | user agent string
            , userAgent   :: !VarString
              -- | height of the last block in sending node
            , startHeight :: !Word32
              -- | relay transactions flag (BIP-37)
            , relay       :: !Bool
            } deriving (Eq, Show)

instance Serialize Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> S.get
                  <*> S.get
                  <*> getWord64le
                  <*> S.get
                  <*> getWord32le
                  <*> (go =<< isEmpty)
      where
        go True  = return True
        go False = getBool

    put (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        put         ua
        putWord32le sh
        putBool     r

-- | 0x00 is 'False', anything else is 'True'.
getBool :: Get Bool
getBool = go =<< getWord8
  where
    go 0 = return False
    go _ = return True

putBool :: Bool -> Put
putBool True  = putWord8 1
putBool False = putWord8 0

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
    deriving (Eq)

instance Show MessageCommand where
    showsPrec _ = shows . commandToString

instance Read MessageCommand where
    readPrec = do
        String str <- lexP
        maybe pfail return (stringToCommand (cs str))

instance Serialize MessageCommand where
    get = go =<< getByteString 12
      where
        go bs =
            let str = unpackCommand bs
            in case stringToCommand str of
                Just cmd -> return cmd
                Nothing  -> fail $ cs $
                    "get MessageCommand: Invalid command: " <> str
    put mc = putByteString $ packCommand $ commandToString mc

instance IsString MessageCommand where
    fromString str =
        fromMaybe
            (error ("Could not recognize message command " <> str))
            (stringToCommand (cs str))

-- | Read a 'MessageCommand' from its string representation.
stringToCommand :: ByteString -> Maybe MessageCommand
stringToCommand str = case str of
    "version"     -> Just MCVersion
    "verack"      -> Just MCVerAck
    "addr"        -> Just MCAddr
    "inv"         -> Just MCInv
    "getdata"     -> Just MCGetData
    "notfound"    -> Just MCNotFound
    "getblocks"   -> Just MCGetBlocks
    "getheaders"  -> Just MCGetHeaders
    "tx"          -> Just MCTx
    "block"       -> Just MCBlock
    "merkleblock" -> Just MCMerkleBlock
    "headers"     -> Just MCHeaders
    "getaddr"     -> Just MCGetAddr
    "filterload"  -> Just MCFilterLoad
    "filteradd"   -> Just MCFilterAdd
    "filterclear" -> Just MCFilterClear
    "ping"        -> Just MCPing
    "pong"        -> Just MCPong
    "alert"       -> Just MCAlert
    "mempool"     -> Just MCMempool
    "reject"      -> Just MCReject
    "sendheaders" -> Just MCSendHeaders
    _             -> Nothing

-- | Convert a 'MessageCommand' to its string representation.
commandToString :: MessageCommand -> ByteString
commandToString mc = case mc of
    MCVersion     -> "version"
    MCVerAck      -> "verack"
    MCAddr        -> "addr"
    MCInv         -> "inv"
    MCGetData     -> "getdata"
    MCNotFound    -> "notfound"
    MCGetBlocks   -> "getblocks"
    MCGetHeaders  -> "getheaders"
    MCTx          -> "tx"
    MCBlock       -> "block"
    MCMerkleBlock -> "merkleblock"
    MCHeaders     -> "headers"
    MCGetAddr     -> "getaddr"
    MCFilterLoad  -> "filterload"
    MCFilterAdd   -> "filteradd"
    MCFilterClear -> "filterclear"
    MCPing        -> "ping"
    MCPong        -> "pong"
    MCAlert       -> "alert"
    MCMempool     -> "mempool"
    MCReject      -> "reject"
    MCSendHeaders -> "sendheaders"

-- | Pack a string 'MessageCommand' so that it is exactly 12-bytes long.
packCommand :: ByteString -> ByteString
packCommand s = B.take 12 $
    s `mappend` C.replicate 12 '\NUL'

-- | Undo packing done by 'packCommand'.
unpackCommand :: ByteString -> ByteString
unpackCommand = B.takeWhile (/= 0)

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
