module Network.Haskoin.Protocol.Types 
( Addr(..)
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
, reject
, VarInt(..)
, VarString(..)
, Version(..)
, MessageCommand(..)
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (replicateM, liftM2, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32, Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( Get
    , getWord8
    , getWord16le
    , getWord16be
    , getWord32be
    , getWord32host
    , getWord32le
    , getWord64le
    , getByteString
    , isEmpty
    )
import Data.Binary.Put 
    ( Put
    , putWord8
    , putWord16le
    , putWord16be
    , putWord32be
    , putWord32host
    , putWord32le
    , putWord64le
    , putByteString
    )
import qualified Data.ByteString as BS 
    ( ByteString
    , length
    , takeWhile
    )
import Network.Socket
    ( SockAddr (SockAddrInet, SockAddrInet6)
    , PortNumber (PortNum)
    )

import Network.Haskoin.Util 
import Network.Haskoin.Crypto.BigWord

-- | Network address with a timestamp
type NetworkAddressTime = (Word32, NetworkAddress)

-- | Provides information on known nodes in the bitcoin network. An 'Addr'
-- type is sent inside a 'Message' as a response to a 'GetAddr' message.
data Addr = 
    Addr { 
           -- List of addresses of other nodes on the network with timestamps.
           addrList :: ![NetworkAddressTime] 
         } 
    deriving (Eq, Show)

instance Binary Addr where

    get = Addr <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) action
        action             = liftM2 (,) getWord32le get 

    put (Addr xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> (putWord32le a) >> (put b)

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

instance NFData Alert where
    rnf (Alert p s) = rnf p `seq` rnf s

instance Binary Alert where
    get = Alert <$> get <*> get
    put (Alert p s) = put p >> put s

-- | The 'GetData' type is used to retrieve information on a specific object
-- ('Block' or 'Tx') identified by the objects hash. The payload of a 'GetData'
-- request is a list of 'InvVector' which represent all the hashes for which a
-- node wants to request information. The response to a 'GetBlock' message
-- wille be either a 'Block' or a 'Tx' message depending on the type of the
-- object referenced by the hash. Usually, 'GetData' messages are sent after a
-- node receives an 'Inv' message to obtain information on unknown object
-- hashes. 
data GetData = 
    GetData {
              -- | List of object hashes 
              getDataList :: ![InvVector] 
            } deriving (Eq, Show, Read)

instance NFData GetData where
    rnf (GetData l) = rnf l

instance Binary GetData where

    get = GetData <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetData xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

-- | 'Inv' messages are used by nodes to advertise their knowledge of new
-- objects by publishing a list of hashes. 'Inv' messages can be sent
-- unsolicited or in response to a 'GetBlocks' message.
data Inv = 
    Inv { 
        -- | Inventory vectors
          invList :: ![InvVector] 
        } deriving (Eq, Show, Read)

instance NFData Inv where
    rnf (Inv l) = rnf l

instance Binary Inv where

    get = Inv <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (Inv xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

-- | Data type identifying the type of an inventory vector. 
data InvType 
    = InvError -- ^ Error. Data containing this type can be ignored.
    | InvTx    -- ^ InvVector hash is related to a transaction 
    | InvBlock -- ^ InvVector hash is related to a block
    | InvMerkleBlock -- ^ InvVector has is related to a merkle block
    deriving (Eq, Show, Read)

instance NFData InvType

instance Binary InvType where

    get = go =<< getWord32le
      where 
        go x = case x of
            0 -> return InvError
            1 -> return InvTx
            2 -> return InvBlock
            3 -> return InvMerkleBlock
            _ -> fail "bitcoinGet InvType: Invalid Type"

    put x = putWord32le $ case x of
                InvError       -> 0
                InvTx          -> 1
                InvBlock       -> 2
                InvMerkleBlock -> 3

-- | Invectory vectors represent hashes identifying objects such as a 'Block'
-- or a 'Tx'. They are sent inside messages to notify other peers about 
-- new data or data they have requested.
data InvVector = 
    InvVector {
                -- | Type of the object referenced by this inventory vector
                invType :: !InvType
                -- | Hash of the object referenced by this inventory vector
              , invHash :: !Word256
              } deriving (Eq, Show, Read)

instance NFData InvVector where
    rnf (InvVector t h) = rnf t `seq` rnf h

instance Binary InvVector where
    get = InvVector <$> get <*> get
    put (InvVector t h) = put t >> put h

-- | Data type describing a bitcoin network address. Addresses are stored in
-- IPv6. IPv4 addresses are mapped to IPv6 using IPv4 mapped IPv6 addresses:
-- <http://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses>. Sometimes,
-- timestamps are sent together with the 'NetworkAddress' such as in the 'Addr'
-- data type.
data NetworkAddress = 
    NetworkAddress {
                   -- | Bitmask of services available for this address
                     naServices :: !Word64
                   -- | IPv6 address and port
                   , naAddress  :: !SockAddr
                   } deriving (Eq, Show)

instance Binary NetworkAddress where

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
                return $ SockAddrInet (PortNum p) d
              else do
                d <- getWord32be
                p <- getWord16be
                return $ SockAddrInet6 (PortNum p) 0 (a,b,c,d) 0

    put (NetworkAddress s (SockAddrInet6 (PortNum p) _ (a,b,c,d) _)) = do
        putWord64le s
        putWord32be a
        putWord32be b
        putWord32be c
        putWord32be d
        putWord16be p

    put (NetworkAddress s (SockAddrInet (PortNum p) a)) = do
        putWord64le s
        putWord32be 0x00000000
        putWord32be 0x00000000
        putWord32be 0x0000ffff
        putWord32host a
        putWord16be p

    put _ = error "NetworkAddress can onle be IPv4 or IPv6"

-- | A 'NotFound' message is returned as a response to a 'GetData' message
-- whe one of the requested objects could not be retrieved. This could happen,
-- for example, if a tranasaction was requested and was not available in the
-- memory pool of the receiving node.
data NotFound = 
    NotFound {
             -- | Inventory vectors related to this request
               notFoundList :: ![InvVector] 
             } deriving (Eq, Show, Read)

instance NFData NotFound where
    rnf (NotFound l) = rnf l

instance Binary NotFound where

    get = NotFound <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (NotFound xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

-- | A Ping message is sent to bitcoin peers to check if a TCP\/IP connection
-- is still valid.
newtype Ping = 
    Ping { 
           -- | A random nonce used to identify the recipient of the ping
           -- request once a Pong response is received.  
           pingNonce :: Word64 
         } deriving (Eq, Show, Read)

instance NFData Ping where
    rnf (Ping n) = rnf n

-- | A Pong message is sent as a response to a ping message.
newtype Pong = 
    Pong { 
           -- | When responding to a Ping request, the nonce from the Ping
           -- is copied in the Pong response.
           pongNonce :: Word64 
         } deriving (Eq, Show, Read)

instance NFData Pong where
    rnf (Pong n) = rnf n

instance Binary Ping where
    get = Ping <$> getWord64le
    put (Ping n) = putWord64le n

instance Binary Pong where
    get = Pong <$> getWord64le
    put (Pong n) = putWord64le n

-- | The reject message is sent when messages are rejected by a peer.
data Reject =
    Reject { 
            -- | Type of message rejected
             rejectMessage :: !MessageCommand
             -- | Code related to the rejected message
           , rejectCode    :: !RejectCode
             -- | Text version of rejected reason
           , rejectReason  :: !VarString
           } deriving (Eq, Show, Read)


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

instance Binary RejectCode where

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

-- | Convenience function to build a Reject message
reject :: MessageCommand -> RejectCode -> String -> Reject
reject cmd code reason = Reject cmd code (VarString $ stringToBS reason)

instance Binary Reject where

    get = get >>= \(VarString bs) -> case stringToCommand $ bsToString bs of
        Just cmd -> Reject cmd <$> get <*> get
        _        -> fail $ unwords $
            [ "Reason get: Invalid message command"
            , bsToString bs
            ]

    put (Reject cmd code reason) = do
        put $ VarString $ stringToBS $ commandToString cmd
        put code
        put reason

-- | Data type representing a variable length integer. The 'VarInt' type
-- usually precedes an array or a string that can vary in length. 
newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Show, Read)

instance NFData VarInt where
    rnf (VarInt w) = rnf w

instance Binary VarInt where

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

-- | Data type for variable length strings. Variable length strings are
-- serialized as a 'VarInt' followed by a bytestring.
newtype VarString = VarString { getVarString :: BS.ByteString }
    deriving (Eq, Show, Read)

instance NFData VarString where
    rnf (VarString s) = rnf s

instance Binary VarString where

    get = VarString <$> (readBS =<< get)
      where 
        readBS (VarInt len) = getByteString (fromIntegral len)

    put (VarString bs) = do
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs

-- | When a bitcoin node creates an outgoing connection to another node,
-- the first message it will send is a 'Version' message. The other node
-- will similarly respond with it's own 'Version' message.
data Version = 
    Version {
              -- | Protocol version being used by the node.
              version     :: !Word32
              -- | Bitmask of features to enable for this connection.
            , services    :: !Word64
              -- | UNIX timestamp
            , timestamp   :: !Word64
              -- | Network address of the node receiving this message.
            , addrRecv    :: !NetworkAddress
              -- | Network address of the node sending this message.
            , addrSend    :: !NetworkAddress
              -- | Randomly generated identifying sent with every version
              -- message. This nonce is used to detect connection to self.
            , verNonce    :: !Word64
              -- | User agent
            , userAgent   :: !VarString
              -- | The height of the last block received by the sending node.
            , startHeight :: !Word32
              -- | Wether the remote peer should announce relaying transactions
              -- or not. This feature is enabled since version >= 70001. See
              -- BIP37 for more details.
            , relay       :: !Bool
            } deriving (Eq, Show)

instance Binary Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> get
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
    | MCReject
    deriving (Eq, Show, Read)

instance NFData MessageCommand

instance Binary MessageCommand where
    
    get = go =<< getByteString 12
      where 
        go bs = case stringToCommand $ unpackCommand bs of
            Just cmd -> return cmd
            Nothing  -> fail "get MessageCommand : Invalid command"

    put mc = putByteString $ packCommand $ commandToString mc


stringToCommand :: String -> Maybe MessageCommand
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
    "reject"      -> Just MCReject
    _             -> Nothing

commandToString :: MessageCommand -> String
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
    MCReject      -> "reject"

packCommand :: String -> BS.ByteString
packCommand s = stringToBS $ take 12 $ s ++ repeat '\NUL'

unpackCommand :: BS.ByteString -> String
unpackCommand bs = bsToString $ BS.takeWhile (/= 0) bs

