module Network.Haskoin.Protocol.Types 
( Addr(..)
, NetworkAddressTime 
, Alert(..)
, Block(..)
, BlockHeader(..) 
, BloomFlags(..)
, BloomFilter(..)
, FilterLoad(..)
, FilterAdd(..)
, GetBlocks(..) 
, BlockLocator
, GetData(..)
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount
, Inv(..)
, InvVector(..) 
, InvType(..)
, MerkleBlock(..)
, NetworkAddress(..)
, NotFound(..)
, Ping(..)
, Pong(..)
, Reject(..)
, RejectCode(..)
, reject
, Tx(..) 
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, CoinbaseTx(..)
, VarInt(..)
, VarString(..)
, Version(..)
, MessageCommand(..)
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2, replicateM, forM_, unless)
import Control.Applicative ((<$>),(<*>))

import Data.Bits (testBit, setBit)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( Get
    , getWord8
    , getWord16le
    , getWord16be
    , getWord32le
    , getWord64le
    , getWord64be
    , getByteString
    , isEmpty
    )
import Data.Binary.Put 
    ( Put
    , putWord8
    , putWord16le
    , putWord16be
    , putWord32le
    , putWord64le
    , putWord64be
    , putByteString
    )
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S (Seq, fromList, length)
import qualified Data.ByteString as BS 
    ( ByteString
    , length
    , takeWhile
    )

import Network.Haskoin.Util 
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Types.BTC

-- | Network address with a timestamp
type NetworkAddressTime = (Word32, NetworkAddress)

-- | Provides information on known nodes in the bitcoin network. An 'Addr'
-- type is sent inside a 'Message' as a response to a 'GetAddr' message.
data Addr = Addr { 
           -- List of addresses of other nodes on the network with timestamps.
           addrList :: ![NetworkAddressTime] 
         } 
    deriving (Eq, Show, Read)

instance NFData Addr where
    rnf (Addr as) = rnf as

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

-- | Data type describing a block in the bitcoin protocol. Blocks are sent in
-- response to 'GetData' messages that are requesting information from a
-- block hash.
data Block = 
    Block {
            -- | Header information for this block.
            blockHeader     :: !BlockHeader
            -- | Coinbase transaction of this block.
          , blockCoinbaseTx :: !CoinbaseTx
            -- | List of transactions pertaining to this block.
          , blockTxns       :: ![Tx]
          } deriving (Eq, Show, Read)

instance NFData Block where
    rnf (Block h c ts) = rnf h `seq` rnf c `seq` rnf ts

instance Binary Block where

    get = do
        header     <- get
        (VarInt c) <- get
        cb         <- get
        txs        <- replicateM (fromIntegral (c-1)) get
        return $ Block header cb txs

    put (Block h cb txs) = do
        put h
        put $ VarInt $ fromIntegral $ (length txs) + 1
        put cb
        forM_ txs put

-- | Data type recording information on a 'Block'. The hash of a block is
-- defined as the hash of this data structure. The block mining process
-- involves finding a partial hash collision by varying the nonce in the
-- 'BlockHeader' and/or additional randomness in the 'CoinbaseTx' of this
-- 'Block'. Variations in the 'CoinbaseTx' will result in different merkle 
-- roots in the 'BlockHeader'.
data BlockHeader = 
    BlockHeader {
                  -- | Block version information, based on the version of the
                  -- software creating this block.
                  blockVersion   :: !Word32
                  -- | Hash of the previous block (parent) referenced by this
                  -- block.
                , prevBlock      :: !BlockHash
                  -- | Root of the merkle tree of all transactions pertaining
                  -- to this block.
                , merkleRoot     :: !Word256
                  -- | Unix timestamp recording when this block was created
                , blockTimestamp :: !Word32
                  -- | The difficulty target being used for this block
                , blockBits      :: !Word32
                  -- | A random nonce used to generate this block. Additional
                  -- randomness is included in the coinbase transaction of
                  -- this block.
                , bhNonce        :: !Word32
                } deriving (Eq, Show, Read)

instance NFData BlockHeader where
    rnf (BlockHeader v p m t b n) =
        rnf v `seq` rnf p `seq` rnf m `seq` rnf t `seq` rnf b `seq` rnf n

instance Binary BlockHeader where

    get = BlockHeader <$> getWord32le
                      <*> get
                      <*> get
                      <*> getWord32le
                      <*> getWord32le
                      <*> getWord32le

    put (BlockHeader v p m bt bb n) = do
        putWord32le v
        put         p
        put         m
        putWord32le bt
        putWord32le bb
        putWord32le n 

-- | The bloom flags are used to tell the remote peer how to auto-update
-- the provided bloom filter. 
data BloomFlags
    = BloomUpdateNone         -- ^ Never update
    | BloomUpdateAll          -- ^ Auto-update on all outputs
    | BloomUpdateP2PubKeyOnly 
    -- ^ Only auto-update on outputs that are pay-to-pubkey or pay-to-multisig.
    -- This is the default setting.
    deriving (Eq, Show, Read)

instance NFData BloomFlags

instance Binary BloomFlags where
    get = go =<< getWord8
      where
        go 0 = return BloomUpdateNone
        go 1 = return BloomUpdateAll
        go 2 = return BloomUpdateP2PubKeyOnly
        go _ = fail "BloomFlags get: Invalid bloom flag"

    put f = putWord8 $ case f of
        BloomUpdateNone         -> 0
        BloomUpdateAll          -> 1
        BloomUpdateP2PubKeyOnly -> 2
            
-- | A bloom filter is a probabilistic data structure that SPV clients send to
-- other peers to filter the set of transactions received from them. Bloom
-- filters are probabilistic and have a false positive rate. Some transactions
-- that pass the filter may not be relevant to the receiving peer. By
-- controlling the false positive rate, SPV nodes can trade off bandwidth
-- versus privacy.
data BloomFilter = BloomFilter
    { bloomData      :: !(S.Seq Word8)
    -- ^ Bloom filter data
    , bloomFull      :: !Bool
    -- ^ Flag indicating if the filter is full ('bloomData' is all 0x00)
    , bloomEmpty     :: !Bool
    -- ^ Flag indicating if the filter is empty ('bloomData' is all 0xff)
    , bloomHashFuncs :: !Word32
    -- ^ Number of hash functions for this filter
    , bloomTweak     :: !Word32
    -- ^ Hash function random nonce
    , bloomFlags     :: !BloomFlags
    -- ^ Bloom filter auto-update flags
    }
    deriving (Eq, Show, Read)

instance NFData BloomFilter where
    rnf (BloomFilter d f e h t g) =
        rnf d `seq` rnf f `seq` rnf e `seq` rnf h `seq` rnf t `seq` rnf g

instance Binary BloomFilter where

    get = BloomFilter <$> (S.fromList <$> (readDat =<< get))
                      <*> (return False) <*> (return False)
                      <*> getWord32le <*> getWord32le
                      <*> get
      where
        readDat (VarInt len) = replicateM (fromIntegral len) getWord8   

    put (BloomFilter dat _ _ hashFuncs tweak flags) = do
        put $ VarInt $ fromIntegral $ S.length dat
        forM_ (F.toList dat) putWord8
        putWord32le hashFuncs
        putWord32le tweak
        put flags

-- | Set a new bloom filter on the peer connection.
newtype FilterLoad = FilterLoad { getBloomFilter :: BloomFilter }
    deriving (Eq, Show, Read)

instance NFData FilterLoad where
    rnf (FilterLoad f) = rnf f

instance Binary FilterLoad where
    get = FilterLoad <$> get
    put (FilterLoad f) = put f

-- | Add the given data element to the connections current filter without
-- requiring a completely new one to be set.
newtype FilterAdd = FilterAdd { getFilterData :: BS.ByteString }
    deriving (Eq, Show, Read)

instance NFData FilterAdd where
    rnf (FilterAdd f) = rnf f

instance Binary FilterAdd where
    get = do
        (VarInt len) <- get
        dat <- getByteString $ fromIntegral len
        return $ FilterAdd dat

    put (FilterAdd bs) = do
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs

type BlockLocator = [BlockHash]

-- | Data type representing a GetBlocks message request. It is used in the
-- bitcoin protocol to retrieve blocks from a peer by providing it a
-- 'BlockLocator' object. The 'BlockLocator' is a sparse list of block hashes
-- from the caller node with the purpose of informing the receiving node
-- about the state of the caller's blockchain. The receiver node will detect
-- a wrong branch in the caller's main chain and send the caller appropriate 
-- 'Blocks'. The response to a 'GetBlocks' message is an 'Inv' message
-- containing the list of block hashes pertaining to the request. 
data GetBlocks = 
    GetBlocks {
                -- | The protocol version
                getBlocksVersion  :: !Word32
                -- | Block locator object. It is a list of block hashes from the
                -- most recent block back to the genesis block. The list is
                -- dense at first and sparse towards the end.
              , getBlocksLocator  :: !BlockLocator
                -- | Hash of the last desired block. If set to zero, the
                -- maximum number of block hashes is returned (500).
              , getBlocksHashStop :: !BlockHash
              } deriving (Eq, Show, Read)

instance NFData GetBlocks where
    rnf (GetBlocks v l h) = rnf v `seq` rnf l `seq` rnf h

instance Binary GetBlocks where

    get = GetBlocks <$> getWord32le
                    <*> (repList =<< get)
                    <*> get
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetBlocks v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

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

-- | Similar to the 'GetBlocks' message type but for retrieving block headers
-- only. The response to a 'GetHeaders' request is a 'Headers' message
-- containing a list of block headers pertaining to the request. A maximum of
-- 2000 block headers can be returned. 'GetHeaders' is used by thin (SPV)
-- clients to exclude block contents when synchronizing the blockchain.
data GetHeaders = 
    GetHeaders {
                 -- | The protocol version
                 getHeadersVersion  :: !Word32
                 -- | Block locator object. It is a list of block hashes from
                 -- the most recent block back to the Genesis block. The list
                 -- is dense at first and sparse towards the end.
               , getHeadersBL       :: !BlockLocator
                 -- | Hash of the last desired block header. When set to zero,
                 -- the maximum number of block headers is returned (2000)
               , getHeadersHashStop :: !BlockHash
               } deriving (Eq, Show, Read)

instance NFData GetHeaders where
    rnf (GetHeaders v l h) = rnf v `seq` rnf l `seq` rnf h

instance Binary GetHeaders where

    get = GetHeaders <$> getWord32le
                     <*> (repList =<< get)
                     <*> get
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetHeaders v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

-- | 'BlockHeader' type with a transaction count as 'VarInt'
type BlockHeaderCount = (BlockHeader, VarInt)

-- | The 'Headers' type is used to return a list of block headers in
-- response to a 'GetHeaders' message.
data Headers = 
    Headers { 
              -- | List of block headers with respective transaction counts
              headersList :: ![BlockHeaderCount] 
            } 
    deriving (Eq, Show, Read)

instance NFData Headers where
    rnf (Headers l) = rnf l

instance Binary Headers where

    get = Headers <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) action
        action = liftM2 (,) get get

    put (Headers xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> put a >> put b

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

data MerkleBlock = 
    MerkleBlock {
                -- | Header information for this merkle block.
                  merkleHeader :: !BlockHeader
                -- | Number of transactions in the block (including
                -- unmatched transactions).
                , merkleTotalTxns :: !Word32
                  -- | Hashes in depth-first order. They are used to rebuild a
                  -- partial merkle tree.
                , mHashes :: ![Word256]
                  -- | Flag bits, packed per 8 in a byte. Least significant bit
                  -- first. Flag bits are used to rebuild a partial merkle
                  -- tree.
                , mFlags :: ![Bool]
                } deriving (Eq, Show, Read)

instance NFData MerkleBlock where
    rnf (MerkleBlock m t h f) = rnf m `seq` rnf t `seq` rnf h `seq` rnf f

instance Binary MerkleBlock where

    get = do
        header <- get
        ntx    <- getWord32le
        (VarInt matchLen) <- get
        hashes <- replicateM (fromIntegral matchLen) get
        (VarInt flagLen)  <- get
        ws <- replicateM (fromIntegral flagLen) getWord8
        return $ MerkleBlock header ntx hashes (decodeMerkleFlags ws)

    put (MerkleBlock h ntx hashes flags) = do
        put h
        putWord32le ntx
        put $ VarInt $ fromIntegral $ length hashes
        forM_ hashes put
        let ws = encodeMerkleFlags flags
        put $ VarInt $ fromIntegral $ length ws
        forM_ ws putWord8

decodeMerkleFlags :: [Word8] -> [Bool]
decodeMerkleFlags ws = 
    [ b | p <- [0..(length ws)*8-1]
    , b <- [testBit (ws !! (p `div` 8)) (p `mod` 8)]
    ]

encodeMerkleFlags :: [Bool] -> [Word8]
encodeMerkleFlags bs = map boolsToWord8 $ splitIn 8 bs
    
splitIn :: Int -> [a] -> [[a]]
splitIn _ [] = []
splitIn c xs = take c xs : (splitIn c $ drop c xs)
 
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs = foldl setBit 0 (map snd $ filter fst $ zip xs [0..7])

-- | Data type describing a bitcoin network address. Addresses are stored in
-- IPv6. IPv4 addresses are mapped to IPv6 using IPv4 mapped IPv6 addresses:
-- <http://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses>. Sometimes,
-- timestamps are sent together with the 'NetworkAddress' such as in the 'Addr'
-- data type.
data NetworkAddress = 
    NetworkAddress {
                   -- | Bitmask of services available for this address
                     naServices :: !Word64
                   -- | IPv6 address serialized as big endian
                   , naAddress  :: !(Word64, Word64)
                   -- | Port number serialized as big endian
                   , naPort     :: !Word16
                   } deriving (Eq, Show, Read)

instance NFData NetworkAddress where
    rnf (NetworkAddress s a p) = rnf s `seq` rnf a `seq` rnf p

instance Binary NetworkAddress where

    get = NetworkAddress <$> getWord64le
                         <*> (liftM2 (,) getWord64be getWord64be)
                         <*> getWord16be

    put (NetworkAddress s (al,ar) p) = do
        putWord64le s
        putWord64be al
        putWord64be ar
        putWord16be p

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

-- | Data type representing a bitcoin transaction
data Tx = 
    Tx { 
         -- | Transaction data format version
         txVersion  :: !Word32
         -- | List of transaction inputs
       , txIn       :: ![TxIn]
         -- | List of transaction outputs
       , txOut      :: ![TxOut]
         -- | The block number of timestamp at which this transaction is locked
       , txLockTime :: !Word32
       } deriving (Eq, Show, Read)

instance NFData Tx where
    rnf (Tx v i o l) = rnf v `seq` rnf i `seq` rnf o `seq` rnf l

instance Binary Tx where

    get = Tx <$> getWord32le
             <*> (replicateList =<< get)
             <*> (replicateList =<< get)
             <*> getWord32le
      where 
        replicateList (VarInt c) = replicateM (fromIntegral c) get

    put (Tx v is os l) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length is
        forM_ is put
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le l

-- | Data type representing the coinbase transaction of a 'Block'. Coinbase
-- transactions are special types of transactions which are created by miners
-- when they find a new block. Coinbase transactions have no inputs. They have
-- outputs sending the newly generated bitcoins together with all the block's
-- fees to a bitcoin address (usually the miners address). Data can be embedded
-- in a Coinbase transaction which can be chosen by the miner of a block. This
-- data also typically contains some randomness which is used, together with
-- the nonce, to find a partial hash collision on the block's hash.
data CoinbaseTx = 
    CoinbaseTx { 
                 -- | Transaction data format version.
                 cbVersion    :: !Word32
                 -- | Previous outpoint. This is ignored for
                 -- coinbase transactions but preserved for computing
                 -- the correct txid.
               , cbPrevOutput :: !OutPoint
                 -- | Data embedded inside the coinbase transaction.
               , cbData       :: !BS.ByteString
                 -- | Transaction sequence number. This is ignored for
                 -- coinbase transactions but preserved for computing
                 -- the correct txid.
               , cbInSequence :: !Word32
                 -- | List of transaction outputs.
               , cbOut        :: ![TxOut]
                 -- | The block number of timestamp at which this 
                 -- transaction is locked.
               , cbLockTime   :: !Word32
               } deriving (Eq, Show, Read)

instance NFData CoinbaseTx where
    rnf (CoinbaseTx v p d i o l) =
        rnf v `seq` rnf p `seq` rnf d `seq` rnf i `seq` rnf o `seq` rnf l

instance Binary CoinbaseTx where

    get = do
        v <- getWord32le
        (VarInt len) <- get
        unless (len == 1) $ fail "CoinbaseTx get: Input size is not 1"
        op <- get
        (VarInt cbLen) <- get
        cb <- getByteString (fromIntegral cbLen)
        sq <- getWord32le
        (VarInt oLen) <- get
        os <- replicateM (fromIntegral oLen) get
        lt <- getWord32le
        return $ CoinbaseTx v op cb sq os lt

    put (CoinbaseTx v op cb sq os lt) = do
        putWord32le v
        put $ VarInt 1
        put op
        put $ VarInt $ fromIntegral $ BS.length cb
        putByteString cb 
        putWord32le sq
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le lt

-- | Data type representing a transaction input.
data TxIn = 
    TxIn { 
           -- | Reference the previous transaction output (hash + position)
           prevOutput   :: !OutPoint
           -- | Script providing the requirements of the previous transaction
           -- output to spend those coins.
         , scriptInput  :: !BS.ByteString
           -- | Transaction version as defined by the sender of the
           -- transaction. The intended use is for replacing transactions with
           -- new information before the transaction is included in a block.
         , txInSequence :: !Word32
         } deriving (Eq, Show, Read)

instance NFData TxIn where
    rnf (TxIn p i s) = rnf p `seq` rnf i `seq` rnf s

instance Binary TxIn where

    get = 
        TxIn <$> get <*> (readBS =<< get) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len

    put (TxIn o s q) = do
        put o 
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s
        putWord32le q

-- | Data type representing a transaction output.
data TxOut = 
    TxOut { 
            -- | Transaction output value.
            outValue     :: !BTC
            -- | Script specifying the conditions to spend this output.
          , scriptOutput :: !BS.ByteString
          } deriving (Eq, Show, Read)

instance NFData TxOut where
    rnf (TxOut v o) = rnf v `seq` rnf o

instance Binary TxOut where

    get = do
        val <- get
        (VarInt len) <- get
        TxOut val <$> (getByteString $ fromIntegral len)

    put (TxOut o s) = do
        put o 
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s

-- | The OutPoint is used inside a transaction input to reference the previous
-- transaction output that it is spending.
data OutPoint = 
    OutPoint { 
               -- | The hash of the referenced transaction.
               outPointHash  :: !TxHash
               -- | The position of the specific output in the transaction.
               -- The first output position is 0.
             , outPointIndex :: !Word32
             } deriving (Read, Show, Eq)

instance NFData OutPoint where
    rnf (OutPoint h i) = rnf h `seq` rnf i

instance Binary OutPoint where
    get = do
        (h,i) <- liftM2 (,) get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

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
            } deriving (Eq, Show, Read)

instance NFData Version where
    rnf (Version ver ser ts ar as vn ua sh re) =
        rnf ver `seq` rnf ser `seq` rnf ts `seq` rnf ar `seq`
        rnf as `seq` rnf vn `seq` rnf ua `seq` rnf sh `seq` rnf re

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

