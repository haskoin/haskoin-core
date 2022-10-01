{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Common data types and functions to handle blocks from the block chain.
module Bitcoin.Block.Common (
    -- * Blocks
    Block (..),
    BlockHeight,
    Timestamp,
    BlockHeader (..),
    headerHash,
    BlockLocator,
    GetBlocks (..),
    GetHeaders (..),
    BlockHeaderCount,
    BlockHash (..),
    blockHashToHex,
    hexToBlockHash,
    Headers (..),
    decodeCompact,
    encodeCompact,
) where

import Control.DeepSeq
import Control.Monad (forM_, liftM2, mzero, replicateM, (<=<))
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    toJSON,
    withObject,
    withText,
    (.:),
    (.=),
 )
import Data.Aeson.Encoding (pairs, unsafeToEncoding)
import Data.Binary (Binary (..))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.ByteString.Builder (char7)
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (
    MonadGet,
    getWord32le,
    runGetL,
    runGetS,
 )
import Data.Bytes.Put (
    MonadPut,
    putWord32le,
    runPutL,
    runPutS,
 )
import Data.Bytes.Serial (Serial (..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..))
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Bitcoin.Crypto.Hash
import Bitcoin.Network.Common
import Bitcoin.Transaction.Common
import Bitcoin.Util
import qualified Text.Read as R


-- | Height of a block in the block chain, starting at 0 for Genesis.
type BlockHeight = Word32


-- | Block timestamp as Unix time (seconds since 1970-01-01 00:00 UTC).
type Timestamp = Word32


-- | Block header and transactions.
data Block = Block
    { blockHeader :: !BlockHeader
    , blockTxns :: ![Tx]
    }
    deriving (Eq, Show, Read, Generic, Hashable, NFData)


instance Serial Block where
    deserialize = do
        header <- deserialize
        (VarInt c) <- deserialize
        txs <- replicateM (fromIntegral c) deserialize
        return $ Block header txs
    serialize (Block h txs) = do
        serialize h
        putVarInt $ length txs
        forM_ txs serialize


instance Serialize Block where
    get = deserialize
    put = serialize


instance Binary Block where
    get = deserialize
    put = serialize


instance ToJSON Block where
    toJSON (Block h t) = object ["header" .= h, "transactions" .= t]
    toEncoding (Block h t) = pairs $ "header" .= h <> "transactions" .= t


instance FromJSON Block where
    parseJSON =
        withObject "Block" $ \o ->
            Block <$> o .: "header" <*> o .: "transactions"


-- | Block header hash. To be serialized reversed for display purposes.
newtype BlockHash = BlockHash
    { getBlockHash :: Hash256
    }
    deriving (Eq, Ord, Generic, Hashable, Serial, NFData)


instance Serialize BlockHash where
    put = serialize
    get = deserialize


instance Binary BlockHash where
    put = serialize
    get = deserialize


instance Show BlockHash where
    showsPrec _ = shows . blockHashToHex


instance Read BlockHash where
    readPrec = do
        R.String str <- R.lexP
        maybe R.pfail return $ hexToBlockHash $ cs str


instance IsString BlockHash where
    fromString s =
        let e = error "Could not read block hash from hex string"
         in fromMaybe e $ hexToBlockHash $ cs s


instance FromJSON BlockHash where
    parseJSON =
        withText "BlockHash" $
            maybe mzero return . hexToBlockHash


instance ToJSON BlockHash where
    toJSON = String . blockHashToHex
    toEncoding h =
        unsafeToEncoding $
            char7 '"'
                <> hexBuilder (BL.reverse (runPutL (serialize h)))
                <> char7 '"'


-- | Block hashes are reversed with respect to the in-memory byte order in a
-- block hash when displayed.
blockHashToHex :: BlockHash -> Text
blockHashToHex (BlockHash h) = encodeHex (B.reverse (runPutS (serialize h)))


-- | Convert a human-readable hex block hash into a 'BlockHash'. Bytes are
-- reversed as normal.
hexToBlockHash :: Text -> Maybe BlockHash
hexToBlockHash hex = do
    bs <- B.reverse <$> decodeHex hex
    h <- eitherToMaybe (runGetS deserialize bs)
    return $ BlockHash h


-- | Data type recording information of a 'Block'. The hash of a block is
-- defined as the hash of this data structure, serialized. The block mining
-- process involves finding a partial hash collision by varying the nonce in the
-- 'BlockHeader' and/or additional entropy in the coinbase 'Transaction' of this
-- 'Block'. Variations in the coinbase will result in different merkle roots in
-- the 'BlockHeader'.
data BlockHeader = BlockHeader
    { blockVersion :: !Word32 --  4 bytes
    , prevBlock :: !BlockHash -- 32 bytes

    -- ^ hash of the previous block (parent)
    , merkleRoot :: !Hash256 -- 32 bytes

    -- ^ root of the merkle tree of transactions
    , blockTimestamp :: !Timestamp --  4 bytes

    -- ^ unix timestamp
    , blockBits :: !Word32 --  4 bytes

    -- ^ difficulty target
    , bhNonce :: !Word32 --  4 bytes

    -- ^ random nonce
    }
    deriving (Eq, Ord, Show, Read, Generic, Hashable, NFData)


-- 80 bytes

instance ToJSON BlockHeader where
    toJSON (BlockHeader v p m t b n) =
        object
            [ "version" .= v
            , "prevblock" .= p
            , "merkleroot" .= encodeHex (runPutS (serialize m))
            , "timestamp" .= t
            , "bits" .= b
            , "nonce" .= n
            ]
    toEncoding (BlockHeader v p m t b n) =
        pairs
            ( "version" .= v
                <> "prevblock" .= p
                <> "merkleroot" .= encodeHex (runPutS (serialize m))
                <> "timestamp" .= t
                <> "bits" .= b
                <> "nonce" .= n
            )


instance FromJSON BlockHeader where
    parseJSON =
        withObject "BlockHeader" $ \o ->
            BlockHeader
                <$> o .: "version"
                <*> o .: "prevblock"
                <*> (f =<< o .: "merkleroot")
                <*> o .: "timestamp"
                <*> o .: "bits"
                <*> o .: "nonce"
      where
        f = maybe mzero return . (eitherToMaybe . runGetS deserialize <=< decodeHex)


instance Serial BlockHeader where
    deserialize = do
        v <- getWord32le
        p <- deserialize
        m <- deserialize
        t <- getWord32le
        b <- getWord32le
        n <- getWord32le
        return
            BlockHeader
                { blockVersion = v
                , prevBlock = p
                , merkleRoot = m
                , blockTimestamp = t
                , blockBits = b
                , bhNonce = n
                }
    serialize (BlockHeader v p m bt bb n) = do
        putWord32le v
        serialize p
        serialize m
        putWord32le bt
        putWord32le bb
        putWord32le n


instance Binary BlockHeader where
    put = serialize
    get = deserialize


instance Serialize BlockHeader where
    put = serialize
    get = deserialize


-- | Compute hash of 'BlockHeader'.
headerHash :: BlockHeader -> BlockHash
headerHash = BlockHash . doubleSHA256 . runPutS . serialize


-- | A block locator is a set of block headers, denser towards the best block
-- and sparser towards the genesis block. It starts at the highest block known.
-- It is used by a node to synchronize against the network. When the locator is
-- provided to a peer, it will send back block hashes starting from the first
-- block in the locator that it recognizes.
type BlockLocator = [BlockHash]


-- | Data type representing a getblocks message request. It is used in the
-- bitcoin protocol to retrieve blocks from a peer by providing it a
-- 'BlockLocator' object. The response to a 'GetBlocks' message is an 'Inv'
-- message containing a list of block hashes that the peer believes this node is
-- missing. The number of block hashes in that inv message will end at the stop
-- block hash, at at the tip of the chain, or after 500 entries, whichever comes
-- earlier.
data GetBlocks = GetBlocks
    { getBlocksVersion :: !Word32
    , getBlocksLocator :: !BlockLocator
    -- ^ block locator object
    , getBlocksHashStop :: !BlockHash
    -- ^ hash of the last desired block
    }
    deriving (Eq, Show, Read, Generic, NFData)


instance Serial GetBlocks where
    deserialize =
        GetBlocks
            <$> getWord32le
            <*> (repList =<< deserialize)
            <*> deserialize
      where
        repList (VarInt c) = replicateM (fromIntegral c) deserialize
    serialize (GetBlocks v xs h) = putGetBlockMsg v xs h


instance Serialize GetBlocks where
    put = serialize
    get = deserialize


putGetBlockMsg :: MonadPut m => Word32 -> BlockLocator -> BlockHash -> m ()
putGetBlockMsg v xs h = do
    putWord32le v
    putVarInt $ length xs
    forM_ xs serialize
    serialize h


-- | Similar to the 'GetBlocks' message type but for retrieving block headers
-- only. The response to a 'GetHeaders' request is a 'Headers' message
-- containing a list of block headers. A maximum of 2000 block headers can be
-- returned. 'GetHeaders' is used by simplified payment verification (SPV)
-- clients to exclude block contents when synchronizing the block chain.
data GetHeaders = GetHeaders
    { getHeadersVersion :: !Word32
    , getHeadersBL :: !BlockLocator
    -- ^ block locator object
    , getHeadersHashStop :: !BlockHash
    -- ^ hash of the last desired block header
    }
    deriving (Eq, Show, Read, Generic, NFData)


instance Serial GetHeaders where
    deserialize =
        GetHeaders
            <$> getWord32le
            <*> (repList =<< deserialize)
            <*> deserialize
      where
        repList (VarInt c) = replicateM (fromIntegral c) deserialize
    serialize (GetHeaders v xs h) = putGetBlockMsg v xs h


instance Serialize GetHeaders where
    put = serialize
    get = deserialize


instance Binary GetHeaders where
    put = serialize
    get = deserialize


-- | 'BlockHeader' type with a transaction count as 'VarInt'
type BlockHeaderCount = (BlockHeader, VarInt)


-- | The 'Headers' type is used to return a list of block headers in
-- response to a 'GetHeaders' message.
newtype Headers = Headers
    { headersList :: [BlockHeaderCount]
    -- ^ list of block headers with transaction count
    }
    deriving (Eq, Show, Read, Generic, NFData)


instance Serial Headers where
    deserialize = Headers <$> (repList =<< deserialize)
      where
        repList (VarInt c) = replicateM (fromIntegral c) action
        action = liftM2 (,) deserialize deserialize
    serialize (Headers xs) = do
        putVarInt $ length xs
        forM_ xs $ \(a, b) -> serialize a >> serialize b


instance Serialize Headers where
    put = serialize
    get = deserialize


instance Binary Headers where
    put = serialize
    get = deserialize


-- | Decode the compact number used in the difficulty target of a block.
--
-- The compact format is a representation of a whole number \(N\) using an
-- unsigned 32-bit number similar to a floating point format. The most
-- significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as the number of bytes of \(N\). The lower 23 bits are the
-- mantissa. Bit number 24 represents the sign of \(N\).
--
-- \[
-- N = -1^{sign} \times mantissa \times 256^{exponent-3}
-- \]
decodeCompact ::
    Word32 ->
    -- | true means overflow
    (Integer, Bool)
decodeCompact nCompact = (if neg then res * (-1) else res, over)
  where
    nSize :: Int
    nSize = fromIntegral nCompact `shiftR` 24
    nWord' :: Word32
    nWord' = nCompact .&. 0x007fffff
    nWord :: Word32
    nWord
        | nSize <= 3 = nWord' `shiftR` (8 * (3 - nSize))
        | otherwise = nWord'
    res :: Integer
    res
        | nSize <= 3 = fromIntegral nWord
        | otherwise = fromIntegral nWord `shiftL` (8 * (nSize - 3))
    neg = nWord /= 0 && (nCompact .&. 0x00800000) /= 0
    over =
        nWord /= 0
            && ( nSize > 34
                    || nWord > 0xff && nSize > 33
                    || nWord > 0xffff && nSize > 32
               )


-- | Encode an 'Integer' to the compact number format used in the difficulty
-- target of a block.
encodeCompact :: Integer -> Word32
encodeCompact i = nCompact
  where
    i' = abs i
    neg = i < 0
    nSize' :: Int
    nSize' =
        let f 0 = 0
            f n = 1 + f (n `shiftR` 8)
         in f i'
    nCompact''' :: Word32
    nCompact'''
        | nSize' <= 3 = fromIntegral $ (low64 .&. i') `shiftL` (8 * (3 - nSize'))
        | otherwise = fromIntegral $ low64 .&. (i' `shiftR` (8 * (nSize' - 3)))
    nCompact'' :: Word32
    nSize :: Int
    (nCompact'', nSize)
        | nCompact''' .&. 0x00800000 /= 0 = (nCompact''' `shiftR` 8, nSize' + 1)
        | otherwise = (nCompact''', nSize')
    nCompact' :: Word32
    nCompact' = nCompact'' .|. (fromIntegral nSize `shiftL` 24)
    nCompact :: Word32
    nCompact
        | neg && (nCompact' .&. 0x007fffff /= 0) = nCompact' .|. 0x00800000
        | otherwise = nCompact'
    low64 :: Integer
    low64 = 0xffffffffffffffff
