{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Haskoin.Block.Types
    ( -- * Block
      Block(..)
    , BlockHeight
    , Timestamp
    , BlockHeader(..)
    , headerHash
    , BlockLocator
    , GetBlocks(..)
    , GetHeaders(..)
    , BlockHeaderCount
    , BlockHash(..)
    , blockHashToHex
    , hexToBlockHash
    , Headers(..)
    , decodeCompact
    , encodeCompact
    ) where

import           Control.DeepSeq                   (NFData, rnf)
import           Control.Monad                     (forM_, liftM2, mzero,
                                                    replicateM)
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    Value (String), parseJSON,
                                                    toJSON, withText)
import           Data.Bits                         (shiftL, shiftR, (.&.),
                                                    (.|.))
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.Hashable                     (Hashable)
import           Data.Maybe                        (fromMaybe)
import           Data.Serialize                    (Serialize, decode, encode,
                                                    get, put)
import           Data.Serialize.Get                (getWord32le)
import           Data.Serialize.Put                (Put, putWord32le)
import           Data.String                       (IsString, fromString)
import           Data.String.Conversions           (cs)
import           Data.Word                         (Word32)
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Network.Types
import           Network.Haskoin.Transaction.Types
import           Network.Haskoin.Util
import qualified Text.Read                         as R

-- | Height of a block in the blockchain, starting at 0 for Genesis.
type BlockHeight = Word32

-- | Block timestamp as Unix time (seconds since 1970-01-01 00:00 UTC).
type Timestamp = Word32

-- | Block header and transactions.
data Block =
    Block { blockHeader :: !BlockHeader
          , blockTxns   :: ![Tx]
          } deriving (Eq, Show)

instance NFData Block where
    rnf (Block h ts) = rnf h `seq` rnf ts

instance Serialize Block where
    get = do
        header     <- get
        (VarInt c) <- get
        txs        <- replicateM (fromIntegral c) get
        return $ Block header txs
    put (Block h txs) = do
        put h
        put $ VarInt $ fromIntegral $ length txs
        forM_ txs put

-- | Block header hash. To be serialized reversed for display purposes.
newtype BlockHash = BlockHash
    { getBlockHash :: Hash256 }
    deriving (Eq, Ord, NFData, Hashable, Serialize)

instance Show BlockHash where
    showsPrec d a =
        showParen (d > 10) $ showString "BlockHash " . shows (blockHashToHex a)

instance Read BlockHash where
    readPrec =
        R.parens $ do
            R.Ident "BlockHash" <- R.lexP
            R.String str <- R.lexP
            maybe R.pfail return $ hexToBlockHash $ cs str

instance IsString BlockHash where
    fromString s =
        let e = error "Could not read block hash from hex string"
        in fromMaybe e $ hexToBlockHash $ cs s

instance FromJSON BlockHash where
    parseJSON = withText "Block hash" $ \t ->
        maybe mzero return $ hexToBlockHash $ cs t

instance ToJSON BlockHash where
    toJSON = String . cs . blockHashToHex

-- | Block hashes are reversed with respect to the in-memory byte order in a
-- block hash when displayed.
blockHashToHex :: BlockHash -> ByteString
blockHashToHex (BlockHash h) = encodeHex (BS.reverse (encode h))

-- | Convert a human-readable hex block hash into a 'BlockHash'. Bytes are
-- reversed as normal.
hexToBlockHash :: ByteString -> Maybe BlockHash
hexToBlockHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- eitherToMaybe (decode bs)
    return $ BlockHash h

-- | Data type recording information of a 'Block'. The hash of a block is
-- defined as the hash of this data structure, serialized. The block mining
-- process involves finding a partial hash collision by varying the nonce in the
-- 'BlockHeader' and/or additional entropy in the coinbase 'Transaction' of this
-- 'Block'. Variations in the coinbase will result in different merkle roots in
-- the 'BlockHeader'.
data BlockHeader =
    BlockHeader { -- | Block version information, based on the version of the
                  -- software creating this block.
                  blockVersion   :: !Word32      -- 16 bytes
                  -- | Hash of the previous block (parent) referenced by this
                  -- block.
                , prevBlock      :: !BlockHash   -- 64 bytes
                  -- | Root of the merkle tree of all transactions pertaining
                  -- to this block.
                , merkleRoot     :: !Hash256     -- 64 bytes
                  -- | Unix timestamp recording when this block was created
                , blockTimestamp :: !Timestamp   -- 16 bytes
                  -- | The difficulty target being used for this block
                , blockBits      :: !Word32      -- 16 bytes
                  -- | A random nonce used to generate this block. Additional
                  -- randomness is included in the coinbase transaction of
                  -- this block.
                , bhNonce        :: !Word32      -- 16 bytes
                } deriving (Eq, Show, Ord)       -- 208 bytes (above + 16 bytes)

-- | Compute hash of 'BlockHeader'.
headerHash :: BlockHeader -> BlockHash
headerHash = BlockHash . doubleSHA256 . encode

instance NFData BlockHeader where
    rnf (BlockHeader v p m t b n) =
        rnf v `seq` rnf p `seq` rnf m `seq`
        rnf t `seq` rnf b `seq` rnf n

instance Serialize BlockHeader where
    get = do
        v <- getWord32le
        p <- get
        m <- get
        t <- getWord32le
        b <- getWord32le
        n <- getWord32le
        return BlockHeader
            { blockVersion   = v
            , prevBlock      = p
            , merkleRoot     = m
            , blockTimestamp = t
            , blockBits      = b
            , bhNonce        = n
            }

    put (BlockHeader v p m bt bb n) = do
        putWord32le v
        put         p
        put         m
        putWord32le bt
        putWord32le bb
        putWord32le n

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
-- missing.
data GetBlocks =
    GetBlocks { -- | Protocol version.
                getBlocksVersion  :: !Word32
                -- | Block locator object. It is a list of block hashes from the
                -- most recent block back to the Genesis block. The list is
                -- dense at first and sparse towards the end.
              , getBlocksLocator  :: !BlockLocator
                -- | Hash of the last desired block. If set to zero, the
                -- maximum number of block hashes is returned (500).
              , getBlocksHashStop :: !BlockHash
              } deriving (Eq, Show)

instance NFData GetBlocks where
    rnf (GetBlocks v l h) = rnf v `seq` rnf l `seq` rnf h

instance Serialize GetBlocks where

    get = GetBlocks <$> getWord32le
                    <*> (repList =<< get)
                    <*> get
      where
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetBlocks v xs h) = putGetBlockMsg v xs h

putGetBlockMsg :: Word32 -> BlockLocator -> BlockHash -> Put
putGetBlockMsg v xs h = do
    putWord32le v
    put $ VarInt $ fromIntegral $ length xs
    forM_ xs put
    put h

-- | Similar to the 'GetBlocks' message type but for retrieving block headers
-- only. The response to a 'GetHeaders' request is a 'Headers' message
-- containing a list of block headers. A maximum of 2000 block headers can be
-- returned. 'GetHeaders' is used by simplified payment verification (SPV)
-- clients to exclude block contents when synchronizing the blockchain.
data GetHeaders =
    GetHeaders {
                 -- | Protocol version.
                 getHeadersVersion  :: !Word32
                 -- | Block locator object. It is a list of block hashes from
                 -- the most recent block back to the Genesis block. The list is
                 -- dense at first and sparse towards the end.
               , getHeadersBL       :: !BlockLocator
                 -- | Hash of the last desired block header. When set to zero,
                 -- the maximum number of block headers is returned (2000).
               , getHeadersHashStop :: !BlockHash
               } deriving (Eq, Show)

instance NFData GetHeaders where
    rnf (GetHeaders v l h) = rnf v `seq` rnf l `seq` rnf h

instance Serialize GetHeaders where

    get = GetHeaders <$> getWord32le
                     <*> (repList =<< get)
                     <*> get
      where
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetHeaders v xs h) = putGetBlockMsg v xs h

-- | 'BlockHeader' type with a transaction count as 'VarInt'
type BlockHeaderCount = (BlockHeader, VarInt)

-- | The 'Headers' type is used to return a list of block headers in
-- response to a 'GetHeaders' message.
newtype Headers =
    Headers { -- | List of block headers with respective transaction counts.
              headersList :: [BlockHeaderCount]
            }
    deriving (Eq, Show)

instance NFData Headers where
    rnf (Headers l) = rnf l

instance Serialize Headers where

    get = Headers <$> (repList =<< get)
      where
        repList (VarInt c) = replicateM (fromIntegral c) action
        action = liftM2 (,) get get

    put (Headers xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> put a >> put b

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
decodeCompact :: Word32 -> (Integer, Bool) -- ^ true means overflow
decodeCompact nCompact = (if neg then res * (-1) else res, over)
  where
    nSize :: Int
    nSize = fromIntegral nCompact `shiftR` 24
    nWord' :: Word32
    nWord' = nCompact .&. 0x007fffff
    nWord :: Word32
    nWord | nSize <= 3 = nWord' `shiftR` (8 * (3 - nSize))
          | otherwise = nWord'
    res :: Integer
    res | nSize <= 3 = fromIntegral nWord
        | otherwise = fromIntegral nWord `shiftL` (8 * (nSize - 3))
    neg = nWord /= 0 && (nCompact .&. 0x00800000) /= 0
    over = nWord /= 0 && (nSize > 34 ||
                          nWord > 0xff && nSize > 33 ||
                          nWord > 0xffff && nSize > 32)

-- | Encode an 'Integer' to the compact number format used in the difficulty
-- target of a block.
encodeCompact :: Integer
              -> Word32
encodeCompact i = nCompact
  where
    i' = abs i
    neg = i < 0
    nSize' :: Int
    nSize' = let f 0 = 0
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
    nCompact | neg && (nCompact' .&. 0x007fffff /= 0) = nCompact' .|. 0x00800000
             | otherwise = nCompact'
    low64 :: Integer
    low64 = 0xffffffffffffffff
