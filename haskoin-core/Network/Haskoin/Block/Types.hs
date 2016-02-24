module Network.Haskoin.Block.Types
( Block(..)
, BlockHeader(..)
, BlockLocator
, GetBlocks(..)
, GetHeaders(..)
, BlockHeaderCount
, BlockHash(..)
, blockHashToHex
, hexToBlockHash
, Headers(..)
, headerHash
, decodeCompact
, encodeCompact
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2, replicateM, forM_, mzero)

import Data.Maybe (fromMaybe)
import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length, reverse)
import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Text.Read (readPrec, parens, lexP, pfail)
import qualified Text.Read as Read (Lexeme(Ident, String))

import Network.Haskoin.Util
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Node.Types
import Network.Haskoin.Transaction.Types

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

newtype BlockHash = BlockHash { getBlockHash :: Hash256 }
    deriving (Eq, Ord)

instance NFData BlockHash where
    rnf (BlockHash h) = rnf $ getHash256 h

instance Show BlockHash where
    showsPrec d h = showParen (d > 10) $
        showString "BlockHash " . shows (blockHashToHex h)

instance Read BlockHash where
    readPrec = parens $ do
        Read.Ident "BlockHash" <- lexP
        Read.String str <- lexP
        maybe pfail return $ hexToBlockHash $ cs str

instance IsString BlockHash where
    fromString = fromMaybe e . hexToBlockHash . cs where
        e = error "Could not read block hash from hex string"

instance Binary BlockHash where
    get = BlockHash <$> get
    put = put . getBlockHash

instance FromJSON BlockHash where
    parseJSON = withText "Block hash" $ \t ->
        maybe mzero return $ hexToBlockHash $ cs t

instance ToJSON BlockHash where
    toJSON = String . cs . blockHashToHex

blockHashToHex :: BlockHash -> ByteString
blockHashToHex (BlockHash h) = encodeHex $ BS.reverse $ getHash256 h

hexToBlockHash :: ByteString -> Maybe BlockHash
hexToBlockHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- bsToHash256 bs
    return $ BlockHash h

-- | Compute the hash of a block header
headerHash :: BlockHeader -> BlockHash
headerHash = BlockHash . doubleHash256 . encode'

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
                , merkleRoot     :: !Hash256
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

-- | Decode the compact number used in the difficulty target of a block into an
-- Integer.
--
-- As described in the Satoshi reference implementation /src/bignum.h:
--
-- The "compact" format is a representation of a whole number N using an
-- unsigned 32bit number similar to a floating point format. The most
-- significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as "number of bytes of N". The lower 23 bits are the mantissa.
-- Bit number 24 (0x800000) represents the sign of N.
--
-- >    N = (-1^sign) * mantissa * 256^(exponent-3)
decodeCompact :: Word32 -> Integer
decodeCompact c =
    if neg then (-res) else res
  where
    size = fromIntegral $ c `shiftR` 24
    neg  = (c .&. 0x00800000) /= 0
    wrd  = c .&. 0x007fffff
    res | size <= 3 = (toInteger wrd) `shiftR` (8*(3 - size))
        | otherwise = (toInteger wrd) `shiftL` (8*(size - 3))

-- | Encode an Integer to the compact number format used in the difficulty
-- target of a block.
encodeCompact :: Integer -> Word32
encodeCompact i
    | i < 0     = c3 .|. 0x00800000
    | otherwise = c3
  where
    posi = abs i
    s1 = BS.length $ integerToBS posi
    c1 | s1 < 3    = posi `shiftL` (8*(3 - s1))
       | otherwise = posi `shiftR` (8*(s1 - 3))
    (s2,c2) | c1 .&. 0x00800000 /= 0  = (s1 + 1, c1 `shiftR` 8)
            | otherwise               = (s1, c1)
    c3 = fromIntegral $ c2 .|. ((toInteger s2) `shiftL` 24)

