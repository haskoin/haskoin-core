module Network.Haskoin.Protocol.BloomFilter 
( BloomFlags(..)
, BloomFilter(..)
, FilterLoad(..)
, FilterAdd(..)
) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word8, Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord8, getWord32le, getByteString)
import Data.Binary.Put (putWord8, putWord32le, putByteString)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S (Seq, fromList, length)
import qualified Data.ByteString as BS (ByteString, length)

import Network.Haskoin.Protocol.VarInt

-- | The bloom flags are used to tell the remote peer how to auto-update
-- the provided bloom filter. 
data BloomFlags
    = BloomUpdateNone         -- ^ Never update
    | BloomUpdateAll          -- ^ Auto-update on all outputs
    | BloomUpdateP2PubKeyOnly 
    -- ^ Only auto-update on outputs that are pay-to-pubkey or pay-to-multisig.
    -- This is the default setting.
    deriving (Eq, Show, Read)

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
    { bloomData      :: S.Seq Word8 -- ^ Bloom filter data
    , bloomFull      :: Bool        
    -- ^ Flag indicating if the filter is full ('bloomData' is all 0x00)
    , bloomEmpty     :: Bool
    -- ^ Flag indicating if the filter is empty ('bloomData' is all 0xff)
    , bloomHashFuncs :: Word32     -- ^ Number of hash functions for this filter
    , bloomTweak     :: Word32     -- ^ Hash function random nonce
    , bloomFlags     :: BloomFlags -- ^ Bloom filter auto-update flags
    }
    deriving (Eq, Show, Read)

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

instance Binary FilterLoad where
    get = FilterLoad <$> get
    put (FilterLoad f) = put f

-- | Add the given data element to the connections current filter without
-- requiring a completely new one to be set.
newtype FilterAdd = FilterAdd { getFilterData :: BS.ByteString }
    deriving (Eq, Show, Read)

instance Binary FilterAdd where
    get = do
        (VarInt len) <- get
        dat <- getByteString $ fromIntegral len
        return $ FilterAdd dat

    put (FilterAdd bs) = do
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs


