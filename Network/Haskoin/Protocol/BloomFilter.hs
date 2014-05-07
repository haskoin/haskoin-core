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

data BloomFlags
    = BloomUpdateNone
    | BloomUpdateAll
    | BloomUpdateP2PubKeyOnly
    | BloomUpdateMask
    deriving (Eq, Show)

instance Binary BloomFlags where
    get = go =<< getWord8
      where
        go 0 = return BloomUpdateNone
        go 1 = return BloomUpdateAll
        go 2 = return BloomUpdateP2PubKeyOnly
        go 3 = return BloomUpdateMask
        go _ = fail "BloomFlags get: Invalid bloom flag"

    put f = putWord8 $ case f of
        BloomUpdateNone         -> 0
        BloomUpdateAll          -> 1
        BloomUpdateP2PubKeyOnly -> 2
        BloomUpdateMask         -> 3
            
data BloomFilter = BloomFilter
    { bloomData      :: S.Seq Word8
    , bloomFull      :: Bool
    , bloomEmpty     :: Bool
    , bloomHashFuncs :: Word32
    , bloomTweak     :: Word32
    , bloomFlags     :: BloomFlags
    }
    deriving (Eq, Show)

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

newtype FilterLoad = FilterLoad { getBloomFilter :: BloomFilter }
    deriving (Eq, Show)

instance Binary FilterLoad where
    get = FilterLoad <$> get
    put (FilterLoad f) = put f

newtype FilterAdd = FilterAdd { getFilterData :: BS.ByteString }
    deriving (Eq, Show)

instance Binary FilterAdd where
    get = do
        (VarInt len) <- get
        dat <- getByteString $ fromIntegral len
        return $ FilterAdd dat

    put (FilterAdd bs) = do
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs


