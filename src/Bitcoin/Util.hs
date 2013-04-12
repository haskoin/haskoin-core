module Bitcoin.Util
( toStrict
, hasMoreM
, fromByteString
) where

import Data.Bits
import Data.Word
import Data.List
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

hasMoreM :: Get a -> Get a -> Get a
hasMoreM t f = isEmpty >>= (\e -> if e then f else t)

fromByteString :: BS.ByteString -> Word64
fromByteString = (foldl' accum 0) . BS.unpack 
    where accum a o = (a `shiftL` 8) .|. fromIntegral o
