module Bitcoin.Util
( toStrictBS
, toLazyBS
, hasMoreM
) where

import Data.Bits
import Data.Word
import Data.List
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

hasMoreM :: Get a -> Get a -> Get a
hasMoreM t f = isEmpty >>= (\e -> if e then f else t)

