module Bitcoin.Util
( toStrict
, hasMoreM
) where

import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

hasMoreM :: Get a -> Get a -> Get a
hasMoreM t f = isEmpty >>= (\e -> if e then f else t)

