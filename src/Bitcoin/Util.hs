module Bitcoin.Util
( toStrictBS
, toLazyBS
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

