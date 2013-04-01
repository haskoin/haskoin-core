module Bitcoin.Util
( toStrict
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks
