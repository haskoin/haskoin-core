module Bitcoin.Crypto 
( doubleSHA256
, checksum
) where

import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString as BS

doubleSHA256 :: BS.ByteString -> BS.ByteString
doubleSHA256 = H.hash . H.hash

checksum :: BS.ByteString -> BS.ByteString
checksum = (BS.take 4) . doubleSHA256

