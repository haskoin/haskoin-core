module Bitcoin.Crypto 
( doubleSHA256
, checksum
) where

import Data.Word
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString as BS

import Bitcoin.Type.Hash
import Bitcoin.Util

doubleSHA256 :: BS.ByteString -> BS.ByteString
doubleSHA256 = H.hash . H.hash

checksum :: BS.ByteString -> Word32
checksum = fromByteString . (BS.take 4) . doubleSHA256

