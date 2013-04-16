module Bitcoin.Crypto 
( doubleSHA256
, checksum
) where

import Data.Word
import Crypto.Hash.SHA256
import Data.Binary.Get

import qualified Data.ByteString as BS

import Bitcoin.Type.Hash
import Bitcoin.Util
import qualified Bitcoin.Type as Bitcoin

doubleSHA256 :: BS.ByteString -> Either String Hash
doubleSHA256 = bsToHash . hash . hash

checksum :: BS.ByteString -> Either String Word32
checksum bs = doubleSHA256 bs >>= 
    (Right . (runGet Bitcoin.getMsgChkSum) . toLazyBS . hashToBS)


