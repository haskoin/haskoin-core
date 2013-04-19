module Bitcoin.Crypto 
( doubleSHA256
, doubleSHA256CheckSum
) where

import Bitcoin.Protocol
import Crypto.Hash.SHA256

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

doubleSHA256 :: BS.ByteString -> Word256
doubleSHA256 bs = runGet getWord256be $ BL.fromChunks [hashResult]
    where hashResult = hash $ hash bs

doubleSHA256CheckSum :: BS.ByteString -> Word32
doubleSHA256CheckSum bs = runGet getWord32be $ BL.fromChunks [hashResult]
    where hashResult = BS.take 4 $ hash $ hash bs

