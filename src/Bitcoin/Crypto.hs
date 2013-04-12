module Bitcoin.Crypto 
( doubleSHA256
, checksum
) where

import Data.Bits
import Data.Word
import qualified Crypto.Hash.SHA256 as C
import qualified Data.ByteString as BS

import qualified Bitcoin.Type.Hash as H
import qualified Bitcoin.Util as U

doubleSHA256 :: BS.ByteString -> H.Hash
doubleSHA256 = H.fromByteString . C.hash . C.hash

checksum :: BS.ByteString -> Word32
checksum bs = case (doubleSHA256 bs) of
    (x,_,_,_) -> fromIntegral $ x `shiftR` 32

