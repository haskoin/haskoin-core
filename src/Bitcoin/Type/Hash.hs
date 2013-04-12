module Bitcoin.Type.Hash 
( Hash 
, fromByteString
) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

import qualified Bitcoin.Util as U

type Hash = (Word64, Word64, Word64, Word64)

fromByteString :: BS.ByteString -> Hash
fromByteString bs = case (splitString [] bs) of
    [w1, w2, w3, w4] -> (w1, w2, w3, w4)
    _                -> error "Hash.fromByteString Hash parsing error"

splitString :: [Word64] -> BS.ByteString -> [Word64]
splitString acc bs 
    | BS.null bs = acc
    | otherwise  = splitString (acc ++ [takeWord64 bs]) (BS.drop 8 bs) 
        where takeWord64 = U.fromByteString . (BS.take 8)

