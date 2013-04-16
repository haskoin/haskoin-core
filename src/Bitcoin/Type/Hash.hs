module Bitcoin.Type.Hash 
( Hash()
, bsToHash
, hashToBS
) where

import Data.Word
import Control.Applicative

import qualified Data.ByteString as BS

import qualified Bitcoin.Type as Bitcoin

newtype Hash = Hash { getHash :: BS.ByteString }
    deriving (Read, Show)

instance Bitcoin.Type Hash where
    get = Hash <$> Bitcoin.getByteString 32
    put (Hash bs) = Bitcoin.putByteString bs

bsToHash :: BS.ByteString -> Either String Hash
bsToHash bs
    | BS.length bs == 32 = Right (Hash bs)
    | otherwise = Left "bsToHash: Bad ByteString length"

hashToBS :: Hash -> BS.ByteString
hashToBS (Hash bs) = bs
