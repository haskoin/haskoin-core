module Bitcoin.Type.GetHeaders ( GetHeaders(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad
import Control.Applicative

import Bitcoin.Type.Hash
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

data GetHeaders = GetHeaders {
    getHeadersVersion :: Word32,
    blockLocatorHash  :: [Hash],
    hashStop          :: Hash
} deriving (Read, Show)

instance Bitcoin.Type GetHeaders where
    get = do
        version        <- Bitcoin.getWord32
        (VarInt count) <- Bitcoin.get
        hashes         <- replicateM (fromIntegral count) Bitcoin.getHash
        stop           <- Bitcoin.getHash
        return $ GetHeaders version hashes stop

    put (GetHeaders v xs h) = do
        Bitcoin.putWord32 v
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.putHash
        Bitcoin.putHash h

