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
    get = GetHeaders <$> Bitcoin.getWord32
                     <*> (readHashes =<< Bitcoin.get)
                     <*> Bitcoin.get
        where readHashes (VarInt c) = replicateM (fromIntegral c) Bitcoin.get

    put (GetHeaders v xs h) = do
        Bitcoin.putWord32 v
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.put
        Bitcoin.put h

