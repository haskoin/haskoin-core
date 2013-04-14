module Bitcoin.Type.Headers 
( Headers(..)
, BlockHeaderCount
) where

import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Monad

import Bitcoin.Type.BlockHeader
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

type BlockHeaderCount = (BlockHeader, VarInt)

data Headers = Headers {
    headersList :: [BlockHeaderCount]
} deriving (Read, Show)

instance Bitcoin.Type Headers where
    get = Headers <$> (getBlocks =<< Bitcoin.get)
        where action = liftM2 (,) Bitcoin.get Bitcoin.get 
              getBlocks (VarInt c) = replicateM (fromIntegral c) action

    put (Headers xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length ) xs
        forM_ xs (\(a,b) -> Bitcoin.put a >> Bitcoin.put b)

