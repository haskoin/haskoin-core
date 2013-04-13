module Bitcoin.Type.GetBlocks ( GetBlocks(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad
import Control.Applicative

import Bitcoin.Type.Hash
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

data GetBlocks = GetBlocks {
    getBlocksVersion :: Word32,
    blockLocatorHash :: [Hash],
    hashStop         :: Hash
} deriving (Read, Show)

instance Bitcoin.Type GetBlocks where
    get = do
        version        <- Bitcoin.getWord32
        (VarInt count) <- Bitcoin.get
        hashes         <- replicateM (fromIntegral count) Bitcoin.getHash
        stop           <- Bitcoin.getHash
        return $ GetBlocks version hashes stop

    put (GetBlocks v xs h) = do
        Bitcoin.putWord32 v
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.putHash
        Bitcoin.putHash h
                    
