module Bitcoin.Type.InvVector 
( InvVector(..) 
, InvType(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import Bitcoin.Type.Hash

import qualified Bitcoin.Type as Bitcoin

data InvType = Error | Tx | Block
    deriving (Show, Read)

instance Bitcoin.Type InvType where
    get = go =<< Bitcoin.getWord32
        where 
            go x = case x of
                0 -> return Error
                1 -> return Tx
                2 -> return Block

    put x = Bitcoin.putWord32 $ case x of
        Error -> 0
        Tx    -> 1
        Block -> 2

data InvVector = InvVector {
    invType :: InvType,
    invHash :: Hash
} deriving (Show, Read)

instance Bitcoin.Type InvVector where
    get = InvVector <$> Bitcoin.get
                    <*> Bitcoin.getHash

    put (InvVector t h) = do
        Bitcoin.put     t
        Bitcoin.putHash h


