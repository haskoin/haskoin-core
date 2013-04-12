module Bitcoin.Type.InvVector ( InvVector(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import Bitcoin.Type.Hash

import qualified Bitcoin.Type as Bitcoin

data InvVector = InvVector {
    invType :: Word32,
    invHash :: Hash
} deriving (Show, Read)

instance Bitcoin.Type InvVector where
    get = InvVector <$> Bitcoin.getWord32
                    <*> Bitcoin.get

    put (InvVector t h) = do
        Bitcoin.putWord32 t
        Bitcoin.put       h


