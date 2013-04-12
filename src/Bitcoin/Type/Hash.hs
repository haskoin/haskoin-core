module Bitcoin.Type.Hash ( Hash(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

data Hash = Hash Word64 Word64 Word64 Word64 
    deriving (Show, Read)

instance Bitcoin.Type Hash where
    get = Hash <$> Bitcoin.getWord64
               <*> Bitcoin.getWord64
               <*> Bitcoin.getWord64
               <*> Bitcoin.getWord64

    put (Hash w1 w2 w3 w4) = do
        Bitcoin.putWord64 w1
        Bitcoin.putWord64 w2
        Bitcoin.putWord64 w3
        Bitcoin.putWord64 w4

