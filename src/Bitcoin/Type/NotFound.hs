module Bitcoin.Type.NotFound ( NotFound(..) ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

import Bitcoin.Type.InvVector
import Bitcoin.Type.VarInt

data NotFound = NotFound {
   notFoundList :: [InvVector] 
} deriving (Read, Show)

instance Bitcoin.Type NotFound where
    get = do
        (VarInt count) <- Bitcoin.get 
        NotFound <$> replicateM (fromIntegral count) Bitcoin.get

    put (NotFound xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.put

