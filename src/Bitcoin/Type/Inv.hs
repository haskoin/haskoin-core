module Bitcoin.Type.Inv ( Inv(..) ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

import Bitcoin.Type.InvVector
import Bitcoin.Type.VarInt

data Inv = Inv {
   invList :: [InvVector] 
} deriving (Read, Show)

instance Bitcoin.Type Inv where
    get = do
        (VarInt count) <- Bitcoin.get 
        Inv <$> replicateM (fromIntegral count) Bitcoin.get

    put (Inv xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.put

