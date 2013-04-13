module Bitcoin.Type.GetData ( GetData(..) ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

import Bitcoin.Type.InvVector
import Bitcoin.Type.VarInt

data GetData = GetData {
   getDataList :: [InvVector] 
} deriving (Read, Show)

instance Bitcoin.Type GetData where
    get = do
        (VarInt count) <- Bitcoin.get 
        GetData <$> replicateM (fromIntegral count) Bitcoin.get

    put (GetData xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.put

