module Bitcoin.Type.Addr ( Addr(..) ) where

import Control.Monad
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.VarInt

data Addr = Addr {
    addrList :: [(Word32, NetworkAddress)]
} deriving (Show, Read)

instance Bitcoin.Type Addr where
    get = do
        (VarInt count) <- Bitcoin.get
        let action = liftM2 (,) Bitcoin.getWord32 Bitcoin.get
        Addr <$> replicateM (fromIntegral count) action

    put (Addr xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs $ \(a,b) -> (Bitcoin.putWord32 a) >> (Bitcoin.put b)

