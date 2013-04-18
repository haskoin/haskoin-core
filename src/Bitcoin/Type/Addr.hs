module Bitcoin.Type.Addr 
( Addr(..)
, NetworkAddressTime 
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad
import Control.Applicative

import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

type NetworkAddressTime = (Word32, NetworkAddress)

data Addr = Addr {
    addrList :: [NetworkAddressTime]
} deriving (Show, Read, Eq)

instance Bitcoin.Type Addr where
    get = do
        (VarInt count) <- Bitcoin.get
        let action = liftM2 (,) Bitcoin.getWord32 Bitcoin.get
        Addr <$> replicateM (fromIntegral count) action

    put (Addr xs) = do
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs $ \(a,b) -> (Bitcoin.putWord32 a) >> (Bitcoin.put b)

