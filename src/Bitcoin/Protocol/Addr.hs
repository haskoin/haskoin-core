module Bitcoin.Protocol.Addr 
( Addr(..)
, NetworkAddressTime 
) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.NetworkAddress

type NetworkAddressTime = (Word32, NetworkAddress)

data Addr = Addr {
    addrList :: [NetworkAddressTime]
} deriving (Eq, Show, Read)

instance BitcoinProtocol Addr where

    bitcoinGet = Addr <$> (readList =<< bitcoinGet)
        where readList (VarInt c) = replicateM (fromIntegral c) action
              action = liftM2 (,) getWord32le bitcoinGet 

    bitcoinPut (Addr xs) = do
        bitcoinPut $ lengthFromList xs
        forM_ xs $ \(a,b) -> (putWord32le a) >> (bitcoinPut b)

