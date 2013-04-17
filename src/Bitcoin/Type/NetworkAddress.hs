module Bitcoin.Type.NetworkAddress 
( NetworkAddress(..)
, IPv6(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin
import qualified Data.ByteString as BS

type IPv6 = (Word64, Word64)

data NetworkAddress = NetworkAddress {
    services :: Word64,
    address  :: IPv6,
    port     :: Word16
} deriving (Show, Read, Eq)

instance Bitcoin.Type NetworkAddress where
    get = NetworkAddress <$> Bitcoin.getWord64
                         <*> Bitcoin.getIPv6
                         <*> Bitcoin.getPortNumber

    put (NetworkAddress s a p) = do
        Bitcoin.putWord64      s
        Bitcoin.putIPv6        a
        Bitcoin.putPortNumber  p

