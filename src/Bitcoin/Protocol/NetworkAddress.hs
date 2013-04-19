module Bitcoin.Protocol.NetworkAddress ( NetworkAddress(..) ) where

import Bitcoin.Protocol
import Control.Applicative

import qualified Data.ByteString as BS

data NetworkAddress = NetworkAddress {
    services :: Word64,
    address  :: Word128,
    port     :: Word16
} deriving (Eq, Show, Read)

instance BitcoinProtocol NetworkAddress where

    bitcoinGet = NetworkAddress <$> getWord64le
                                <*> getWord128be
                                <*> getWord16be

    bitcoinPut (NetworkAddress s a p) = do
        putWord64le  s
        putWord128be a
        putWord16be  p

