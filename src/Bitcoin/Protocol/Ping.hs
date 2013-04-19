module Bitcoin.Protocol.Ping
( Ping(..)
, Pong(..)
) where

import Bitcoin.Protocol
import Control.Applicative

newtype Ping = Ping { pingNonce :: Word64 } 
    deriving (Eq, Show, Read)

newtype Pong = Pong { pongNonce :: Word64 } 
    deriving (Eq, Show, Read)

instance BitcoinProtocol Ping where
    bitcoinGet = Ping <$> getWord64le
    bitcoinPut (Ping n) = putWord64le n

instance BitcoinProtocol Pong where
    bitcoinGet = Pong <$> getWord64le
    bitcoinPut (Pong n) = putWord64le n

