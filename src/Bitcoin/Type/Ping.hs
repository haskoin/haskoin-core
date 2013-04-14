module Bitcoin.Type.Ping
( Ping(..)
, Pong(..)
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

newtype Ping = Ping { pingNonce :: Word64 } deriving (Show, Read)
newtype Pong = Pong { pongNonce :: Word64 } deriving (Show, Read)

instance Bitcoin.Type Ping where
    get = Ping <$> Bitcoin.getWord64
    put (Ping n) = Bitcoin.putWord64 n

instance Bitcoin.Type Pong where
    get = Pong <$> Bitcoin.getWord64
    put (Pong n) = Bitcoin.putWord64 n

