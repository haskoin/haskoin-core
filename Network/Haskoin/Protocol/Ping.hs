module Network.Haskoin.Protocol.Ping
( Ping(..)
, Pong(..)
) where

import Control.Applicative ((<$>))

import Data.Word (Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord64le)
import Data.Binary.Put (putWord64le)

-- | A Ping message is sent to bitcoin peers to check if a TCP\/IP connection
-- is still valid.
newtype Ping = 
    Ping { 
           -- | A random nonce used to identify the recipient of the ping
           -- request once a Pong response is received.  
           pingNonce :: Word64 
         } deriving (Eq, Show)

-- | A Pong message is sent as a response to a ping message.
newtype Pong = 
    Pong { 
           -- | When responding to a Ping request, the nonce from the Ping
           -- is copied in the Pong response.
           pongNonce :: Word64 
         } deriving (Eq, Show)

instance Binary Ping where
    get = Ping <$> getWord64le
    put (Ping n) = putWord64le n

instance Binary Pong where
    get = Pong <$> getWord64le
    put (Pong n) = putWord64le n

