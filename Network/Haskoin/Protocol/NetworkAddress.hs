module Network.Haskoin.Protocol.NetworkAddress ( NetworkAddress(..) ) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word16, Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord16be
    , getWord64le
    , getWord64be
    )
import Data.Binary.Put
    ( putWord16be
    , putWord64le
    , putWord64be
    )

-- | Data type describing a bitcoin network address. Addresses are stored in
-- IPv6. IPv4 addresses are mapped to IPv6 using IPv4 mapped IPv6 addresses:
-- <http://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses>. Sometimes,
-- timestamps are sent together with the 'NetworkAddress' such as in the 'Addr'
-- data type.
data NetworkAddress = 
    NetworkAddress {
                   -- | Bitmask of services available for this address
                     naServices :: !Word64
                   -- | IPv6 address serialized as big endian
                   , naAddress  :: !(Word64, Word64)
                   -- | Port number serialized as big endian
                   , naPort     :: !Word16
                   } deriving (Eq, Show)

instance Binary NetworkAddress where

    get = NetworkAddress <$> getWord64le
                         <*> (liftM2 (,) getWord64be getWord64be)
                         <*> getWord16be

    put (NetworkAddress s (al,ar) p) = do
        putWord64le s
        putWord64be al
        putWord64be ar
        putWord16be p

