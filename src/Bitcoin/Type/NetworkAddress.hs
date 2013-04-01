module Bitcoin.Type.NetworkAddress ( NetworkAddress(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative
import qualified Data.ByteString as BS

data NetworkAddress = NetworkAddress {
    services :: Word64,
    address  :: BS.ByteString,
    port     :: Word16
} deriving (Eq, Show, Read)

instance Binary NetworkAddress where
    get = NetworkAddress <$> getWord64le
                         <*> getByteString 16
                         <*> getWord16be 

    put (NetworkAddress s a p) = do
        putWord64le       s
        putByteString     a
        putWord16be       p

