module Bitcoin.Type.NetworkAddress ( NetworkAddress(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin
import qualified Data.ByteString as BS

data NetworkAddress = NetworkAddress {
    services :: Word64,
    address  :: BS.ByteString,
    port     :: Word16
} deriving (Eq, Show, Read)

instance Bitcoin.Type NetworkAddress where
    get = NetworkAddress <$> Bitcoin.getWord64
                         <*> Bitcoin.getByteString 16
                         <*> Bitcoin.getPortNumber

    put (NetworkAddress s a p) = do
        Bitcoin.putWord64      s
        Bitcoin.putByteString  a
        Bitcoin.putPortNumber  p

