module Network.Haskoin.Protocol.VarString ( VarString(..) ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString as BS 
    ( ByteString
    , length
    )
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)

import Network.Haskoin.Protocol.VarInt

-- | Data type for variable length strings. Variable length strings are
-- serialized as a 'VarInt' followed by a bytestring.
newtype VarString = VarString { getVarString :: BS.ByteString }
    deriving (Eq, Show)

instance Binary VarString where

    get = VarString <$> (readBS =<< get)
      where 
        readBS (VarInt len) = getByteString (fromIntegral len)

    put (VarString bs) = do
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs

