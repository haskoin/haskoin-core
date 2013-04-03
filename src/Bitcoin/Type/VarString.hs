module Bitcoin.Type.VarString ( VarString(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.ByteString as BS

import qualified Bitcoin.Type as Bitcoin
import Bitcoin.Type.VarInt

newtype VarString = VarString { getVarString :: BS.ByteString }
    deriving (Eq, Ord, Show, Read)

instance Bitcoin.Type VarString where
    get = do
        (VarInt len) <- Bitcoin.get
        VarString <$> Bitcoin.getByteString (fromIntegral len)

    put (VarString x) = do
        Bitcoin.put . VarInt . fromIntegral . BS.length $ x
        Bitcoin.putByteString x

