module Bitcoin.Type.VarString ( VarString(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Data.ByteString as BS

import Bitcoin.Type.VarInt

newtype VarString = VarString { getVarString :: BS.ByteString }
    deriving (Eq, Show, Read)

instance Binary VarString where
    get = do
        (VarInt size) <- get :: Get VarInt
        VarString <$> getByteString (fromIntegral size)

    put (VarString x) = do
        put . VarInt . fromIntegral . BS.length $ x
        putByteString x
