module Bitcoin.Protocol.VarString ( VarString(..) ) where

import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt

import qualified Data.ByteString as BS

newtype VarString = VarString { getVarString :: BS.ByteString }
    deriving (Eq, Ord, Show, Read)

instance BitcoinProtocol VarString where

    bitcoinGet = VarString <$> (readBS =<< bitcoinGet)
        where readBS (VarInt len) = getByteString (fromIntegral len)

    bitcoinPut (VarString bs) = do
        bitcoinPut $ lengthFromBS bs
        putByteString bs

