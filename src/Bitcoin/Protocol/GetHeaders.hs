module Bitcoin.Protocol.GetHeaders ( GetHeaders(..) ) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt

data GetHeaders = GetHeaders {
    getHeadersVersion :: Word32,
    blockLocatorHash  :: [Word256],
    hashStop          :: Word256
} deriving (Read, Show)

instance BitcoinProtocol GetHeaders where

    bitcoinGet = GetHeaders <$> getWord32le
                            <*> (readList =<< bitcoinGet)
                            <*> getWord256be
        where readList (VarInt c) = replicateM (fromIntegral c) getWord256be

    bitcoinPut (GetHeaders v xs h) = do
        putWord32le v
        bitcoinPut $ lengthFromList xs
        forM_ xs putWord256be
        putWord256be h

