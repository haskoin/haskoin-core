module Bitcoin.Protocol.Version ( Version(..) ) where

import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Util

data Version = Version {
    version     :: Word32,
    services    :: Word64,
    timestamp   :: Word64,
    addrRecv    :: NetworkAddress,
    addrSend    :: NetworkAddress,
    nonce       :: Word64,
    userAgent   :: VarString,
    startHeight :: Word32,
    relay       :: Bool
} deriving (Show, Read, Eq)

instance BitcoinProtocol Version where

    bitcoinGet = Version <$> getWord32le
                         <*> getWord64le
                         <*> getWord64le
                         <*> bitcoinGet
                         <*> bitcoinGet
                         <*> getWord64le
                         <*> bitcoinGet
                         <*> getWord32le
                         <*> hasMore getBool (return True)

    bitcoinPut (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        bitcoinPut  ar
        bitcoinPut  as
        putWord64le n
        bitcoinPut  ua
        putWord32le sh
        putBool     r

