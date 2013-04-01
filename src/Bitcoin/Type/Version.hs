module Bitcoin.Type.Version ( Version(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.VarString

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
} deriving (Show, Read)

instance Binary Version where
    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> get
                  <*> getWord32le
                  <*> get

    put (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        put         ua
        putWord32le sh
        put         r

