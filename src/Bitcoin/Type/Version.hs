module Bitcoin.Type.Version ( Version(..) ) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

import Bitcoin.Type.NetworkAddress
import Bitcoin.Type.VarString
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
} deriving (Show, Read)

instance Bitcoin.Type Version where
    get = Version <$> Bitcoin.getWord32
                  <*> Bitcoin.getWord64
                  <*> Bitcoin.getWord64
                  <*> Bitcoin.get
                  <*> Bitcoin.get
                  <*> Bitcoin.getWord64
                  <*> Bitcoin.get
                  <*> Bitcoin.getWord32
                  <*> Bitcoin.hasMore Bitcoin.getBool (return True)

    put (Version v s t ar as n ua sh r) = do
        Bitcoin.putWord32 v
        Bitcoin.putWord64 s
        Bitcoin.putWord64 t
        Bitcoin.put ar
        Bitcoin.put as
        Bitcoin.putWord64 n
        Bitcoin.put ua
        Bitcoin.putWord32 sh
        Bitcoin.putBool r


