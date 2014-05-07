module Network.Haskoin.Protocol.Version ( Version(..) ) where

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32, Word64)
import Data.Binary (Binary, get, put, Get, Put)
import Data.Binary.Get 
    ( getWord8
    , getWord32le
    , getWord64le
    , isEmpty
    )
import Data.Binary.Put 
    ( putWord8
    , putWord32le
    , putWord64le
    )

import Network.Haskoin.Protocol.VarString
import Network.Haskoin.Protocol.NetworkAddress

-- | When a bitcoin node creates an outgoing connection to another node,
-- the first message it will send is a 'Version' message. The other node
-- will similarly respond with it's own 'Version' message.
data Version = 
    Version {
              -- | Protocol version being used by the node.
              version     :: !Word32
              -- | Bitmask of features to enable for this connection.
            , services    :: !Word64
              -- | UNIX timestamp
            , timestamp   :: !Word64
              -- | Network address of the node receiving this message.
            , addrRecv    :: !NetworkAddress
              -- | Network address of the node sending this message.
            , addrSend    :: !NetworkAddress
              -- | Randomly generated identifying sent with every version
              -- message. This nonce is used to detect connection to self.
            , verNonce    :: !Word64
              -- | User agent
            , userAgent   :: !VarString
              -- | The height of the last block received by the sending node.
            , startHeight :: !Word32
              -- | Wether the remote peer should announce relaying transactions
              -- or not. This feature is enabled since version >= 70001. See
              -- BIP37 for more details.
            , relay       :: !Bool
            } deriving (Eq, Show)

instance Binary Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> get
                  <*> getWord32le
                  <*> (go =<< isEmpty)
      where 
        go True  = return True
        go False = getBool

    put (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        put         ua
        putWord32le sh
        putBool     r

getBool :: Get Bool
getBool = go =<< getWord8
  where 
    go 0 = return False
    go _ = return True

putBool :: Bool -> Put 
putBool True  = putWord8 1
putBool False = putWord8 0

