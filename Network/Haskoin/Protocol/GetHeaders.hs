module Network.Haskoin.Protocol.GetHeaders ( GetHeaders(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.GetBlocks (BlockLocator)

import Network.Haskoin.Crypto (Hash256)

-- | Similar to the 'GetBlocks' message type but for retrieving block headers
-- only. The response to a 'GetHeaders' request is a 'Headers' message
-- containing a list of block headers pertaining to the request. A maximum of
-- 2000 block headers can be returned. 'GetHeaders' is used by thin (SPV)
-- clients to exclude block contents when synchronizing the blockchain.
data GetHeaders = 
    GetHeaders {
                 -- | The protocol version
                 getHeadersVersion  :: !Word32
                 -- | Block locator object. It is a list of block hashes from
                 -- the most recent block back to the Genesis block. The list
                 -- is dense at first and sparse towards the end.
               , getHeadersBL       :: !BlockLocator
                 -- | Hash of the last desired block header. When set to zero,
                 -- the maximum number of block headers is returned (2000)
               , getHeadersHashStop :: !Hash256
               } deriving (Eq, Show)

instance Binary GetHeaders where

    get = GetHeaders <$> getWord32le
                     <*> (repList =<< get)
                     <*> get
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetHeaders v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

