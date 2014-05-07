module Network.Haskoin.Protocol.NotFound ( NotFound(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.InvVector

-- | A 'NotFound' message is returned as a response to a 'GetData' message
-- whe one of the requested objects could not be retrieved. This could happen,
-- for example, if a tranasaction was requested and was not available in the
-- memory pool of the receiving node.
data NotFound = 
    NotFound {
             -- | Inventory vectors related to this request
               notFoundList :: ![InvVector] 
             } deriving (Eq, Show)

instance Binary NotFound where

    get = NotFound <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (NotFound xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

