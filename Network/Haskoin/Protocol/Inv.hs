module Network.Haskoin.Protocol.Inv ( Inv(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Network.Haskoin.Protocol.InvVector
import Network.Haskoin.Protocol.VarInt

-- | 'Inv' messages are used by nodes to advertise their knowledge of new
-- objects by publishing a list of hashes. 'Inv' messages can be sent
-- unsolicited or in response to a 'GetBlocks' message.
data Inv = 
    Inv { 
        -- | Inventory vectors
          invList :: ![InvVector] 
        } deriving (Eq, Show)

instance Binary Inv where

    get = Inv <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (Inv xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

