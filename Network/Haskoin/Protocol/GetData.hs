module Network.Haskoin.Protocol.GetData ( GetData(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Network.Haskoin.Protocol.InvVector
import Network.Haskoin.Protocol.VarInt

-- | The 'GetData' type is used to retrieve information on a specific object
-- ('Block' or 'Tx') identified by the objects hash. The payload of a 'GetData'
-- request is a list of 'InvVector' which represent all the hashes for which a
-- node wants to request information. The response to a 'GetBlock' message
-- wille be either a 'Block' or a 'Tx' message depending on the type of the
-- object referenced by the hash. Usually, 'GetData' messages are sent after a
-- node receives an 'Inv' message to obtain information on unknown object
-- hashes. 
data GetData = 
    GetData {
              -- | List of object hashes 
              getDataList :: ![InvVector] 
            } deriving (Eq, Show)

instance Binary GetData where

    get = GetData <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetData xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

