module Network.Haskoin.Protocol.Headers 
( Headers(..)
, BlockHeaderCount
) where

import Control.Monad (liftM2, replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.BlockHeader

-- | 'BlockHeader' type with a transaction count as 'VarInt'
type BlockHeaderCount = (BlockHeader, VarInt)

-- | The 'Headers' type is used to return a list of block headers in
-- response to a 'GetHeaders' message.
data Headers = 
    Headers { 
              -- | List of block headers with respective transaction counts
              headersList :: ![BlockHeaderCount] 
            } 
    deriving (Eq, Show)

instance Binary Headers where

    get = Headers <$> (repList =<< get)
      where 
        repList (VarInt c) = replicateM (fromIntegral c) action
        action = liftM2 (,) get get

    put (Headers xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> put a >> put b

