module Network.Haskoin.Protocol.GetBlocks 
( GetBlocks(..) 
, BlockLocator
) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Crypto (Hash256)

type BlockLocator = [Hash256]

-- | Data type representing a GetBlocks message request. It is used in the
-- bitcoin protocol to retrieve blocks from a peer by providing it a
-- 'BlockLocator' object. The 'BlockLocator' is a sparse list of block hashes
-- from the caller node with the purpose of informing the receiving node
-- about the state of the caller's blockchain. The receiver node will detect
-- a wrong branch in the caller's main chain and send the caller appropriate 
-- 'Blocks'. The response to a 'GetBlocks' message is an 'Inv' message
-- containing the list of block hashes pertaining to the request. 
data GetBlocks = 
    GetBlocks {
                -- | The protocol version
                getBlocksVersion  :: !Word32
                -- | Block locator object. It is a list of block hashes from the
                -- most recent block back to the genesis block. The list is
                -- dense at first and sparse towards the end.
              , getBlocksLocator  :: !BlockLocator
                -- | Hash of the last desired block. If set to zero, the
                -- maximum number of block hashes is returned (500).
              , getBlocksHashStop :: !Hash256
              } deriving (Eq, Show)

instance Binary GetBlocks where

    get = GetBlocks <$> getWord32le
                    <*> (repList =<< get)
                    <*> get
      where 
        repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetBlocks v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

