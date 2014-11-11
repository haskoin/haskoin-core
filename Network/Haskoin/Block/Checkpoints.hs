module Network.Haskoin.Block.Checkpoints 
( checkpointMap
, checkpointList
, verifyCheckpoint
) where

import qualified Data.IntMap.Strict as M (IntMap, fromList, lookup)
import qualified Data.ByteString as BS (reverse)

import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as an IntMap.
checkpointMap :: M.IntMap BlockHash
checkpointMap = M.fromList checkpointList

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as a list.
checkpointList :: [(Int, BlockHash)]
checkpointList = 
    map (\(a,b) -> (a,rev (b :: BlockHash))) checkpoints
  where
    rev = decode' . BS.reverse . encode'

-- | Verify that a block hash at a given height either matches an existing 
-- checkpoint or is not a checkpoint. 
verifyCheckpoint :: Int -> BlockHash -> Bool
verifyCheckpoint height hash = case M.lookup height checkpointMap of
    Just value -> hash == value
    Nothing    -> True
    
