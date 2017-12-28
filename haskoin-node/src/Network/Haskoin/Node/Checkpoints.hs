module Network.Haskoin.Node.Checkpoints
( checkpointMap
, checkpointList
, verifyCheckpoint
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (fromList, lookup)

import Data.Word (Word32)
import Network.Haskoin.Block
import Network.Haskoin.Constants

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as an IntMap.
checkpointMap :: Map Word32 BlockHash
checkpointMap = M.fromList checkpointList

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as a list.
checkpointList :: [(Word32, BlockHash)]
checkpointList = checkpoints

-- | Verify that a block hash at a given height either matches an existing
-- checkpoint or is not a checkpoint.
verifyCheckpoint :: Word32 -> BlockHash -> Bool
verifyCheckpoint height hash = case M.lookup height checkpointMap of
    Just value -> hash == value
    Nothing    -> True

