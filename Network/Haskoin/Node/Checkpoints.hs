module Network.Haskoin.Node.Checkpoints
( checkpointMap
, checkpointList
, verifyCheckpoint
) where

import qualified Data.IntMap.Strict as M (IntMap, fromList, lookup)

import Network.Haskoin.Block
import Network.Haskoin.Constants

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as an IntMap.
checkpointMap :: M.IntMap BlockHash
checkpointMap = M.fromList checkpointList

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as a list.
checkpointList :: [(Int, BlockHash)]
checkpointList = checkpoints

-- | Verify that a block hash at a given height either matches an existing
-- checkpoint or is not a checkpoint.
verifyCheckpoint :: Int -> BlockHash -> Bool
verifyCheckpoint height hash = case M.lookup height checkpointMap of
    Just value -> hash == value
    Nothing    -> True

