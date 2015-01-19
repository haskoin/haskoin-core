module Network.Haskoin.Block.Checkpoints 
( checkpointMap
, checkpointList
, verifyCheckpoint
) where

import qualified Data.IntMap.Strict as M (IntMap, fromList, lookup)
import qualified Data.ByteString as BS (reverse)

import Network.Haskoin.Crypto
import Network.Haskoin.Network
import Network.Haskoin.Util

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as an IntMap.
checkpointMap :: Network a => a -> M.IntMap BlockHash
checkpointMap net = M.fromList (checkpointList net)

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as a list.
checkpointList :: Network a => a -> [(Int, BlockHash)]
checkpointList net = 
    map (\(a,b) -> (a,rev (b :: BlockHash))) (checkpoints net)
  where
    rev = decode' . BS.reverse . encode'

-- | Verify that a block hash at a given height either matches an existing 
-- checkpoint or is not a checkpoint. 
verifyCheckpoint :: Network a => a -> Int -> BlockHash -> Bool
verifyCheckpoint net height hash =
    case M.lookup height (checkpointMap net) of
        Just value -> hash == value
        Nothing    -> True
    
