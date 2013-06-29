module Bitcoin.MemMap
( MemMap(..)
, App
, StateSTM
, MapBlockIndex
, MapOrphanBlocks
, runStateSTM
, existsBlockIndex
, existsOrphanBlock
, alreadyHave
, addBlockIndex
, addOrphanBlock
, lookupBlockIndex
, lookupOrphanBlock
, getBestBlockIndex
, putBestBlockIndex
, getOrphanRoot
) where

import Control.Concurrent.STM

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.LevelDB

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

type StateSTM a = StateT MemMap STM a
type App a = StateT MemMap (ResourceT IO) a
type MapBlockIndex = Map Word256 BlockIndex
type MapOrphanBlocks = Map Word256 Block

data MemMap = MemMap
    { mapBlockIndex   :: TVar MapBlockIndex
    , mapOrphanBlocks :: TVar MapOrphanBlocks
    , bestBlock       :: TVar BlockIndex
    } 

runStateSTM :: StateSTM a -> App a
runStateSTM m = get >>= liftIO . atomically . (evalStateT m)

getMapBlockIndex :: StateSTM MapBlockIndex
getMapBlockIndex = get >>= lift . readTVar . mapBlockIndex

putMapBlockIndex :: MapBlockIndex -> StateSTM ()
putMapBlockIndex mbi = do
    mm <- get
    lift $ writeTVar (mapBlockIndex mm) mbi

getMapOrphanBlocks :: StateSTM MapOrphanBlocks
getMapOrphanBlocks = get >>= lift . readTVar . mapOrphanBlocks

putMapOrphanBlocks :: MapOrphanBlocks -> StateSTM ()
putMapOrphanBlocks mob = do
    mm <- get
    lift $ writeTVar (mapOrphanBlocks mm) mob

getBestBlockIndex :: StateSTM BlockIndex
getBestBlockIndex = get >>= lift . readTVar . bestBlock

putBestBlockIndex :: BlockIndex -> StateSTM ()
putBestBlockIndex bb = do
    mm <- get
    lift $ writeTVar (bestBlock mm) bb

existsBlockIndex :: Word256 -> StateSTM Bool
existsBlockIndex w = liftM (Map.member w) getMapBlockIndex

existsOrphanBlock :: Word256 -> StateSTM Bool
existsOrphanBlock w = liftM (Map.member w) getMapOrphanBlocks

alreadyHave :: Word256 -> StateSTM Bool
alreadyHave w = liftM2 (||) (existsBlockIndex w) (existsOrphanBlock w)

addBlockIndex :: BlockIndex -> StateSTM ()
addBlockIndex bi = do
    map <- getMapBlockIndex
    putMapBlockIndex $ Map.insert (biHash bi) bi map
    best <- getBestBlockIndex
    when ((biHeight bi) > (biHeight best)) (putBestBlockIndex bi)

addOrphanBlock :: Block -> StateSTM ()
addOrphanBlock ob =
    getMapOrphanBlocks >>= putMapOrphanBlocks . (Map.insert (blockHash ob) ob)

lookupBlockIndex :: Word256 -> StateSTM (Maybe BlockIndex)
lookupBlockIndex w = getMapBlockIndex >>= return . (Map.lookup w)

lookupOrphanBlock :: Word256 -> StateSTM (Maybe Block)
lookupOrphanBlock w = getMapOrphanBlocks >>= return . (Map.lookup w)

getOrphanRoot :: Block -> StateSTM Block
getOrphanRoot b = do
    let prevHash = prevBlock $ blockHeader b
    prevBlock <- lookupOrphanBlock prevHash 
    case prevBlock of
        (Just orphan) -> getOrphanRoot orphan
        Nothing       -> return b

