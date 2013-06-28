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
) where

import Control.Concurrent.STM

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.LevelDB

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

type StateSTM a = StateT MemMap STM a
type App a = StateT MemMap (ResourceT IO) a
type MapBlockIndex = Map Word256 BlockIndex
type MapOrphanBlocks = Map Word256 Block

data MemMap = MemMap
    { mapBlockIndex :: TVar MapBlockIndex
    , mapOrphanBlocks :: TVar MapOrphanBlocks
    } 

runStateSTM :: StateSTM a -> App a
runStateSTM m = get >>= liftIO . atomically . (evalStateT m)

getMapBlockIndex :: StateSTM MapBlockIndex
getMapBlockIndex = get >>= lift . readTVar . mapBlockIndex

getMapOrphanBlocks :: StateSTM MapOrphanBlocks
getMapOrphanBlocks = get >>= lift . readTVar . mapOrphanBlocks

existsBlockIndex :: Word256 -> StateSTM Bool
existsBlockIndex w = liftM (Map.member w) getMapBlockIndex

existsOrphanBlock :: Word256 -> StateSTM Bool
existsOrphanBlock w = liftM (Map.member w) getMapOrphanBlocks

alreadyHave :: Word256 -> StateSTM Bool
alreadyHave w = liftM2 (||) (existsBlockIndex w) (existsOrphanBlock w)
    

