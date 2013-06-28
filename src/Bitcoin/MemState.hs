module Bitcoin.MemState
( MemState(..)
, App
, initMapBlockIndex
, existsBlockIndex
, getBlockIndex
, putBlockIndex
, initMapOrphanBlocks
, existsOrphanBlock
, getOrphanBlock
, putOrphanBlock
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

type App a = StateT MemState (ResourceT IO) a

newtype MapBlockIndex = MapBlockIndex (TVar (Map Word256 BlockIndex))
    
newtype MapOrphanBlocks = MapOrphanBlocks (TVar (Map Word256 Block))

data MemState = MemState
    { mapBlockIndex :: MapBlockIndex
    , mapOrphanBlocks :: MapOrphanBlocks
    } 

getMapBlockIndex :: App MapBlockIndex
getMapBlockIndex = get >>= return . mapBlockIndex

getMapOrphanBlocks :: App MapOrphanBlocks
getMapOrphanBlocks = get >>= return . mapOrphanBlocks

getMemMaps :: App (MapBlockIndex, MapOrphanBlocks)
getMemMaps = do
    mem <- get
    let mapBI = mapBlockIndex mem
        mapOB = mapOrphanBlocks mem
    return $ (mapBI, mapOB)

alreadyHave :: Word256 -> App Bool
alreadyHave w = do
    (mapBI, mapOB) <- getMemMaps
    liftIO $ atomically $ do
        a <- existsBlockIndex mapBI w 
        b <- existsOrphanBlock mapOB w
        return $ a || b

initMapBlockIndex :: IO MapBlockIndex
initMapBlockIndex = MapBlockIndex <$> newTVarIO Map.empty

existsBlockIndex :: MapBlockIndex -> Word256 -> STM Bool
existsBlockIndex (MapBlockIndex m) w = do
    map <- readTVar m
    return $ Map.member w map

getBlockIndex :: MapBlockIndex -> Word256 -> STM (Maybe BlockIndex)
getBlockIndex (MapBlockIndex m) w = do
    map <- readTVar m
    return $ Map.lookup w map

putBlockIndex :: MapBlockIndex -> BlockIndex -> STM ()
putBlockIndex (MapBlockIndex m) bi = do
    map <- readTVar m
    modifyTVar' m $ \map -> Map.insert (biHash bi) bi map

initMapOrphanBlocks :: IO MapOrphanBlocks
initMapOrphanBlocks = MapOrphanBlocks <$> newTVarIO Map.empty

existsOrphanBlock :: MapOrphanBlocks -> Word256 -> STM Bool
existsOrphanBlock (MapOrphanBlocks m) w = do
    map <- readTVar m
    return $ Map.member w map

getOrphanBlock :: MapOrphanBlocks -> Word256 -> STM (Maybe Block)
getOrphanBlock (MapOrphanBlocks m) w = do
    map <- readTVar m
    return $ Map.lookup w map

putOrphanBlock :: MapOrphanBlocks -> Block -> STM ()
putOrphanBlock (MapOrphanBlocks m) b = do
    map <- readTVar m
    modifyTVar' m $ \map -> Map.insert (blockHash b) b map


