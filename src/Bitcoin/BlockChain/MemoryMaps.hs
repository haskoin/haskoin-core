module Bitcoin.BlockChain.MemoryMaps
( MemoryMap(..)
, App
, StateSTM
, MapBlockIndex
, MapOrphanBlocks
, runStateSTM
, logString
, existsBlockIndex
, existsOrphanBlock
, alreadyHave
, addBlockIndex
, addOrphanBlock
, lookupBlockIndex
, lookupOrphanBlock
, getBestBlockIndex
, putBestBlockIndex
, initMemoryMaps
, addBlock
, getOrphanRoot
, buildBlockLocator
) where

import Data.Maybe

import Control.Concurrent.STM

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Resource

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.GetBlocks
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

type App = StateT MemoryMap (ResourceT IO)
type StateSTM = StateT MemoryMap (WriterT String STM)

type MapBlockIndex = Map Word256 BlockIndex
type MapOrphanBlocks = Map Word256 Block

data MemoryMap = MemoryMap
    { mapBlockIndex   :: TVar MapBlockIndex
    , mapOrphanBlocks :: TVar MapOrphanBlocks
    , bestBlock       :: TVar BlockIndex
    } 

runStateSTM :: StateSTM a -> App (a, String)
runStateSTM m = get >>= liftIO . atomically . runWriterT . (evalStateT m)

liftSTM :: STM a -> StateSTM a
liftSTM = lift . lift

logString :: String -> StateSTM ()
logString = lift . tell . (++ "\n")

getMapBlockIndex :: StateSTM MapBlockIndex
getMapBlockIndex = get >>= liftSTM . readTVar . mapBlockIndex

putMapBlockIndex :: MapBlockIndex -> StateSTM ()
putMapBlockIndex mbi = do
    mm <- get
    liftSTM $ writeTVar (mapBlockIndex mm) mbi

getMapOrphanBlocks :: StateSTM MapOrphanBlocks
getMapOrphanBlocks = get >>= liftSTM . readTVar . mapOrphanBlocks

putMapOrphanBlocks :: MapOrphanBlocks -> StateSTM ()
putMapOrphanBlocks mob = do
    mm <- get
    liftSTM $ writeTVar (mapOrphanBlocks mm) mob

getBestBlockIndex :: StateSTM BlockIndex
getBestBlockIndex = get >>= liftSTM . readTVar . bestBlock

putBestBlockIndex :: BlockIndex -> StateSTM ()
putBestBlockIndex bb = do
    mm <- get
    liftSTM $ writeTVar (bestBlock mm) bb

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

initMemoryMaps :: StateSTM ()
initMemoryMaps = do
    let genesisBI = buildBlockIndex testGenesisBlock Nothing
    logString $ "Indexing Genesis Block " ++ (show genesisBI)
    addBlockIndex genesisBI

addBlock :: Block -> StateSTM Bool
addBlock block = do
    let prevHash = prevBlock $ blockHeader block
    prev <- lookupBlockIndex prevHash
    case prev of
        (Just prevBlockIndex) -> do
            let newBI = buildBlockIndex block (Just prevBlockIndex)
            logString $ "Indexing new block: " ++ (show $ biHash newBI)
                ++ " at height " ++ (show $ biHeight newBI)
            addBlockIndex newBI
            processOrphansOf block
            -- todo accept orphans that depend on this one
            return True
        Nothing -> do
            logString $ "Got orphan block: " ++ (show $ blockHash block)
            addOrphanBlock block
            return False

processOrphansOf :: Block -> StateSTM ()
processOrphansOf block = do
    let hash = blockHash block
    map <- getMapOrphanBlocks
    let (toProcess, newMap) = 
            Map.partition ((== hash) . prevBlock . blockHeader) map
    putMapOrphanBlocks newMap
    when (not $ null (Map.elems toProcess)) (logString $ "Processing orphans: "
        ++ (show toProcess))
    forM_ (Map.elems toProcess) addBlock


getOrphanRoot :: Block -> StateSTM Block
getOrphanRoot b = do
    let prevHash = prevBlock $ blockHeader b
    prevBlock <- lookupOrphanBlock prevHash 
    case prevBlock of
        (Just orphan) -> getOrphanRoot orphan
        Nothing       -> return b

buildBlockLocator :: Maybe BlockIndex -> StateSTM BlockLocator
buildBlockLocator (Just h) = (go 1 [h]) >>= return . (++ [testGenesisBlockHash])
    where go step acc = do
              next <- move (Just $ head acc) step
              let nextStep = if (length acc) > 10 then step * 2 else 1
              case next of
                  (Just n) -> go nextStep (n:acc)
                  Nothing  -> return $ map biHash (reverse acc)
          move bi step 
              | step > 0 && isJust bi = do
                  next <- lookupBlockIndex (biPrev $ fromJust bi)
                  move next (step - 1)
              | otherwise = return bi
buildBlockLocator Nothing = return [testGenesisBlockHash]
              
