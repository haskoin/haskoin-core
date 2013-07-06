module Bitcoin.BlockChain.BitcoinMem
( BitcoinMem
, MemState(..)
, MapBlockIndex
, MapOrphanBlocks
, existsBlockIndex
, existsOrphanBlock
, alreadyHave
, putBlockIndexMem
, putOrphanBlock
, lookupBlockIndexMem
, lookupOrphanBlock
, getBestBlockIndex
, putBestBlockIndex
, initBitcoinMem
, putBlock
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

type BitcoinMem = StateT MemState (WriterT String STM)
type MapBlockIndex = Map Word256 BlockIndex
type MapOrphanBlocks = Map Word256 Block

data MemState = MemState
    { mapBlockIndex   :: TVar MapBlockIndex
    , mapOrphanBlocks :: TVar MapOrphanBlocks
    , bestBlock       :: TVar BlockIndex
    } 

liftSTM :: STM a -> BitcoinMem a
liftSTM = lift . lift

initBitcoinMem :: BitcoinMem ()
initBitcoinMem = do
    let genesisBI = buildBlockIndex testGenesisBlock Nothing
    tell $ "Indexing Genesis Block ... "
    putBlockIndexMem genesisBI

getMapBlockIndex :: BitcoinMem MapBlockIndex
getMapBlockIndex = get >>= liftSTM . readTVar . mapBlockIndex

putMapBlockIndex :: MapBlockIndex -> BitcoinMem ()
putMapBlockIndex mbi = do
    mm <- get
    liftSTM $ writeTVar (mapBlockIndex mm) mbi

getMapOrphanBlocks :: BitcoinMem MapOrphanBlocks
getMapOrphanBlocks = get >>= liftSTM . readTVar . mapOrphanBlocks

putMapOrphanBlocks :: MapOrphanBlocks -> BitcoinMem ()
putMapOrphanBlocks mob = do
    mm <- get
    liftSTM $ writeTVar (mapOrphanBlocks mm) mob

getBestBlockIndex :: BitcoinMem BlockIndex
getBestBlockIndex = get >>= liftSTM . readTVar . bestBlock

putBestBlockIndex :: BlockIndex -> BitcoinMem ()
putBestBlockIndex bb = do
    mm <- get
    liftSTM $ writeTVar (bestBlock mm) bb

existsBlockIndex :: Word256 -> BitcoinMem Bool
existsBlockIndex w = liftM (Map.member w) getMapBlockIndex

existsOrphanBlock :: Word256 -> BitcoinMem Bool
existsOrphanBlock w = liftM (Map.member w) getMapOrphanBlocks

alreadyHave :: Word256 -> BitcoinMem Bool
alreadyHave w = liftM2 (||) (existsBlockIndex w) (existsOrphanBlock w)

putBlockIndexMem :: BlockIndex -> BitcoinMem ()
putBlockIndexMem bi = do
    map <- getMapBlockIndex
    putMapBlockIndex $ Map.insert (biHash bi) bi map
    best <- getBestBlockIndex
    when ((biHeight bi) > (biHeight best)) (putBestBlockIndex bi)

putOrphanBlock :: Block -> BitcoinMem ()
putOrphanBlock ob =
    getMapOrphanBlocks >>= putMapOrphanBlocks . (Map.insert (blockHash ob) ob)

lookupBlockIndexMem :: Word256 -> BitcoinMem (Maybe BlockIndex)
lookupBlockIndexMem w = getMapBlockIndex >>= return . (Map.lookup w)

lookupOrphanBlock :: Word256 -> BitcoinMem (Maybe Block)
lookupOrphanBlock w = getMapOrphanBlocks >>= return . (Map.lookup w)

putBlock :: Block -> BitcoinMem [BlockIndex]
putBlock block = do
    let prevHash = prevBlock $ blockHeader block
    prev <- lookupBlockIndexMem prevHash
    case prev of
        (Just prevBlockIndex) -> do
            let newBI = buildBlockIndex block (Just prevBlockIndex)
            tell $ "Indexing new block: " ++ (show $ biHash newBI)
               ++ " at height " ++ (show $ biHeight newBI)
            putBlockIndexMem newBI
            orphanBIs <- processOrphansOf block
            return $ newBI : orphanBIs
        Nothing -> do
            tell $ "Got orphan block: " ++ (show $ blockHash block)
            putOrphanBlock block
            return []

processOrphansOf :: Block -> BitcoinMem [BlockIndex]
processOrphansOf block = do
    let hash = blockHash block
    map <- getMapOrphanBlocks
    let (toProcess, newMap) = 
            Map.partition ((== hash) . prevBlock . blockHeader) map
    putMapOrphanBlocks newMap
    when (not $ null (Map.elems toProcess)) 
        (tell $ "Processing orphans: " ++ (show toProcess))
    liftM concat $ forM (Map.elems toProcess) putBlock

getOrphanRoot :: Block -> BitcoinMem Block
getOrphanRoot b = do
    let prevHash = prevBlock $ blockHeader b
    prevBlock <- lookupOrphanBlock prevHash 
    case prevBlock of
        (Just orphan) -> getOrphanRoot orphan
        Nothing       -> return b

buildBlockLocator :: BlockIndex -> BitcoinMem BlockLocator
buildBlockLocator h = (go 1 [h]) >>= addGenesisBlock
    where go step acc = do
              next <- move (Just $ head acc) step
              let nextStep = if (length acc) > 10 then step * 2 else 1
              case next of
                  (Just n) -> go nextStep (n:acc)
                  Nothing  -> return $ map biHash (reverse acc)
          move bi step 
              | step > 0 && isJust bi = do
                  next <- lookupBlockIndexMem (biPrev $ fromJust bi)
                  move next (step - 1)
              | otherwise = return bi
          addGenesisBlock res
              | (last res) == testGenesisBlockHash = return res
              | otherwise = return $ res ++ [testGenesisBlockHash]
              
