module Bitcoin.BlockChain.BitcoinMem
( MemState(..)
, BitcoinMem
, MapBlockIndex
, MapOrphanBlocks
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
, initBitcoinMem
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

logString :: String -> BitcoinMem ()
logString = lift . tell . (++ "\n")

initBitcoinMem :: BitcoinMem ()
initBitcoinMem = do
    let genesisBI = buildBlockIndex testGenesisBlock Nothing
    logString $ "Indexing Genesis Block " ++ (show genesisBI)
    addBlockIndex genesisBI

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

addBlockIndex :: BlockIndex -> BitcoinMem ()
addBlockIndex bi = do
    map <- getMapBlockIndex
    putMapBlockIndex $ Map.insert (biHash bi) bi map
    best <- getBestBlockIndex
    when ((biHeight bi) > (biHeight best)) (putBestBlockIndex bi)

addOrphanBlock :: Block -> BitcoinMem ()
addOrphanBlock ob =
    getMapOrphanBlocks >>= putMapOrphanBlocks . (Map.insert (blockHash ob) ob)

lookupBlockIndex :: Word256 -> BitcoinMem (Maybe BlockIndex)
lookupBlockIndex w = getMapBlockIndex >>= return . (Map.lookup w)

lookupOrphanBlock :: Word256 -> BitcoinMem (Maybe Block)
lookupOrphanBlock w = getMapOrphanBlocks >>= return . (Map.lookup w)

addBlock :: Block -> BitcoinMem Bool
addBlock block = do
    let prevHash = prevBlock $ blockHeader block
    prev <- lookupBlockIndex prevHash
    case prev of
        (Just prevBlockIndex) -> do
            let newBI = buildBlockIndex block (Just prevBlockIndex)
            --logString $ "Indexing new block: " ++ (show $ biHash newBI)
            --   ++ " at height " ++ (show $ biHeight newBI)
            addBlockIndex newBI
            processOrphansOf block
            -- todo accept orphans that depend on this one
            return True
        Nothing -> do
            logString $ "Got orphan block: " ++ (show $ blockHash block)
            addOrphanBlock block
            return False

processOrphansOf :: Block -> BitcoinMem ()
processOrphansOf block = do
    let hash = blockHash block
    map <- getMapOrphanBlocks
    let (toProcess, newMap) = 
            Map.partition ((== hash) . prevBlock . blockHeader) map
    putMapOrphanBlocks newMap
    when (not $ null (Map.elems toProcess)) (logString $ "Processing orphans: "
        ++ (show toProcess))
    forM_ (Map.elems toProcess) addBlock


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
                  next <- lookupBlockIndex (biPrev $ fromJust bi)
                  move next (step - 1)
              | otherwise = return bi
          addGenesisBlock res
              | (last res) == testGenesisBlockHash = return res
              | otherwise = return $ res ++ [testGenesisBlockHash]
              
