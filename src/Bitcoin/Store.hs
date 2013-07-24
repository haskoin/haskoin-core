{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Bitcoin.Store
( Store(..)
, BlockStore(..)
, IndexStore(..)
, AppStore(..)
, getBlockLocator
, getRootOf
, getChildrenOf
, getAllChildrenOf
) where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import qualified Data.Conduit as C

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.BlockChain.BlockIndex

class (Monad m, Functor m) => Store v m where 
    dbKey     :: v -> m Word256
    dbGet     :: Word256 -> m (Maybe v)
    dbPut     :: v -> m ()
    dbBest    :: m (Maybe v)
    dbPrev    :: v -> m (Maybe v)
    dbPrevKey :: v -> m Word256
    dbStream  :: C.Source m v

class Store Block m => BlockStore m where
    delBlock     :: Word256 -> m ()
    existsBlock  :: Word256 -> m Bool
    existsBlock w = do
        mVal <- dbGet w :: BlockStore m => m (Maybe Block) 
        return $ isJust mVal
    dbGetBlock   :: Word256 -> m (Maybe Block)
    dbGetBlock = dbGet
    dbStreamBlocks :: C.Source m Block
    dbStreamBlocks = dbStream

class Store BlockIndex m => IndexStore m where
    delIndex     :: Word256 -> m ()
    existsIndex  :: Word256 -> m Bool
    existsIndex w = do
        mVal <- dbGet w :: IndexStore m => m (Maybe BlockIndex) 
        return $ isJust mVal
    dbGetIndex   :: Word256 -> m (Maybe BlockIndex)
    dbGetIndex = dbGet
    dbStreamIndices :: C.Source m BlockIndex
    dbStreamIndices = dbStream

class (IndexStore m, MonadIO m) => AppStore m where
    runAppDB :: m a -> IO a

getBlockLocator :: Store v m => v -> Word256 -> m [Word256]
getBlockLocator h genesisHash = (go 1 [h]) >>= addGenesis
    where go step acc = do
              next <- move (Just $ head acc) step
              let nextStep = if (length acc) > 10 then step * 2 else 1
              case next of
                  (Just n) -> go nextStep (n:acc)
                  Nothing  -> mapM dbKey (reverse acc)
          move v step 
              | step > 0 && isJust v = do
                  next <- dbPrev $ fromJust v
                  move next (step - 1)
              | otherwise = return v
          addGenesis res
              | (last res) == genesisHash = return res
              | otherwise = return $ res ++ [genesisHash]

getRootOf :: Store v m => v -> m v
getRootOf val = go =<< dbPrev val 
    where go (Just next) = getRootOf next
          go Nothing     = return val

getAllChildrenOf :: Store v m => v -> m [v]
getAllChildrenOf val = go =<< getChildrenOf val
    where go [] = return []
          go xs = do
              rest <- forM xs getAllChildrenOf
              return $ xs ++ (concat rest)

getChildrenOf :: Store v m => v -> m [v]
getChildrenOf val = do
    key <- dbKey val
    dbStream C.$$ (go key [])
    where go key acc = do
            next <- C.await
            case next of
                (Just n) -> do
                    prev <- lift $ dbPrevKey n
                    go key (if key == prev then (n:acc) else acc)
                Nothing -> return acc
                
