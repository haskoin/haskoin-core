{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Bitcoin.Store.STM
( Mem(..)
, STMState(..)
, newSTMState
) where

import Data.Maybe

import Control.Concurrent.STM

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex

import Bitcoin.Store

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

type MemMap v = Map Word256 v
data STMState v = STMState
    { stmMap  :: MemMap v
    , stmBest :: Maybe v  
    }
data Mem v a = Mem { runMem :: ReaderT (TVar (STMState v)) STM a }

newSTMState :: IO (TVar (STMState v))
newSTMState = newTVarIO $ STMState (Map.empty :: MemMap v) Nothing

getMap :: Mem v (MemMap v)
getMap = Mem $ do
    tvar <- ask
    state <- lift $ readTVar tvar
    return $ stmMap state

putMap :: MemMap v -> Mem v ()
putMap map = Mem $ do
    tvar <- ask
    state <- lift $ readTVar tvar
    lift $ writeTVar tvar state{ stmMap = map }

getBest :: Mem v (Maybe v)
getBest = Mem $ do
    tvar <- ask
    state <- lift $ readTVar tvar
    return $ stmBest state

putBest :: Maybe v -> Mem v ()
putBest val = Mem $ do
    tvar <- ask
    state <- lift $ readTVar tvar
    lift $ writeTVar tvar state{ stmBest = val }

instance Monad (Mem v) where

    a >>= f = Mem $ do
        val <- runMem a
        runMem $ f val

    return = Mem . return

instance Functor (Mem v) where
    fmap f m = liftM f m

instance Store BlockIndex (Mem BlockIndex) where
    dbKey bi = return $ biHash bi
    dbGet w = getMap >>= return . (Map.lookup w)
    dbPut bi = do
        map  <- getMap
        mBest <- getBest
        putMap $ Map.insert (biHash bi) bi map
        case mBest of
            (Just best) -> 
                when ((biHeight bi) > (biHeight best)) (putBest $ Just bi)
            Nothing -> putBest $ Just bi
    dbBest = getBest
    dbPrev bi = getMap >>= return . (Map.lookup (biPrev bi))
    dbPrevKey bi = return $ biPrev bi
    dbStream = (lift getMap) >>= CL.sourceList . Map.elems

instance IndexStore (Mem BlockIndex) where
    delIndex w = do
        map <- getMap
        putMap $ Map.delete w map

instance Store Block (Mem Block) where
    dbKey b = return $ blockHash b
    dbGet w = getMap >>= return . (Map.lookup w)
    dbPut b = do
        map  <- getMap
        mBest <- getBest
        putMap $ Map.insert (blockHash b) b map
        case mBest of
            (Just best) -> 
                when ((prevBlock $ blockHeader b) == (blockHash best)) 
                    (putBest $ Just b)
            Nothing -> putBest $ Just b
    dbBest = getBest
    dbPrev b = getMap >>= return . (Map.lookup (prevBlock $ blockHeader b))
    dbPrevKey b = return $ prevBlock $ blockHeader b
    dbStream = (lift getMap) >>= CL.sourceList . Map.elems

instance BlockStore (Mem Block) where
    delBlock w = do
        map <- getMap
        putMap $ Map.delete w map

