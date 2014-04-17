{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Haskoin.JSONRPC.Conduit
( AppTCP
, AppContext
, runAppTCP
, newReq
, recvRes
) where

import Control.Monad.State.Lazy
import Data.Conduit.Network
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Traversable as DT

data AppState r m o = AppState (IntMap.IntMap (r -> m o)) Int

type AppContext r m o = StateT (AppState r m o)
type AppTCP r m o = AppData IO -> AppContext r m o IO ()

initialState :: AppState r m o
initialState = AppState IntMap.empty 0

runAppTCP :: ClientSettings IO -> AppTCP r m o -> IO ()
runAppTCP cs app = runTCPClient cs (\ad -> evalStateT (app ad) initialState)

newReq :: (Monad m, Monad n)
       => (Int -> a) -> (r -> m o) -> AppContext r m o n a
newReq f cb = do
    AppState c i <- get
    let ni = i + 1
        nc = IntMap.insert ni cb c
    put (AppState nc ni)
    return (f ni)

recvRes :: (Monad m, Monad n)
        => (r -> Int) -> r -> AppContext r m o n (m (Maybe o))
recvRes g x = do
    AppState c n <- get
    let (cb, nc) = IntMap.updateLookupWithKey (\_ _ -> Nothing) (g x) c
    put (AppState nc n)
    -- return $ maybe (return Nothing) (\f -> f x >>= return . Just) cb
    return $ DT.sequence (cb >>= \f -> return $ f x)
