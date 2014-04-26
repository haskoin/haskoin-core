{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Haskoin.JSONRPC.Conduit
( AppTCP
, AppContext
, runAppTCP
, newReq
, recvRes
, noMoreData
) where

import Control.Monad.State.Lazy
import Data.Conduit.Network
import qualified Data.IntMap.Strict as IntMap

-- Callbacks are actions to invoque upon responses to requests.
data AppState r m o = AppState
    { callbacks :: IntMap.IntMap (r -> m o)
    , lastID    :: Int
    }

type AppContext r m o = StateT (AppState r m o)
type AppTCP r m o = AppData IO -> AppContext r m o IO ()

initialState :: AppState r m o
initialState = AppState IntMap.empty 0

runAppTCP :: ClientSettings IO -> AppTCP r m o -> IO ()
runAppTCP cs app = runTCPClient cs (\ad -> evalStateT (app ad) initialState)

-- Create a new request. Provide a function that gets an integer ID and returns
-- a the request, as well as an action to invoque upon response to that
-- request.
newReq :: (Monad m, Monad n)
       => (Int -> a) -> (r -> m o) -> AppContext r m o n a
newReq f cb = do
    cbs <- gets callbacks
    lastid <- gets lastID
    let newid = lastid + 1
        newcbs = IntMap.insert newid cb cbs
    modify (\s -> s { callbacks = newcbs, lastID = newid })
    return (f newid)

-- Process incoming response. Provide numeric numeric ID for callback and
-- response to feed to it.
recvRes :: (Monad m, Monad n)
        => (r -> Int) -> r -> AppContext r m o n (Maybe (m o))
recvRes g x = do
    cbs <- gets callbacks
    let (cb, newcbs) = IntMap.updateLookupWithKey (\_ _ -> Nothing) (g x) cbs
    modify (\s -> s { callbacks = newcbs })
    return $ cb >>= \f -> return $ f x

-- Return True if there are no more requests awaiting response
noMoreData :: (Monad n) => AppContext r m o n Bool
noMoreData = gets callbacks >>= return . IntMap.null
