{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Network.Haskoin.JSONRPC.Conduit
( initSession
, newReq
, sourceThread
, responsePipe
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State.Lazy
import Data.Aeson
import Data.ByteString.Lazy.Char8 as C
import Data.Conduit as Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import qualified Data.Text as T
import Data.IntMap.Strict as IntMap
import Network.Haskoin.JSONRPC

type Callback = Response -> IO ()
type CallbackMap = IntMap Callback
data Session = Session (Chan Request) (MVar Int) (MVar CallbackMap)

initSession :: IO Session
initSession = Session <$> newChan <*> newMVar 0 <*> newMVar IntMap.empty

getId :: Response -> Int
getId r = case resId r of
    IntId i -> i
    TxtId t -> read $ T.unpack t

nullCB :: MVar CallbackMap -> IO Bool
nullCB vfs = do
    fs <- readMVar vfs
    let b = IntMap.null fs
    return b

newReq :: Session -> (Int -> Request) -> Callback -> IO ()
newReq (Session q vl vfs) r f = do
    l <- takeMVar vl
    fs <- takeMVar vfs
    let i = l + 1
    putMVar vl i
    putMVar vfs (insert i f fs)
    writeChan q (r i)

runCB :: MVar CallbackMap -> Response -> IO ()
runCB vfs r = do
    fs <- takeMVar vfs
    let i = getId r
    putMVar vfs (i `delete` fs)
    (fs ! i) r

sourceThread :: Session -> AppData IO -> IO ThreadId
sourceThread (Session q _ _) ad = forkIO $ do
    rs <- getChanContents q
    CL.sourceList rs
        $= CL.map (C.toStrict . flip C.append "\n" . encode)
        $$ appSink ad

responsePipe :: Session -> AppData IO -> IO ()
responsePipe (Session _ _ vfs) ad = appSource ad
    $$ CB.lines
    =$ filterData vfs
    =$ CL.mapMaybe (decode' . C.fromStrict)
    =$ CL.iterM (runCB vfs)
    =$ CL.sinkNull

filterData :: MVar CallbackMap -> ConduitM i i IO ()
filterData vfs = do
    b <- liftIO $ nullCB vfs
    case b of
        True -> return ()
        False -> await >>= \mx -> case mx of
            Just x -> Conduit.yield x >> filterData vfs
            Nothing -> error "connection closed with unanswered requests"
