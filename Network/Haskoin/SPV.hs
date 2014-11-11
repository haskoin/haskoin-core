{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  This package provides an implementation of a Bitcoin SPV node.
-}
module Network.Haskoin.SPV
( 
  -- * SPV Node running on LevelDB
  NodeHandle
, DBHandle
, runNodeHandle
) where

import System.Random (randomIO)

import Control.Applicative ((<$>))
import Control.Monad 
    ( when
    , unless
    , forM
    , forM_
    , filterM
    , foldM
    , void
    , forever
    , replicateM
    , liftM
    )
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (Async, withAsync)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State as S (StateT, evalStateT, get, gets, modify)
import Control.Monad.Logger 
    ( LoggingT
    , MonadLogger
    , runStdoutLoggingT
    , logInfo
    , logWarn
    , logDebug
    , logError
    )
import Control.Monad.Logger 
    ( LoggingT
    , MonadLogger
    , runStdoutLoggingT
    , logInfo
    , logWarn
    , logDebug
    , logError
    )

import qualified Data.Text as T (pack)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes)
import Data.Word (Word32)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Default (def)
import Data.List (nub, partition, delete, maximumBy)
import qualified Data.ByteString as BS 
    ( ByteString
    , append
    , reverse
    , empty
    )
import qualified Data.Map as M 
    ( Map
    , insert
    , member
    , delete
    , lookup
    , fromList
    , keys
    , elems
    , null
    , empty
    , partition
    )
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.Network 
    ( runTCPClient
    , clientSettings
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , newTBMChan
    , sourceTBMChan
    , writeTBMChan
    , closeTBMChan
    , (>=<)
    )

import qualified Database.LevelDB.Base as DB 
    ( DB
    , open
    , defaultOptions
    , createIfMissing
    , cacheSize
    , get
    , put
    , write
    , BatchOp( Put )
    , withIter
    , iterFirst
    , iterKeys
    , delete
    )

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

data DBSession = DBSession
    { chainHandle :: DB.DB
    , dataHandle  :: DB.DB
    }

type NodeHandle = S.StateT DBSession (LoggingT (ResourceT IO))

instance SPVNode DBHandle NodeHandle where
    runHeaderChain s = do
        db <- liftM chainHandle $ lift $ lift $ S.get
        liftIO $ S.evalStateT s db

    saveWalletHash h = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        liftIO $ DB.put db def (encode' h) BS.empty
        
    existsWalletHash h = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        liftM isJust $ liftIO $ DB.get db def $ encode' h

    clearWalletHash = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        DB.withIter db def $ \iter -> do
            DB.iterFirst iter
            keys <- DB.iterKeys iter
            forM_ keys $ DB.delete db def

runNodeHandle :: NodeHandle a -> IO a
runNodeHandle m = do
    h1 <- DB.open "headerchain"
        DB.defaultOptions{ DB.createIfMissing = True
                         , DB.cacheSize       = 2048
                         }
    h2 <- DB.open "hashdata"
        DB.defaultOptions{ DB.createIfMissing = True
                         , DB.cacheSize       = 2048
                         }
    runResourceT $ runStdoutLoggingT $ S.evalStateT m $ DBSession h1 h2

type DBHandle = S.StateT DB.DB IO

indexKey :: BlockHash -> BS.ByteString
indexKey h = "index_" `BS.append` encode' h

bestHeaderKey :: BS.ByteString
bestHeaderKey = "bestheader"

instance BlockHeaderStore DBHandle where

    getBlockHeaderNode h = do
        db  <- S.get
        res <- DB.get db def $ indexKey h
        when (isNothing res) $ error "getBlockHeaderNode: Key does not exist"
        return $ fromJust $ decodeToMaybe =<< res
        
    putBlockHeaderNode bhn = do
        db <- S.get
        DB.put db def (indexKey $ nodeBlockHash bhn) $ encode' bhn

    existsBlockHeaderNode h = do
        db <- S.get
        res <- DB.get db def $ indexKey h
        return $ isJust res
        
    getBestBlockHeader = do
        db  <- S.get
        key <- (decodeToMaybe =<<) <$> DB.get db def bestHeaderKey
        getBlockHeaderNode $ fromJust key

    setBestBlockHeader bhn = do
        db <- S.get
        DB.put db def bestHeaderKey $ encode' $ nodeBlockHash bhn
