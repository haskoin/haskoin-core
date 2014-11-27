{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Control.Applicative ((<$>))
import Control.Monad (when, forM_, liftM)
import Control.Exception (SomeException(..), tryJust)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Control.Monad.State as S (StateT, evalStateT, get)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

import Data.Maybe (isJust, isNothing, fromJust)
import Data.Default (def)
import qualified Data.ByteString as BS (ByteString, append, empty)

import qualified Database.LevelDB.Base as DB 
    ( DB
    , open
    , defaultOptions
    , createIfMissing
    , cacheSize
    , get
    , put
    , withIter
    , iterFirst
    , delete
    )
import Database.LevelDB.Iterator (iterKey, iterNext)

import Database.Persist (Filter, count)
import Database.Persist.Sqlite (ConnectionPool, runSqlPersistMPool)

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

data DBSession = DBSession
    { chainHandle :: DB.DB
    , dataHandle  :: DB.DB
    , walletPool  :: ConnectionPool
    , bloomFP     :: Double
    }

type NodeHandle = S.StateT DBSession (LoggingT (ResourceT IO))

instance SPVNode DBHandle NodeHandle where
    runHeaderChain s = do
        db <- liftM chainHandle $ lift $ lift $ S.get
        liftIO $ S.evalStateT s db

    wantTxHash h = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        liftM isNothing $ liftIO $ DB.get db def $ encode' h

    haveMerkleHash h = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        liftM isJust $ liftIO $ DB.get db def $ encode' h

    rescanCleanup = do
        db <- liftM dataHandle $ lift $ lift $ S.get
        DB.withIter db def $ \iter -> DB.iterFirst iter >> go db iter
      where
        go db iter = do
            keyM <- iterKey iter
            when (isJust keyM) $ do
                DB.delete db def $ fromJust keyM
                iterNext iter
                go db iter

    spvImportTxs txs = do
        pool <- liftM walletPool $ lift $ lift $ S.get
        db <- liftM dataHandle $ lift $ lift $ S.get
        fp <- liftM bloomFP $ lift $ lift $ S.get
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ do
            before <- count ([] :: [Filter (DbAddressGeneric b)])
            forM_ txs $ \tx -> importTx tx NetworkSource Nothing
            after <- count ([] :: [Filter (DbAddressGeneric b)])
            -- Update the bloom filter if new addresses were generated
            if after > before 
                then Just <$> walletBloomFilter fp
                else return Nothing
        forM_ txs $ \tx -> (saveHash db) $ txHash tx
        case resE of
            Left err -> liftIO $ print err
            Right bloomM -> when (isJust bloomM) $ 
                processBloomFilter $ fromJust bloomM
      where
        saveHash db h = liftIO $ DB.put db def (encode' h) BS.empty
        f (SomeException e) = Just $ show e

    spvImportMerkleBlock mb expTxs = do
        db   <- liftM dataHandle $ lift $ lift $ S.get
        pool <- liftM walletPool $ lift $ lift $ S.get
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ 
            importBlock mb expTxs
        when (isLeft resE) $ liftIO $ print $ fromLeft resE
        saveHash db $ nodeBlockHash $ getActionNode mb
      where
        saveHash db h = liftIO $ DB.put db def (encode' h) BS.empty
        f (SomeException e) = Just $ show e

runNodeHandle :: Double -> ConnectionPool -> NodeHandle a -> IO a
runNodeHandle fp pool m = do
    h1 <- DB.open "headerchain"
        DB.defaultOptions{ DB.createIfMissing = True
                         , DB.cacheSize       = 2048
                         }
    h2 <- DB.open "hashdata"
        DB.defaultOptions{ DB.createIfMissing = True
                         , DB.cacheSize       = 2048
                         }
    runResourceT $ runStdoutLoggingT $ S.evalStateT m $ DBSession h1 h2 pool fp

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

