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
, runNodeHandle
) where

import Control.Applicative ((<$>))
import Control.Monad (when, forM_, forM, liftM)
import Control.Exception (SomeException(..), tryJust)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Control.Monad.State as S (StateT, evalStateT, get)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

import Data.Maybe (isJust, isNothing, fromJust, catMaybes)
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

import Database.Persist.Sql (ConnectionPool, runSqlPersistMPool)

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Types

data DBSession = DBSession
    { chainHandle :: DB.DB
    , walletPool  :: ConnectionPool
    , bloomFP     :: Double
    }

type NodeHandle = S.StateT DBSession (LoggingT (ResourceT IO))

instance SPVNode LevelDBChain NodeHandle where
    runHeaderChain s = do
        db <- liftM chainHandle $ lift $ lift $ S.get
        resE <- liftIO $ tryJust f $ runLevelDBChain db s
        case resE of
            Left err -> liftIO (print err) >> undefined
            Right res -> return res
      where
        f (SomeException e) = Just $ show e

    spvImportTxs txs = do
        pool <- liftM walletPool $ lift $ lift $ S.get
        fp <- liftM bloomFP $ lift $ lift $ S.get
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ do
            xs <- forM txs $ \tx -> importTx tx NetworkSource Nothing
            -- Update the bloom filter if new addresses were generated
            if or $ map lst3 $ catMaybes xs
                then Just <$> walletBloomFilter fp
                else return Nothing
        case resE of
            Left err -> liftIO $ print err
            Right bloomM -> when (isJust bloomM) $ 
                processBloomFilter $ fromJust bloomM
      where
        f (SomeException e) = Just $ show e

    spvImportMerkleBlock mb expTxs = do
        pool <- liftM walletPool $ lift $ lift $ S.get
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ 
            importBlock mb expTxs
        when (isLeft resE) $ liftIO $ print $ fromLeft resE
      where
        f (SomeException e) = Just $ show e

runNodeHandle :: Double -> ConnectionPool -> NodeHandle a -> IO a
runNodeHandle fp pool m = do
    db <- DB.open "headerchain"
        DB.defaultOptions{ DB.createIfMissing = True
                         , DB.cacheSize       = 2048
                         }
    runResourceT $ runStdoutLoggingT $ S.evalStateT m $ DBSession db pool fp

