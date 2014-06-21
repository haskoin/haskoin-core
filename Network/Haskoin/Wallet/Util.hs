{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Util
( catStatus
, isWalletInit
, checkInit
, dbGetWallet
, dbGetTx
, dbGetBestHeight
, dbSetBestHeight
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.Word
import Data.Maybe

import Database.Persist

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

catStatus :: [CoinStatus] -> [Hash256]
catStatus = foldr f []
  where
    f (Spent str) acc    = str:acc
    f (Reserved str) acc = str:acc
    f _ acc              = acc

isWalletInit :: PersistUnique m => String -> m Bool
isWalletInit name = do
    entM <- getBy $ UniqueWalletName name
    return $ isJust entM

checkInit :: PersistUnique m => m ()
checkInit = do
    isInit <- isWalletInit "main"
    unless isInit $ liftIO $ throwIO $ 
        InitializationException "Wallet main is not initialized"

dbGetWallet :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> m (Entity (DbWalletGeneric b))
dbGetWallet name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InitializationException $ 
            unwords ["Wallet", name, "is not initialized"]

dbGetTx :: (PersistUnique m, PersistMonadBackend m ~ b)
        => Hash256 -> m (Entity (DbTxGeneric b))
dbGetTx tid = do
    entM <- getBy $ UniqueTx tid
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InvalidTransactionException $
            unwords ["Transaction", encodeTxid tid, "not in database"]

dbGetBestHeight :: PersistQuery m => m Word32
dbGetBestHeight = do
    cnf <- selectFirst [] []
    -- TODO: throw an exception here instead of fromJust
    return $ dbConfigBestHeight $ entityVal $ fromJust cnf

dbSetBestHeight :: PersistQuery m => Word32 -> m ()
dbSetBestHeight h = updateWhere [] [DbConfigBestHeight =. h]

