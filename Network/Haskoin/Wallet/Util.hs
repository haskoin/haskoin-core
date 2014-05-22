{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Util
( catStatus
, isWalletInit
, checkInit
, dbGetWallet
, dbGetTxBlob
) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Database.Persist

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

dbGetTxBlob :: (PersistUnique m, PersistMonadBackend m ~ b)
            => Hash256 -> m (Entity (DbTxBlobGeneric b))
dbGetTxBlob tid = do
    entM <- getBy $ UniqueTxBlob tid
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InvalidTransactionException $
            unwords ["Transaction", encodeTxid tid, "not in database"]

