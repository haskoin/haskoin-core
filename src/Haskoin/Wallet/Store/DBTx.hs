-- |This module provides a transaction type for use in the wallet database
module Haskoin.Wallet.Store.DBTx
( DBTx(..)
, dbGetTxByID
, dbGetTxByPos
, dbPutTx
, dbTxList
, dbImportTx
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Store.DBCoin
import Haskoin.Wallet.Store.DBAccount
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

data DBTx = DBTx
    { dbTx     :: Tx
    , txPos    :: Int
    , txAccPos :: Int
    } deriving (Eq, Show)

-- |Query a transaction by it's txid
dbGetTxByID :: MonadResource m => Int -> Hash256 -> WalletDB m DBTx
dbGetTxByID pos id = do
    acPos <- liftEither $ dbEncodeInt pos
    bs <- dbGet $ concat ["txid_",acPos,"_",bsToString $ encode' id]
    liftEither . decodeToEither =<< (dbGet $ bsToString bs)

-- |Query a transaction by it's position
dbGetTxByPos :: MonadResource m => Int -> Int -> WalletDB m DBTx
dbGetTxByPos ap tp = do
    acPos <- liftEither $ dbEncodeInt ap
    txPos <- liftEither $ dbEncodeInt tp
    bs <- dbGet $ concat ["tx_",acPos,"_",txPos]
    liftEither $ decodeToEither bs

dbPutTx :: MonadResource m => DBTx -> WalletDB m ()
dbPutTx dbtx@(DBTx tx tp ap) = do
    acPos <- liftEither $ dbEncodeInt ap
    txPos <- liftEither $ dbEncodeInt tp
    let key     = concat ["tx_",acPos,"_",txPos]
        txidKey = concat ["txid_",acPos,"_",bsToString $ encode' $ txid tx]
    dbPut key $ encode' dbtx
    dbPut txidKey $ stringToBS key
     
dbTxList :: MonadResource m => Int -> WalletDB m [DBTx] 
dbTxList pos = do
    acc   <- dbGetAcc $ AccPos pos
    acPos <- liftEither $ dbEncodeInt pos
    start <- liftEither $ dbEncodeInt 1
    let prefix = concat ["tx_",acPos,"_"]
        sKey   = concat [prefix,start]
    vals <- dbIter sKey prefix $ accTxCount $ runAccData acc
    liftEither $ forM vals decodeToEither

-- |Extract coins from a transaction and save them in the database
dbImportTx :: MonadResource m => Tx -> WalletDB m [DBCoin]
dbImportTx tx = do
    newCoins   <- mapRights (dbImportCoin $ txid tx) $ zip (txOut tx) [0..]
    spentCoins <- mapRights dbSpendCoin $ txIn tx
    let xs = nubBy f $ newCoins ++ spentCoins
    forM xs $ \coin -> do
        acc <- dbGetAcc $ AccPos $ coinAccPos coin
        let aData = runAccData acc
            total = accTxCount aData
            id    = bsToString $ encode' $ txid tx
        acPos <- liftEither $ dbEncodeInt $ accPos aData
        exists <- dbExists $ concat ["txid_",acPos,"_",id]
        unless exists $ do
            dbPutTx $ DBTx tx (total + 1) $ accPos aData
            dbPutAcc acc{ runAccData = aData{ accTxCount = total + 1 } }
    return newCoins
    where f a b = (coinAccPos a) == (coinAccPos b)

instance Binary DBTx where
    get = DBTx <$> get
               <*> (fromIntegral . getVarInt <$> get)
               <*> (fromIntegral . getVarInt <$> get)

    put (DBTx t p a) = do
        put t 
        put $ VarInt $ fromIntegral p
        put $ VarInt $ fromIntegral a

