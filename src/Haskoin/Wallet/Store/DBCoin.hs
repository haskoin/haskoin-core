-- |This module provides a Coin type for the wallet database
module Haskoin.Wallet.Store.DBCoin
( DBCoin(..)
, CoinKey(..)
, dbGetCoin
, dbPutCoin
, dbImportTx
, dbCoinList
, dbCoinListAll
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Store.DBAddress
import Haskoin.Wallet.Store.DBAccount
import Haskoin.Wallet.Store.DBConfig
import Haskoin.Script
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

-- |Coin type used in the wallet database
data DBCoin = DBCoin{ coinOutPoint :: OutPoint
                    , coinTxOut    :: TxOut
                    , coinSpent    :: Bool
                    , coinPos      :: Int
                    , coinAccPos   :: Int
                    } deriving (Eq, Show)

-- |Key used for querying coins with dbGetCoin
-- Coins can be queried by OutPoint or by position
data CoinKey = CoinOutPoint OutPoint | CoinPos Int Int
    deriving (Eq, Show)

-- |Query a coin from the database using a CoinKey query type
dbGetCoin :: MonadResource m => CoinKey -> WalletDB m DBCoin
dbGetCoin key = case key of
    CoinOutPoint op -> do
        bs <- dbGet $ "coinoutpoint_" ++ (bsToString $ encode' op)
        f =<< (dbGet $ bsToString bs)
    CoinPos ap p -> do
        acPos <- liftEither $ dbEncodeInt ap
        coPos <- liftEither $ dbEncodeInt p
        f =<< (dbGet $ "coin_" ++ acPos ++ "_" ++ coPos)
    where f = liftEither . decodeToEither

-- |Store a coin in the wallet database
dbPutCoin :: MonadResource m => DBCoin -> WalletDB m ()
dbPutCoin coin = do
    acPos <- liftEither $ dbEncodeInt $ coinAccPos coin
    coPos <- liftEither $ dbEncodeInt $ coinPos coin
    let key = "coin_" ++ acPos ++ "_" ++ coPos
    dbPut key $ encode' coin
    dbPut ("coinoutpoint_" ++ opKey) $ stringToBS key
    where opKey = bsToString $ encode' $ coinOutPoint coin

-- |Extract coins from a transaction and save them in the database
dbImportTx :: MonadResource m => Tx -> WalletDB m [DBCoin]
dbImportTx tx = mapRights (dbImportCoin $ txid tx) $ zip (txOut tx) [0..]

dbImportCoin :: MonadResource m => Hash256 -> (TxOut,Word32) 
             -> WalletDB m DBCoin
dbImportCoin id (txout,i) = do
    a    <- liftEither $ scriptRecipient $ scriptOutput txout
    addr <- dbGetAddr $ AddrBase58 $ addrToBase58 a
    acc  <- dbGetAcc (AccPos $ addrAccPos addr)
    let aData   = runAccData acc
        coinPos = accCoinCount aData + 1
        coin    = DBCoin op txout False coinPos $ accPos aData
    exists <- dbExists $ "coinoutpoint" ++ (bsToString $ encode' op)
    when exists (liftEither $ Left "Coin already exists")
    -- update account-level count
    dbPutCoin coin
    dbPutAcc acc{ runAccData = aData{accCoinCount = coinPos} }
    -- update total count in config
    total <- dbGetConfig cfgCoinCount
    dbPutConfig $ \cfg -> cfg{ cfgCoinCount = total + 1 }
    return coin
    where op  = OutPoint id i

-- |List unspent coins from one account in the wallet database
dbCoinList :: MonadResource m => Int -> WalletDB m [DBCoin]
dbCoinList pos = do
    acc    <- dbGetAcc $ AccPos pos
    prefix <- ("coin_" ++) . (++ "_") <$> (liftEither $ dbEncodeInt pos)
    key    <- (prefix ++) <$> (liftEither $ dbEncodeInt 1)
    vals   <- dbIter key prefix $ accCoinCount $ runAccData acc
    res    <- liftEither $ forM vals decodeToEither
    return $ filter (not . coinSpent) res

-- |List unspent coins from all accounts in the wallet database
dbCoinListAll :: MonadResource m => WalletDB m [DBCoin]
dbCoinListAll = do
    total <- dbGetConfig cfgCoinCount
    key   <- (\i -> prefix ++ i ++ "_" ++ i) <$> (liftEither $ dbEncodeInt 1)
    vals  <- dbIter key prefix total
    res   <- liftEither $ forM vals decodeToEither
    return $ filter (not . coinSpent) res
    where prefix = "coin_"

-- |Binary instance for the coin type
instance Binary DBCoin where
    get = DBCoin <$> get 
                 <*> get 
                 <*> get
                 <*> (fromIntegral . getVarInt <$> get)
                 <*> (fromIntegral . getVarInt <$> get)

    put (DBCoin o t s p ap) = do
        put o >> put t >> put s
        put $ VarInt $ fromIntegral p
        put $ VarInt $ fromIntegral ap

