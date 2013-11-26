{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.DbCoin 
( cmdBalance
, cmdBalances
, cmdCoins
, cmdAllCoins
, yamlCoin
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Time
import Data.Yaml
import Data.Maybe
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Conduit as C

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Store.DbAccount
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

yamlCoin :: DbCoinGeneric b -> Value
yamlCoin coin = object $
    [ "TxID" .= dbCoinTxid coin 
    , "Index" .= dbCoinIndex coin
    , "Value" .= dbCoinValue coin
    , "Script" .= dbCoinScript coin
    , "Orphan" .= dbCoinOrphan coin
    ] ++ addrPair
    where s = maybeToEither err $ hexToBS $ dbCoinScript coin
          err = "yamlCoin: Invalid script encoding"
          addrPair = either (const []) 
                            (\a -> ["Addr" .= addrToBase58 a])
                            (scriptRecipient =<< decodeToEither =<< s)

dbBalance :: ( PersistQuery m
             , PersistMonadBackend m ~ b
             )
          => Entity (DbAccountGeneric b)
          -> EitherT String m Int
dbBalance (Entity ai acc) = do
    coins <- selectList [ DbCoinAccount ==. ai
                        , DbCoinSpent   ==. Nothing
                        , DbCoinOrphan  ==. False
                        ] []
    return $ sum $ map (dbCoinValue . entityVal) coins

cmdBalance :: (PersistUnique m, PersistQuery m) 
           => AccountName -> EitherT String m Value
cmdBalance name = do
    acc <- dbGetAcc name
    toJSON <$> dbBalance acc

cmdBalances :: PersistQuery m => EitherT String m Value
cmdBalances = do
    accs <- selectList [] []
    bals <- mapM dbBalance accs
    return $ toJSON $ map f $ zip accs bals
    where f (acc,b) = object $
            [ "Account" .= (yamlAcc $ entityVal acc)
            , "Balance" .= b
            ]

cmdCoins :: (PersistQuery m, PersistUnique m) 
         => AccountName -> EitherT String m Value
cmdCoins name = do
    (Entity ai _) <- dbGetAcc name
    coins <- selectList [ DbCoinAccount ==. ai
                        , DbCoinSpent   ==. Nothing
                        , DbCoinOrphan  ==. False
                        ] []
    return $ toJSON $ map (yamlCoin . entityVal) coins

cmdAllCoins :: PersistQuery m => EitherT String m Value
cmdAllCoins = do
    accs  <- selectList [] []
    coins <- mapM (f . entityKey) accs
    return $ toJSON $ map g $ zip accs coins
    where f ai = selectList [ DbCoinAccount ==. ai
                            , DbCoinSpent   ==. Nothing
                            , DbCoinOrphan  ==. False
                            ] []
          g (acc,cs) = object $
            [ "Account" .= (yamlAcc $ entityVal acc)
            , "Coins" .= (toJSON $ map (yamlCoin . entityVal) cs)
            ]

{-

dbImportCoin :: MonadResource m => Hash256 -> (TxOut,Word32) 
             -> WalletDB m DBCoin
dbImportCoin id (txout,i) = do
    a    <- liftEither $ scriptRecipient $ scriptOutput txout
    addr <- dbGetAddr $ AddrBase58 $ addrToBase58 a
    acc  <- dbGetAcc (AccPos $ addrAccPos addr)
    let aData   = runAccData acc
        coinPos = accCoinCount aData + 1
        coin    = DBCoin op txout False coinPos $ accPos aData
    exists <- dbExists $ concat ["coinoutpoint_", bsToString $ encode' op]
    when exists $ left "dbImportCoin: Coin already exists"
    -- update account-level count
    dbPutCoin coin
    dbPutAcc acc{ runAccData = aData{accCoinCount = coinPos} }
    -- update total count in config
    total <- dbGetConfig cfgCoinCount
    dbPutConfig $ \cfg -> cfg{ cfgCoinCount = total + 1 }
    return coin
    where op  = OutPoint id i

dbSpendCoin :: MonadResource m => TxIn -> WalletDB m DBCoin
dbSpendCoin (TxIn op s _) = do
    coin <- dbGetCoin $ CoinOutPoint op 
    when (coinSpent coin) $ left "dbSpendCoin: Coin already spent"
    dbPutCoin coin{ coinSpent = True }
    return coin

-}
