{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.DbCoin 
( cmdBalance
, cmdBalances
, dbCoins
, cmdCoins
, cmdAllCoins
, yamlCoin
, toCoin
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

toCoin :: DbCoinGeneric b -> Either String Coin
toCoin c = do
    scp <- decodeScriptOps =<< maybeToEither scpErr (hexToBS $ dbCoinScript c)
    rdm <- if isJust $ dbCoinRdmScript c
        then do
            bs <- maybeToEither rdmErr $ hexToBS =<< dbCoinRdmScript c
            Just <$> decodeScriptOps bs
        else return Nothing
    id  <- maybeToEither tidErr (hexToBS $ dbCoinTxid c)
    h   <- decodeToEither $ BS.reverse id
    return $ Coin (TxOut (fromIntegral $ dbCoinValue c) scp)
                  (OutPoint h (fromIntegral $ dbCoinPos c))
                  rdm
  where
    scpErr = "toCoin: Could not decode coin script"
    tidErr = "toCoin: Could not decode coin txid"
    rdmErr = "toCoin: Could not decode coin redeem script"

yamlCoin :: DbCoinGeneric b -> Value
yamlCoin coin = object $ concat
    [ [ "TxID"    .= dbCoinTxid coin 
      , "Index"   .= dbCoinPos coin
      , "Value"   .= dbCoinValue coin
      , "Script"  .= dbCoinScript coin
      , "Orphan"  .= dbCoinOrphan coin
      , "Address" .= dbCoinAddress coin
      ] 
    , if isJust $ dbCoinRdmScript coin 
        then ["Redeem" .= fromJust (dbCoinRdmScript coin)] 
        else []
    ]

dbBalance :: ( PersistQuery m
             , PersistMonadBackend m ~ b
             )
          => Entity (DbAccountGeneric b)
          -> EitherT String m Int
dbBalance (Entity ai acc) = do
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinSpent   ==. Nothing
        , DbCoinOrphan  ==. False
        ] []
    return $ sum $ map (dbCoinValue . entityVal) coins

cmdBalance :: (PersistUnique m, PersistQuery m) 
           => AccountName -> EitherT String m Value
cmdBalance name = toJSON <$> (dbBalance =<< dbGetAcc name)

cmdBalances :: PersistQuery m => EitherT String m Value
cmdBalances = do
    accs <- selectList [] []
    bals <- mapM dbBalance accs
    return $ toJSON $ map f $ zip accs bals
  where 
    f (acc,b) = object
        [ "Account" .= (yamlAcc $ entityVal acc)
        , "Balance" .= b
        ]

dbCoins :: ( PersistQuery m
           , PersistUnique m
           , PersistMonadBackend m ~ b
           , b ~ SqlBackend
           ) 
        => DbAccountId 
        -> EitherT String m [DbCoinGeneric b]
dbCoins ai = do
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinSpent   ==. Nothing
        , DbCoinOrphan  ==. False
        ] [Asc DbCoinCreated]
    return $ map entityVal coins

cmdCoins :: ( PersistQuery m
            , PersistUnique m
            , PersistMonadBackend m ~ SqlBackend
            ) 
         => AccountName -> EitherT String m Value
cmdCoins name = do
    (Entity ai _) <- dbGetAcc name
    coins <- dbCoins ai
    return $ toJSON $ map yamlCoin coins

cmdAllCoins :: ( PersistQuery m 
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => EitherT String m Value
cmdAllCoins = do
    accs  <- selectList [] []
    coins <- mapM (dbCoins . entityKey) accs
    return $ toJSON $ map g $ zip accs coins
  where 
    g (acc,cs) = object
        [ "Account" .= (yamlAcc $ entityVal acc)
        , "Coins" .= (toJSON $ map yamlCoin cs)
        ]

