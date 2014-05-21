{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.DbCoin 
( dbCoins
, dbBalance
, yamlCoin
, toCoin
) where

import Data.Word (Word64)
import Data.Yaml
    ( Value 
    , object 
    , (.=)
    )
import Data.Maybe (isJust, fromJust)

import Database.Persist
    ( PersistStore
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , selectList
    , (==.)
    , SelectOpt(Asc)
    )
import Database.Persist.Sqlite (SqlBackend)

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Protocol
import Network.Haskoin.Transaction

toCoin :: DbCoinGeneric b -> Coin
toCoin c = 
    Coin (TxOut (fromIntegral $ dbCoinValue c) (dbCoinScript c))
         (OutPoint (dbCoinTxid c) (fromIntegral $ dbCoinPos c))
         (dbCoinRdmScript c)

yamlCoin :: DbCoinGeneric b -> Value
yamlCoin coin = object $ concat
    [ [ "TxID"    .= (encodeTxid $ dbCoinTxid coin)
      , "Index"   .= dbCoinPos coin
      , "Value"   .= dbCoinValue coin
      , "Script"  .= dbCoinScript coin
      , "Address" .= dbCoinAddress coin
      ] 
    , if isJust $ dbCoinRdmScript coin 
        then ["Redeem" .= fromJust (dbCoinRdmScript coin)] 
        else []
    , if dbCoinOrphan coin then ["Orphan" .= True] else []
    ]

dbBalance :: (PersistQuery m, PersistMonadBackend m ~ b)
          => Entity (DbAccountGeneric b)
          -> m Word64
dbBalance (Entity ai _) = do
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinStatus  ==. Unspent
        , DbCoinOrphan  ==. False
        ] []
    return $ sum $ map (dbCoinValue . entityVal) coins

dbCoins :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) 
        => DbAccountId 
        -> m [DbCoinGeneric SqlBackend]
dbCoins ai = do
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinStatus  ==. Unspent
        , DbCoinOrphan  ==. False
        ] [Asc DbCoinCreated]
    return $ map entityVal coins


