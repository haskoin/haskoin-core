{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Store.DbCoin 
( dbCoins
, dbBalance
, yamlCoin
, toCoin
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT)

import Data.Yaml
    ( Value 
    , object 
    , (.=)
    )
import Data.Maybe (isJust, fromJust)

import Database.Persist
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , selectList
    , (==.)
    , SelectOpt(Asc)
    )
import Database.Persist.Sqlite (SqlBackend)

import Network.Haskoin.Wallet.TxBuilder
import Network.Haskoin.Wallet.Store.Util
import Network.Haskoin.Protocol
import Network.Haskoin.Util

toCoin :: DbCoinGeneric b -> Either String Coin
toCoin c = do
    scp <- decodeScriptOps =<< maybeToEither scpErr (hexToBS $ dbCoinScript c)
    rdm <- if isJust $ dbCoinRdmScript c
        then do
            bs <- maybeToEither rdmErr $ hexToBS =<< dbCoinRdmScript c
            Just <$> decodeScriptOps bs
        else return Nothing
    h <- maybeToEither tidErr $ decodeTxid $ dbCoinTxid c
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
      , "Address" .= dbCoinAddress coin
      ] 
    , if isJust $ dbCoinRdmScript coin 
        then ["Redeem" .= fromJust (dbCoinRdmScript coin)] 
        else []
    , if dbCoinOrphan coin then ["Orphan" .= True] else []
    ]

dbBalance :: ( PersistQuery m
             , PersistMonadBackend m ~ b
             )
          => Entity (DbAccountGeneric b)
          -> EitherT String m Int
dbBalance (Entity ai _) = do
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinStatus  ==. Unspent
        , DbCoinOrphan  ==. False
        ] []
    return $ sum $ map (dbCoinValue . entityVal) coins

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
        , DbCoinStatus  ==. Unspent
        , DbCoinOrphan  ==. False
        ] [Asc DbCoinCreated]
    return $ map entityVal coins


