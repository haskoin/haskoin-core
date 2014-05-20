{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.DbAccount 
( dbGetAccount
, yamlAcc
, isMSAcc
) where

import Control.Monad (when, unless)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)
import Control.Monad.Logger (MonadLogger, logErrorN)

import Data.Time (getCurrentTime)
import Data.Yaml (Value, object, (.=))
import Data.Maybe (fromJust, isJust)
import Data.List (nub)
import qualified Data.Text as T (pack)

import Database.Persist 
    ( PersistQuery
    , PersistUnique
    , PersistStore
    , PersistMonadBackend
    , Entity(..)
    , getBy
    , insert_
    , update
    , count
    , replace
    , (==.), (=.)
    )

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Crypto
import Network.Haskoin.Util

yamlAcc :: DbAccountGeneric b -> Value
yamlAcc acc = object $ concat
    [ [ "Name" .= dbAccountName acc
      , "Tree" .= dbAccountTree acc
      ]
    , datType, datWarn
    ]
    where msReq = fromJust $ dbAccountMsRequired acc
          msTot = fromJust $ dbAccountMsTotal acc
          ms    = unwords [show msReq,"of",show msTot]
          miss  = msTot - length (dbAccountMsKeys acc) - 1
          datType | isMSAcc acc = ["Type" .= unwords [ "Multisig", ms ]]
                  | otherwise   = ["Type" .= ("Regular" :: String)]
          datWarn | isMSAcc acc && miss > 0 =
                      [ (T.pack "Warning") .= 
                          unwords [show miss,"multisig keys missing"]
                      ]
                  | otherwise = []

dbGetAccount :: (MonadLogger m, PersistUnique m, PersistMonadBackend m ~ b)
         => String 
         -> m (Entity (DbAccountGeneric b))
dbGetAccount name = do
    entM <- getBy $ UniqueAccName name
    case entM of
        Just ent -> return ent
        Nothing   -> do
            logErrorN $ T.pack $ "Account " ++ name ++ " does not exist"
            liftIO $ throwIO InvalidAccountException


isMSAcc :: DbAccountGeneric b -> Bool
isMSAcc acc = (isJust $ dbAccountMsRequired acc) && 
              (isJust $ dbAccountMsTotal acc) 

