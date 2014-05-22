{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.DbAccount 
( dbGetAccount
, yamlAcc
, isMSAcc
) where

import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value, (.=), object)
import Data.Maybe (fromJust, isJust)
import Database.Persist  (PersistUnique, PersistMonadBackend, Entity, getBy)
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

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
                      [ "Warning" .=
                        unwords [show miss,"multisig keys missing"]
                      ]
                  | otherwise = []

dbGetAccount :: (PersistUnique m, PersistMonadBackend m ~ b)
         => String 
         -> m (Entity (DbAccountGeneric b))
dbGetAccount name = do
    entM <- getBy $ UniqueAccName name
    case entM of
        Just ent -> return ent
        Nothing   -> liftIO $ throwIO $ InvalidAccountException $ 
            unwords ["Account", name, "does not exist"]

isMSAcc :: DbAccountGeneric b -> Bool
isMSAcc acc = (isJust $ dbAccountMsRequired acc) && 
              (isJust $ dbAccountMsTotal acc) 

