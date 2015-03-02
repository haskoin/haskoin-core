module Network.Haskoin.Wallet.Database where

import Control.Monad.Logger (runNoLoggingT)

import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (SqliteConf(..), createSqlitePool)

type DatabaseConfType = SqliteConf

getDatabasePool :: DatabaseConfType -> IO ConnectionPool
getDatabasePool conf = runNoLoggingT $ 
    createSqlitePool (sqlDatabase conf) (sqlPoolSize conf)

