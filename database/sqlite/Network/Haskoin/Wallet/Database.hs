module Network.Haskoin.Wallet.Database where

import Control.Monad.Logger (runNoLoggingT)

import Data.Text (Text)

import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (SqliteConf(..), createSqlitePool)

type DatabaseConfType = SqliteConf

databaseEngine :: Text
databaseEngine = "sqlite"

getDatabasePool :: DatabaseConfType -> IO ConnectionPool
getDatabasePool conf = runNoLoggingT $ 
    createSqlitePool (sqlDatabase conf) (sqlPoolSize conf)

paramLimit :: Int
paramLimit = 50

