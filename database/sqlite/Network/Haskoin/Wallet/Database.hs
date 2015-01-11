module Network.Haskoin.Wallet.Database
( getDatabasePool
) where

import Control.Monad.Logger (runNoLoggingT)

import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (SqliteConf(..), createSqlitePool)

import Network.Haskoin.Wallet.Settings

getDatabasePool :: SPVConfig -> IO ConnectionPool
getDatabasePool cfg = runNoLoggingT $ createSqlitePool
    (sqlDatabase $ spvDatabase cfg)
    (sqlPoolSize $ spvDatabase cfg)

