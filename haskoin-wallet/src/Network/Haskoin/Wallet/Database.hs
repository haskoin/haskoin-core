module Network.Haskoin.Wallet.Database where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)

import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (SqliteConf(..), createSqlitePool)

type DatabaseConfType = SqliteConf

getDatabasePool :: (MonadLoggerIO m, MonadUnliftIO m)
                => DatabaseConfType -> m ConnectionPool
getDatabasePool conf = createSqlitePool (sqlDatabase conf) (sqlPoolSize conf)

paramLimit :: Int
paramLimit = 20

