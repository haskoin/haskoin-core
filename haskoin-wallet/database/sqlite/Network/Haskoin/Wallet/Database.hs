module Network.Haskoin.Wallet.Database where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (Text)

import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (SqliteConf(..), createSqlitePool)

type DatabaseConfType = SqliteConf

databaseEngine :: Text
databaseEngine = "sqlite"

getDatabasePool :: (MonadLoggerIO m, MonadBaseControl IO m)
                => DatabaseConfType -> m ConnectionPool
getDatabasePool conf = createSqlitePool (sqlDatabase conf) (sqlPoolSize conf)

paramLimit :: Int
paramLimit = 20

