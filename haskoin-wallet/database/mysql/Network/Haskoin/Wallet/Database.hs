module Network.Haskoin.Wallet.Database where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Text (Text)

import Database.Persist.MySQL (MySQLConf(..), createMySQLPool)
import Database.Persist.Sql (ConnectionPool)

type DatabaseConfType = MySQLConf

databaseEngine :: Text
databaseEngine = "mysql"

getDatabasePool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
                => DatabaseConfType -> m ConnectionPool
getDatabasePool conf = createMySQLPool (myConnInfo conf) (myPoolSize conf)

paramLimit :: Int
paramLimit = 20

