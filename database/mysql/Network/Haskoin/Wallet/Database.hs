module Network.Haskoin.Wallet.Database where

import Control.Monad.Logger (runNoLoggingT)

import Database.Persist.MySQL (MySQLConf(..), createMySQLPool)
import Database.Persist.Sql (ConnectionPool)

type DatabaseConfType = MySQLConf

databaseEngine :: String
databaseEngine = "mysql"

getDatabasePool :: DatabaseConfType -> IO ConnectionPool
getDatabasePool conf = runNoLoggingT $ 
    createMySQLPool (myConnInfo conf) (myPoolSize conf)

