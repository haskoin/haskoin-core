module Network.Haskoin.Wallet.Database
( getDatabasePool
) where

import Control.Monad.Logger (runNoLoggingT)

import Database.Persist.MySQL
    ( ConnectInfo(..)
    , defaultConnectInfo
    , createMySQLPool
    )
import Database.Persist.Sql (ConnectionPool)

import Network.Haskoin.Wallet.Settings

getDatabasePool :: SPVConfig -> IO ConnectionPool
getDatabasePool cfg = runNoLoggingT $ createMySQLPool 
    (myConnInfo $ spvDatabase cfg)
    (myPoolSize $ spvDatabase cfg)

