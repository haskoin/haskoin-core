{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.Database 
( OptionsDB(..)
, haskoinDBPool
, optionsDB
) where

import System.Console.GetOpt 
    ( OptDescr (Option)
    , ArgDescr (ReqArg)
    )

import Control.Monad.Logger (runNoLoggingT)

import Data.Default (Default, def)
import Data.Aeson

import Database.Persist.MySQL
    ( ConnectInfo(..)
    , defaultConnectInfo
    , createMySQLPool
    )
import Database.Persist.Sql (ConnectionPool)

data OptionsDB = OptionsDB
    { optMySQL    :: ConnectInfo
    , optConnPool :: Int
    }
  deriving (Eq, Show)

instance Default OptionsDB where
    def = OptionsDB 
        { optMySQL    = defaultConnectInfo
        , optConnPool = 10
        }

instance ToJSON OptionsDB where
    toJSON opt = object 
        [ "host"           .= (connectHost $ optMySQL opt)
        , "post"           .= (connectPort $ optMySQL opt)
        , "user"           .= (connectUser $ optMySQL opt)
        , "password"       .= (connectPassword $ optMySQL opt)
        , "database"       .= (connectDatabase $ optMySQL opt)
        , "filepath"       .= (connectPath $ optMySQL opt)
        , "connectionpool" .= optConnPool opt
        ]

instance FromJSON OptionsDB where
    parseJSON = withObject "optionsdb" $ \o -> do
        h <- o .: "host"
        p <- o .: "post"
        u <- o .: "user"
        x <- o .: "password"
        d <- o .: "database"
        f <- o .: "filepath"
        pool <- o .: "connectionpool"
        let config = defaultConnectInfo
                { connectHost = h
                , connectPort = p
                , connectUser = u
                , connectPassword = x
                , connectDatabase = d
                , connectPath = f
                }
        return $ OptionsDB config pool

optionsDB :: [OptDescr (OptionsDB -> OptionsDB)]
optionsDB =
    [ Option [] ["sqlpool"]
        (ReqArg (\p opts -> opts{ optConnPool = read p }) "INT")
        "MySQL connection pool size (default: 10)"
    , Option [] ["sqlhost"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectHost = s } }) "HOST")
        "MySQL Host (Default: localhost)"
    , Option [] ["sqlport"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectPort = read s } }) "PORT")
        "MySQL Port (Default: 3306)"
    , Option [] ["sqluser"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectUser = s } }) "STRING")
        "MySQL User (Default: root)"
    , Option [] ["sqlpwd"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectPassword = s } }) "STRING")
        "MySQL Password (Default: \"\")"
    , Option [] ["sqldb"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectDatabase = s } }) "STRING")
        "MySQL Database (Default: test)"
    , Option [] ["sqlpath"] 
        (ReqArg (\s opts -> opts{ 
            optMySQL = (optMySQL opts) { connectPath = s } }) "STRING")
        "MySQL Path"
    ]

haskoinDBPool :: OptionsDB -> IO ConnectionPool
haskoinDBPool opts = runNoLoggingT $ 
    createMySQLPool (optMySQL opts) (optConnPool opts)

