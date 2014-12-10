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

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Logger (runNoLoggingT)

import Data.Text (pack)
import Data.Default (Default, def)
import Data.Aeson

import Database.Sqlite (open)
import Database.Persist.Sqlite 
    ( ConnectionPool
    , wrapConnection
    , createSqlPool
    )

data OptionsDB = OptionsDB
    { optWalletName :: String
    , optConnPool   :: Int
    }
  deriving (Eq, Show)

instance Default OptionsDB where
    def = OptionsDB 
        { optWalletName = "wallet"
        , optConnPool   = 1
        }

instance ToJSON OptionsDB where
    toJSON opt = object 
        [ "walletname"     .= optWalletName opt 
        , "connectionpool" .= optConnPool opt
        ]

instance FromJSON OptionsDB where
    parseJSON = withObject "optionsdb" $ \o -> OptionsDB
        <$> o .: "walletname"
        <*> o .: "connectionpool"

optionsDB :: [OptDescr (OptionsDB -> IO OptionsDB)]
optionsDB =
    [ Option [] ["sqlwallet"]
        (ReqArg (\w opts -> return opts{ optWalletName = w }) "FILE")
        "Sqlite database filename (default: wallet)"
    , Option [] ["sqlpool"]
        (ReqArg (\p opts -> return opts{ optConnPool = read p }) "INT")
        "Sqlite connection pool size (default: 1)"
    ]

haskoinDBPool :: OptionsDB -> IO ConnectionPool
haskoinDBPool opts = runNoLoggingT $ 
    createSqlPool (\lf -> open name >>= flip wrapConnection lf) size
  where
    name = pack $ optWalletName opts
    size = optConnPool opts

