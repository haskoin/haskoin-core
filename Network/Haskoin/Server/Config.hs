{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server.Config 
( ServerConfig(..)
, defaultServerConfig
, encodeFile
, decodeFile
)
where

import Control.Monad (mzero)

import Data.Yaml 
    ( ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , object
    , Value (Object)
    , encodeFile
    , decodeFile
    , (.=), (.:)
    )

data ServerConfig = ServerConfig
    { configHosts :: [(String,Int)]
    , configPort :: Int
    , configBind :: String
    } deriving (Eq, Show, Read)

defaultServerConfig = ServerConfig 
    { configHosts = [ ("127.0.0.1", 8333) ]
    , configPort  = 4000
    , configBind  = "127.0.0.1"
    }

instance ToJSON ServerConfig where
    toJSON (ServerConfig hs p a) = object
        [ "bitcoin nodes" .= map f hs
        , "server port"  .= p
        , "server bind"  .= a
        ]
      where
        f (a,b) = object
            [ "host" .= a
            , "port" .= b
            ]

instance FromJSON ServerConfig where
    parseJSON (Object o) = do
        hs <- mapM f =<< o .: "bitcoin nodes"
        p  <- o .: "server port"
        a  <- o .: "server bind"
        return $ ServerConfig hs p a
      where
        f (Object x) = do
            a <- x .: "host"
            b <- x .: "port"
            return (a,b)
        f _ = mzero
    parseJSON _ = mzero


