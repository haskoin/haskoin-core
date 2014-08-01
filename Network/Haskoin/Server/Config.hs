{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server.Config 
( ServerConfig(..)
, defaultServerConfig
, encodeFile
, decodeFile
)
where

import Control.Monad (mzero)
import Network.Haskoin.Constants

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
    { configHosts   :: [(String,Int)]
    , configPort    :: Int
    , configBind    :: String
    , configBatch   :: Int
    , configFpRate  :: Double
    , configOffline :: Bool
    } deriving (Eq, Show, Read)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig 
    { configHosts   = [ ("127.0.0.1", defaultPort) ]
    , configPort    = 4000
    , configBind    = "127.0.0.1"
    , configBatch   = 100
    , configFpRate  = 0.00001
    , configOffline = False
    }

instance ToJSON ServerConfig where
    toJSON (ServerConfig hs p a b fp o) = object
        [ "bitcoin nodes" .= map f hs
        , "server port"   .= p
        , "server bind"   .= a
        , "block batch"   .= b
        , "fp rate"       .= fp
        , "offline"       .= o
        ]
      where
        f (x,y) = object
            [ "host" .= x
            , "port" .= y
            ]

instance FromJSON ServerConfig where
    parseJSON (Object o) = do
        hs <- mapM f =<< o .: "bitcoin nodes"
        p  <- o .: "server port"
        a  <- o .: "server bind"
        b  <- o .: "block batch"
        fp <- o .: "fp rate"
        ol <- o .: "offline"
        return $ ServerConfig hs p a b fp ol
      where
        f (Object x) = do
            a <- x .: "host"
            b <- x .: "port"
            return (a,b)
        f _ = mzero
    parseJSON _ = mzero


