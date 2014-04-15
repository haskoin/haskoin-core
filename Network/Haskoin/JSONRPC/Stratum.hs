{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.JSONRPC.Stratum
( VersionRequest(..)
, HistoryRequest(..)
, VersionResponse(..)
, HistoryResponse(..)
) where

-- import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.Haskoin.Crypto
import qualified Network.Haskoin.JSONRPC as JSONRPC
import Network.Haskoin.JSONRPC.Types

{-
data Request
    = RequestVersion VersionRequest
    | RequestHistory HistoryRequest

data Response
    = ResponseVersion VersionResponse
    | ResponseHistory HistoryResponse

instance ToJSON Request where
    toJSON (RequestVersion r) = toJSON r
    toJSON (RequestHistory r) = toJSON r

instance ToJSON Response where
    toJSON (ResponseVersion r) = toJSON r
    toJSON (ResponseHistory r) = toJSON r

instance FromJSON Request where
    parseJSON o@(Object _) = v <|> h
      where
        v = return . RequestVersion =<< parseJSON o
        h = return . RequestHistory =<< parseJSON o
    parseJSON _ = mzero
-}

data VersionRequest = VersionRequest
    { clientVersion :: String
    , protoVersion :: String
    , versionReqID :: JSONRPC.ID
    } deriving (Eq, Show)

data HistoryRequest = HistoryRequest
    { historyAddress :: Address
    , historyReqID :: JSONRPC.ID
    } deriving (Eq, Show)

data VersionResponse = VersionResponse
    { serverVersion :: String
    , versionResID :: JSONRPC.ID
    } deriving (Eq, Show)

data HistoryResponse = HistoryResponse
    { historyList :: [HeightHash]
    , historyResID :: JSONRPC.ID
    } deriving (Eq, Show)

instance ToJSON VersionRequest where
    toJSON (VersionRequest c v i) = toJSON $ JSONRPC.Request
        { JSONRPC.reqMethod = "server.version"
        , JSONRPC.reqParams = toJSON [c, v]
        , JSONRPC.reqID = i
        }

instance ToJSON HistoryRequest where
    toJSON (HistoryRequest a i) = toJSON $ JSONRPC.Request
        { JSONRPC.reqMethod = "blockchain.address.get_history"
        , JSONRPC.reqParams = toJSON [a]
        , JSONRPC.reqID = i
        }

instance FromJSON VersionRequest where
    parseJSON o@(Object _) = do
        JSONRPC.Request m p i <- parseJSON o
        guard $ m == "server.version"
        (c, v) <- parseJSON p
        return $ VersionRequest c v i
    parseJSON _ = mzero

instance FromJSON HistoryRequest where
    parseJSON o@(Object _) = do
        JSONRPC.Request m p i <- parseJSON o
        guard $ m == "blockchain.address.get_history"
        [a] <- parseJSON p
        return $ HistoryRequest a i
    parseJSON _ = mzero

instance ToJSON VersionResponse where
    toJSON (VersionResponse v i) = toJSON $ JSONRPC.Response
        { JSONRPC.resResult = toJSON v
        , JSONRPC.resID = i
        }

instance ToJSON HistoryResponse where
    toJSON (HistoryResponse ls i) = toJSON $ JSONRPC.Response
        { JSONRPC.resResult = toJSON ls
        , JSONRPC.resID = i
        }

instance FromJSON VersionResponse where
    parseJSON o@(Object _) = do
        JSONRPC.Response r i <- parseJSON o
        v <- parseJSON r
        return $ VersionResponse v i
    parseJSON _ = mzero

instance FromJSON HistoryResponse where
    parseJSON o@(Object _) = do
        JSONRPC.Response r i <- parseJSON o
        ls <- parseJSON r
        return $ HistoryResponse ls i
    parseJSON _ = mzero
