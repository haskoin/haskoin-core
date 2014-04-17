{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.JSONRPC.Stratum
( StratumRequest(..)
, StratumResponse(..)
, toRequest
, fromResponse
) where

-- import Control.Applicative
import Data.Aeson
import Network.Haskoin.Crypto
import Network.Haskoin.JSONRPC
import Network.Haskoin.JSONRPC.Types

data StratumRequest
    = ReqVersion { reqVersionClient :: String, reqVersionProto :: String }
    | ReqHistory { reqAddr :: Address }
    deriving (Eq, Show)

data StratumResponse
    = ResVersion { resVersionServer :: String }
    | ResHistory { resHistory :: [HeightHash] }
    | ResError ErrorObj
    deriving (Eq, Show)

toRequest :: StratumRequest -> Int -> Request
toRequest (ReqVersion c p) i
    = Request "server.version" (toJSON (c, p)) (IntID i)
toRequest (ReqHistory a) i
    = Request "blockchain.address.get_history" (toJSON [a]) (IntID i)

fromResponse :: StratumRequest -> Response -> Either String StratumResponse
fromResponse (ReqVersion _ _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResVersion
fromResponse (ReqHistory _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResHistory
fromResponse _ (ErrorResponse e _) = return $ ResError e

resultToEither :: Result a -> Either String a
resultToEither (Success x) = Right x
resultToEither (Error x) = Left x
