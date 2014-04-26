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

-- Convert a StratumRequest into a JSONRPC Request. An Int must be provided to
-- use as id for the Request.
toRequest :: StratumRequest -> Int -> Request
toRequest (ReqVersion c p) i
    = Request "server.version" (toJSON (c, p)) (IntId i)
toRequest (ReqHistory a) i
    = Request "blockchain.address.get_history" (toJSON [a]) (IntId i)

-- Pair a StratumRequest and a JSONRPC Response to obtain a StratumResponse. It
-- is suggested to partially apply to generate a function that will receive the
-- Response.
fromResponse :: StratumRequest -> Response -> Either String StratumResponse
fromResponse (ReqVersion _ _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResVersion
fromResponse (ReqHistory _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResHistory
fromResponse _ (ErrorResponse e _) = return $ ResError e

resultToEither :: Result a -> Either String a
resultToEither (Success x) = Right x
resultToEither (Error x) = Left x
