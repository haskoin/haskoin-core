{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
-- | Implementation of basic JSON-RPC data types.
module Network.Haskoin.Stratum.RPC
( -- ** Requests
  RPCRequest
, rpcMethod
, encodeRequest
, encodeResponse
, encodeError
, encodeErrorU
, parseParams
, parseResult
, parseError
, parseRequest
, parseResponse
  -- ** Notifications
, RPCNotif
, rpcNotifMethod
, encodeNotif
, parseNotifParams
, parseNotif
  -- ** Encoding
) where

import Control.Applicative ((<$>))
import Data.Aeson.Types
import Data.Text (Text)
import Data.IntMap.Strict (IntMap)

type Method = Text
 
-- | Class for JSON-RPC requests. Defines functions to retrieve method
-- associated with a request, parse request parameters from JSON Value, and
-- create continuations to parse response result or error objects. Only
-- rpcMethod, parseParams, parseResult, and parseError need to be implemented.
class (ToJSON a, FromJSON e) => RPCRequest a e r | a -> e, a -> r
  where
    -- | Method for request. Usually one per constructor.
    rpcMethod :: a -> Method

    -- | Parse params field of request object given method.
    parseParams :: Method -> Value -> Parser a

    -- | Parse result field in response given corresponding request.
    parseResult :: a -> Value -> Parser r

    -- | Parse error field of response given corresponding request.
    parseError :: a -> Value -> Parser e

    -- | Parse JSON-RPC request.
    parseRequest :: Value -> Parser a
    parseRequest = withObject "request" $ \o -> do
        m <- o .: "method"
        p <- o .: "params"
        parseParams m p

    -- | Parse response using request.
    parseResponse :: a -> Value -> Parser (Either e r)
    parseResponse a = withObject "response" $ \o ->
        o .:? "error" .!= Null >>= \e -> case e of
            Null -> o .: "result" >>= \r -> Right <$> parseResult a r
            _ -> Left <$> parseError a e

    -- | Encode JSON-RPC request.
    encodeRequest :: a -> Int -> Value
    encodeRequest a i = object [ "jsonrpc" .= ("2.0" :: Text)
                               , "method"  .= rpcMethod a
                               , "params"  .= a
                               , "id"      .= i
                               ]

-- | Encode JSON-RPC response.
encodeResponse :: ToJSON r => r -> Int -> Value
encodeResponse x i = object [ "jsonrpc" .= ("2.0" :: Text)
                            , "result"  .= x
                            , "id"      .= i
                            ]

-- | Encode JSON-RPC error with id.
encodeError :: ToJSON e => e -> Int -> Value
encodeError e i = object [ "jsonrpc" .= ("2.0" :: Text)
                         , "error"   .= e
                         , "id"      .= i
                         ]

-- | Encode JSON-RPC error without id.
encodeErrorU :: ToJSON e => e -> Value
encodeErrorU e = object [ "jsonrpc" .= ("2.0" :: Text)
                        , "error"   .= e
                        ]


-- | Class for JSON-RPC notifications. Only rpcNotifMethod and parseNotifParams
-- need to be implemented.
class ToJSON a => RPCNotif a where
    -- | Method for notification. Usually one per constructor.

    rpcNotifMethod :: a -> Method

    -- | Parse params field of notification object given method.
    parseNotifParams :: Method -> Value -> Parser a

    -- Encode JSON-RPC notification.
    encodeNotif :: a -> Value
    encodeNotif a = object [ "jsonrpc" .= ("2.0" :: Text)
                           , "method"  .= rpcNotifMethod a
                           , "params"  .= a
                           ]

    -- | Parse notification.
    parseNotif :: Value -> Parser a
    parseNotif = withObject "notification" $ \o -> do
        m <- o .: "method"
        p <- o .: "params"
        parseNotifParams m p
