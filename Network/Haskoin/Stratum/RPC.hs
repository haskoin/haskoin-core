{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
-- | Implementation of basic JSON-RPC data types.
module Network.Haskoin.Stratum.RPC
( -- ** Requests
  RPCRequest
, Method
, rpcMethod
, encodeRPCRequest
, encodeRPCResponse
, encodeRPCError
, encodeRPCErrorU
, parseRPCParams
, parseRPCResult
, parseRPCError
, parseRPCRequest
, parseRPCResponse
  -- ** Notifications
, RPCNotif
, rpcNotifMethod
, encodeRPCNotif
, parseRPCNotifParams
, parseRPCNotif
) where

import Data.Aeson.Types
import Data.Text (Text)

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
    parseRPCParams :: Method -> Value -> Parser a

    -- | Parse result field in response given corresponding request.
    parseRPCResult :: a -> Value -> Parser r

    -- | Parse error field of response given corresponding request.
    parseRPCError :: a -> Value -> Parser e

    -- | Parse JSON-RPC request.
    parseRPCRequest :: Value -> Parser (a, Int)
    parseRPCRequest = withObject "request" $ \o -> do
        i <- o .: "id"
        m <- o .: "method"
        p <- o .: "params"
        res <- parseRPCParams m p
        return (res, i)

    -- | Parse response using request.
    parseRPCResponse :: a -> Value -> Parser (Either e r, Int)
    parseRPCResponse a = withObject "response" $ \o -> do
        i <- o .: "id"
        o .:? "error" .!= Null >>= \e -> case e of
            Null -> o .: "result" >>= \r -> do
                res <- parseRPCResult a r
                return (Right res, i)
            _ -> do
                err <- parseRPCError a e
                return (Left err, i)

    -- | Encode JSON-RPC request.
    encodeRPCRequest :: a -> Int -> Value
    encodeRPCRequest a i = object [ "jsonrpc" .= ("2.0" :: Text)
                                  , "method"  .= rpcMethod a
                                  , "params"  .= a
                                  , "id"      .= i
                                  ]

-- | Encode JSON-RPC response.
encodeRPCResponse :: ToJSON r => r -> Int -> Value
encodeRPCResponse x i = object [ "jsonrpc" .= ("2.0" :: Text)
                               , "result"  .= x
                               , "id"      .= i
                               ]

-- | Encode JSON-RPC error with id.
encodeRPCError :: ToJSON e => e -> Int -> Value
encodeRPCError e i = object [ "jsonrpc" .= ("2.0" :: Text)
                            , "error"   .= e
                            , "id"      .= i
                            ]

-- | Encode JSON-RPC error without id.
encodeRPCErrorU :: ToJSON e => e -> Value
encodeRPCErrorU e = object [ "jsonrpc" .= ("2.0" :: Text)
                           , "error"   .= e
                           ]


-- | Class for JSON-RPC notifications. Only rpcNotifMethod and parseNotifParams
-- need to be implemented.
class ToJSON a => RPCNotif a where
    -- | Method for notification. Usually one per constructor.

    rpcNotifMethod :: a -> Method

    -- | Parse params field of notification object given method.
    parseRPCNotifParams :: Method -> Value -> Parser a

    -- Encode JSON-RPC notification.
    encodeRPCNotif :: a -> Value
    encodeRPCNotif a = object [ "jsonrpc" .= ("2.0" :: Text)
                              , "method"  .= rpcNotifMethod a
                              , "params"  .= a
                              ]

    -- | Parse notification.
    parseRPCNotif :: Value -> Parser a
    parseRPCNotif = withObject "notification" $ \o -> do
        m <- o .: "method"
        p <- o .: "params"
        parseRPCNotifParams m p
