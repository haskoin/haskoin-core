{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of basic JSON-RPC data types.
module Network.Haskoin.Stratum.Message
( -- * Types
  Method
, ErrorValue
, RequestValue
, ResponseValue
, MsgValue
, ResultValue
, Id(..)
, Result
, Error(..)
  -- * Messages
, Request(..)
, Response(..)
, Msg(..)
  -- * Errors
, errParse
, errReq
, errMeth
, errParams
, errInternal
, errStr
  -- * Helpers
, leftStr
, numericId
) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson.Types hiding (Result)
import Data.Text (Text, unpack)
import Text.Read (readEither)
 
-- | JSON-RPC method name.
type Method = Text
-- | JSON-RPC result.
type Result r e v = Either (Error e v) r
-- | JSON-RPC error object with default JSON values.
type ErrorValue = Error Value String
-- | JSON-RPC request with default JSON values.
type RequestValue = Request Value
-- | JSON-RPC response with default JSON values.
type ResponseValue = Response Value Value String
-- | JSON-RPC request or response with default JSON values.
type MsgValue = Msg Value Value Value String
-- | JSON-RPC result with default JSON values.
type ResultValue = Result Value Value String

-- | JSON-RPC id in text or integer form.
data Id = IntId { intId :: Int }  -- ^ Id in integer form.
        | TxtId { txtId :: Text } -- ^ Id in string form (discouraged).
        deriving (Eq, Show)

-- | JSON-RPC error object in v1 or v2 format.
-- Sent inside a Response in case of error.
data Error e v -- | Error object in JSON-RPC version 2 format.
               = ErrObj
                   { errCode :: Int      -- ^ Integer error code.
                   , errMsg :: String    -- ^ Error message.
                   , errData :: Maybe e  -- ^ Optional error object.
                   }
               -- | Error object in JSON-RPC version 1 format.
               | ErrVal
                   { errVal :: v  -- ^ Usually String.
                   }
               deriving (Eq, Show)

-- | JSON-RPC request on notification.
data Request j = Request
    { reqMethod :: Method   -- ^ Request method.
    , reqParams :: Maybe j  -- ^ Request parameters. Should be Object or Array.
    , reqId :: Maybe Id     -- ^ Request id. Nothing for notifications.
    } deriving (Eq, Show)

-- | JSON-RPC response or error.
data Response r e v = Response
    { resResult :: Result r e v -- ^ Result or error.
    , resId :: Maybe Id         -- ^ Result id.
    } deriving (Eq, Show)

-- | JSON-RPC message, can contain request or response.
data Msg j r e v
    -- | Request message container.
    = MsgRequest (Request j)
    -- | response message container.
    | MsgResponse (Response r e v)
    deriving (Eq, Show)

instance FromJSON Id where
    parseJSON (String s) = return (TxtId s)
    parseJSON (Number n) = do
        let (i, d) = properFraction n
        if d == 0.0 then return (IntId i) else mzero
    parseJSON _ = mzero

instance ToJSON Id where
    toJSON (TxtId s) = toJSON s
    toJSON (IntId i) = toJSON i

instance (FromJSON e, FromJSON v) => FromJSON (Error e v) where
    parseJSON v@(Object o) = do
        mc <- o .:? "code"
        mm <- o .:? "message"
        md <- o .:? "data"
        case (mc, mm) of
            (Just c, Just m) -> return (ErrObj c m md)
            _ -> parseJSON v >>= return . ErrVal
    parseJSON v = parseJSON v >>= return . ErrVal

instance (ToJSON e, ToJSON v) => ToJSON (Error e v) where
    toJSON (ErrObj c m d) = object
        [ "code"    .= c
        , "message" .= m
        , "data"    .= d ]
    toJSON (ErrVal v) = toJSON v

instance FromJSON j => FromJSON (Request j) where
    parseJSON (Object o) = do
        m <- o .: "method"
        p <- o .:? "params"
        i <- o .:? "id"
        return (Request m p i)
    parseJSON _ = mzero

instance ToJSON j => ToJSON (Request j) where
    toJSON (Request m p i) = object $ filter f
        [ "jsonrpc" .= ("2.0" :: String)
        , "method"  .= m
        , "params"  .= p
        , "id"      .= i ]
      where
        f (_, Null) = False
        f _ = True

instance (FromJSON r, FromJSON e, FromJSON v)
    => FromJSON (Response r e v)
  where
    parseJSON (Object o) = do
        mi <- o .: "id"
        me <- o .:? "error" .!= Nothing
        mr <- o .:? "result" .!= Nothing
        case (me, mr) of
            (Just e, _) -> return (Response (Left e) mi)
            (_, Just r) -> return (Response (Right r) mi)
            _ -> mzero
    parseJSON _ = mzero

instance (ToJSON r, ToJSON e, ToJSON v)
    => ToJSON (Response r e v)
  where
    toJSON (Response (Right r) i) = object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= i
        , "result" .= r
        ]
    toJSON (Response (Left e) i) = object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= i
        , "error" .= e
        ]

instance (FromJSON j, FromJSON r, FromJSON e, FromJSON v)
    => FromJSON (Msg j r e v)
  where
    parseJSON o@(Object _) = q <|> s
      where
        q = parseJSON o >>= return . MsgRequest
        s = parseJSON o >>= return . MsgResponse
    parseJSON _ = mzero

instance (ToJSON j, ToJSON r, ToJSON e, ToJSON v)
    => ToJSON (Msg j r e v)
  where
    toJSON (MsgRequest r) = toJSON r
    toJSON (MsgResponse r) = toJSON r

-- | Parse error in JSON-RPC v2 format.
-- Provide optional error object.
errParse :: ToJSON e => Maybe e -> Error e v
errParse = ErrObj (-32700) "Parse error"

-- | Request error in JSON-RPC v2 format.
-- Provide optional error object.
errReq :: ToJSON e => Maybe e -> Error e v
errReq = ErrObj (-32600) "Invalid request"

-- | Unknown method error in JSON-RPC v2 format.
-- Provide optional error object.
errMeth :: ToJSON e => Maybe e -> Error e v
errMeth = ErrObj (-32601) "Method not found"

-- | Invalid parameters error in JSON-RPC v2 format.
-- Provide optional error object.
errParams :: ToJSON e => Maybe e -> Error e v
errParams = ErrObj (-32602) "Invalid params"

-- | Internal error in JSON-RPC v2 format.
-- Provide optional error object.
errInternal :: ToJSON e => Maybe e -> Error e v
errInternal = ErrObj (-32606) "Internal error"

-- | Get string from error object.
errStr :: Error e Value -> String
errStr (ErrObj _ m _) = m
errStr (ErrVal v) = either id id . flip parseEither v $
    withText "error string" (return . unpack)

-- | Map Left error objects to strings.
leftStr :: Either (Error e Value) r -> Either String r
leftStr (Left e) = Left (errStr e)
leftStr (Right r) = Right r

-- | Force an id into a number or fail if not possible.
numericId :: Id -> Either String Int
numericId (IntId i) = Right i
numericId (TxtId t) = readEither $ unpack t
