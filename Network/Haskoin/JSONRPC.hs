{-# LANGUAGE OverloadedStrings #-}
-- | Provides instances of ToJSON and FromJSON to use with the Aeson library.
module Network.Haskoin.JSONRPC
( -- * Data
  Method
, Id(..)
, ErrObj(..)

  -- * Messages
, JSONReq(..)
, JSONRes(..)
, JSONMsg(..)

  -- * Errors
, errParse
, errReq
, errMeth
, errParams
, errInternal
, errStr
, leftStr
) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson.Types
import Data.Text (Text)
 
-- | JSON-RPC method name.
type Method = Text

-- | JSON-RPC id in text or integer form.
data Id
    -- | Integral id
    = IntId { intId :: Int }
    -- | Text id (discouraged)
    | TxtId { txtId :: Text }
    deriving (Eq, Show)

-- | JSON-RPC error object. Sent inside a JSONRes in case of error.
data ErrObj e v
    -- | Error object in JSON-RPC version 2 format.
    = ErrObj
        { errCode :: Int      -- ^ Integer error code.
        , errMsg :: String    -- ^ Error message.
        , errData :: Maybe e  -- ^ Optional error object.
        }
    -- | Error object in JSON-RPC version 1 format, usually e would be String.
    | ErrVal { errVal :: v }
    deriving (Eq, Show)

-- | JSON-RPC request on notification.
data JSONReq j = JSONReq
    { reqMethod :: Method   -- ^ Request method.
    , reqParams :: Maybe j  -- ^ Request parameters. Should be Object or Array.
    , reqId :: Maybe Id     -- ^ Request id. Nothing for notifications.
    } deriving (Eq, Show)

-- | JSON-RPC response or error.
data JSONRes r e v = JSONRes
    { resResult :: Either (ErrObj e v) r -- ^ Result or error.
    , resId :: Maybe Id                 -- ^ Result id.
    } deriving (Eq, Show)

-- | JSON-RPC message, can contain request or response.
data JSONMsg j r e v
    -- | Request message container.
    = JSONMsgReq (JSONReq j)
    -- | response message container.
    | JSONMsgRes (JSONRes r e v)
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

instance (FromJSON e, FromJSON v) => FromJSON (ErrObj e v) where
    parseJSON v@(Object o) = do
        mc <- o .:? "code"
        mm <- o .:? "message"
        md <- o .:? "data"
        case (mc, mm) of
            (Just c, Just m) -> return (ErrObj c m md)
            _ -> parseJSON v >>= return . ErrVal
    parseJSON v = parseJSON v >>= return . ErrVal

instance (ToJSON e, ToJSON v) => ToJSON (ErrObj e v) where
    toJSON (ErrObj c m d) = object
        [ "code"    .= c
        , "message" .= m
        , "data"    .= d
        ]
    toJSON (ErrVal v) = toJSON v

instance FromJSON j => FromJSON (JSONReq j) where
    parseJSON (Object o) = do
        m <- o .: "method"
        p <- o .:? "params"
        i <- o .:? "id"
        return (JSONReq m p i)
    parseJSON _ = mzero

instance ToJSON j => ToJSON (JSONReq j) where
    toJSON (JSONReq m p i) = object $ filter f
        [ "jsonrpc" .= ("2.0" :: String)
        , "method"  .= m
        , "params"  .= p
        , "id"      .= i
        ]
      where
        f (_, Null) = False
        f _ = True

instance (FromJSON r, FromJSON e, FromJSON v) => FromJSON (JSONRes r e v) where
    parseJSON (Object o) = do
        mi <- o .: "id"
        me <- o .:? "error" .!= Nothing
        mr <- o .:? "result" .!= Nothing
        case (me, mr) of
            (Just e, _) -> return (JSONRes (Left e) mi)
            (_, Just r) -> return (JSONRes (Right r) mi)
            _ -> mzero
    parseJSON _ = mzero

instance (ToJSON r, ToJSON e, ToJSON v) => ToJSON (JSONRes r e v) where
    toJSON (JSONRes (Right r) i) = object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= i
        , "result" .= r
        ]
    toJSON (JSONRes (Left e) i) = object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= i
        , "error" .= e
        ]

instance (FromJSON j, FromJSON r, FromJSON e, FromJSON v)
    => FromJSON (JSONMsg j r e v)
  where
    parseJSON o@(Object _) = res <|> req
      where
        req = return . JSONMsgReq =<< parseJSON o
        res = return . JSONMsgRes =<< parseJSON o
    parseJSON _ = mzero

instance (ToJSON j, ToJSON r, ToJSON e, ToJSON v)
    => ToJSON (JSONMsg j r e v)
  where
    toJSON (JSONMsgReq m) = toJSON m
    toJSON (JSONMsgRes m) = toJSON m

-- | Parse error in JSON-RPC v2 format.
-- Provide optional error object.
errParse :: ToJSON e => Maybe e -> ErrObj e v
errParse = ErrObj (-32700) "Parse error"

-- | Request error in JSON-RPC v2 format.
-- Provide optional error object.
errReq :: ToJSON e => Maybe e -> ErrObj e v
errReq = ErrObj (-32600) "Invalid request"

-- | Unknown method error in JSON-RPC v2 format.
-- Provide optional error object.
errMeth :: ToJSON e => Maybe e -> ErrObj e v
errMeth = ErrObj (-32601) "Method not found"

-- | Invalid parameters error in JSON-RPC v2 format.
-- Provide optional error object.
errParams :: ToJSON e => Maybe e -> ErrObj e v
errParams = ErrObj (-32602) "Invalid params"

-- | Internal error in JSON-RPC v2 format.
-- Provide optional error object.
errInternal :: ToJSON e => Maybe e -> ErrObj e v
errInternal = ErrObj (-32606) "Internal error"

-- | Get string from error object.
errStr :: ErrObj e String -> String
errStr (ErrObj _ m _) = m
errStr (ErrVal v) = v

-- | Map Left error objects to strings.
leftStr :: Either (ErrObj e String) r -> Either String r
leftStr (Left e) = Left (errStr e)
leftStr (Right r) = Right r
