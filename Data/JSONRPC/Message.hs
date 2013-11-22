{-# LANGUAGE OverloadedStrings #-}
module Data.JSONRPC.Message
( ID(..)
, Params(..)
, Method

-- Message types
, Request(..)
, Response(..)
, Message(..)
, Document(..)

-- Helper functions
, paramsArray
, paramsObject

-- Convert to version 1
, responseToVersion1
, requestToVersion1

-- Errors
, parsingError
, invalidRequest
, methodNotFound
, invalidParams
, internalError
) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import qualified Data.Text              as T
import qualified Data.HashMap.Strict    as H
import qualified Data.Vector            as V

import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
 
type Method = String

data Params = PObject Object
            | PArray Array
    deriving (Eq, Show)

data ID = IntID Integer
        | TextID T.Text
        | NullID
    deriving (Eq, Show)

data Request    = Request
                    { requestID :: ID
                    , requestMethod :: Method
                    , requestParams :: Maybe Params
                    }
                | Notification
                    { requestMethod :: Method
                    , requestParams :: Maybe Params
                    }
                | Request1
                    { requestID :: ID
                    , requestMethod :: Method
                    , requestParams :: Maybe Params
                    }
                | Notification1
                    { requestMethod :: Method
                    , requestParams :: Maybe Params
                    }
            deriving (Eq, Show)

data Response   = Response
                    { responseID :: ID
                    , responseResult :: Value
                    }
                | ErrorResponse
                    { responseID :: ID
                    , errorCode :: Int
                    , errorMessage :: String
                    , errorData :: Maybe Value
                    }
                | Response1
                    { responseID :: ID
                    , responseResult :: Value
                    }
                | ErrorResponse1
                    { responseID :: ID
                    , responseResult :: Value
                    }
            deriving (Eq, Show)

data Message    = MRequest Request
                | MResponse Response
        deriving (Eq, Show)

data Document   = Batch [Either Value Message]
                | Single (Either Value Message)
        deriving (Eq, Show)

instance FromJSON ID where
    parseJSON (Number (I i)) = return . IntID $ i
    parseJSON (Null)         = return NullID
    parseJSON (String t)     = return . TextID $ t
    parseJSON _              = mzero

instance ToJSON ID where
    toJSON (IntID  i) = toJSON i
    toJSON (NullID)   = toJSON Null
    toJSON (TextID t) = toJSON t

instance FromJSON Params where
    parseJSON (Object o) = return $ PObject o
    parseJSON (Array a)  = return $ PArray a
    parseJSON _          = mzero

instance ToJSON Params where
    toJSON (PObject o) = toJSON o
    toJSON (PArray a) = toJSON a

instance FromJSON Request where
    parseJSON (Object v) = do
        m  <- v .:  "method"
        mp <- v .:? "params"
        mi <- v .:? "id"
        mj <- v .:? "jsonrpc" :: Parser (Maybe String)
        case mj of
            Just "2.0" -> case mi of
                Nothing     -> return $ Notification m mp
                Just i      -> return $ Request    i m mp
            Nothing -> case mi of
                Nothing     -> return $ Notification1 m mp
                Just NullID -> return $ Notification1 m mp
                Just i      -> return $ Request1    i m mp
            _ -> mzero
    parseJSON _ = mzero

instance ToJSON Request where
    toJSON (Request i m mp) = object $
        [ "jsonrpc" .= ("2.0" :: String)
        , "id"      .= i
        , "method"  .= m
        ] ++ (maybeToList $ ("params".=) <$> mp)
    toJSON (Notification m mp) = object $
        [ "jsonrpc" .= ("2.0" :: String)
        , "method"  .= m
        ] ++ (maybeToList $ ("params".=) <$> mp)
    toJSON (Request1 i m mp) = object $
        [ "id"      .= i
        , "method"  .= m
        ] ++ (maybeToList $ ("params".=) <$> mp)
    toJSON (Notification1 m mp) = object $
        [ "method"  .= m
        ] ++ (maybeToList $ ("params".=) <$> mp)

instance FromJSON Response where
    parseJSON (Object v) = do
        i <- v .: "id"
        mj <- v .:? "jsonrpc" :: Parser (Maybe String)
        case mj of
            Just "2.0" -> do
                me <- v .:? "error"
                mr <- v .:? "result"
                case mr of
                    Just r -> do
                        guard $ isNothing me
                        return $ Response i r
                    Nothing -> case me of
                        Just e -> do
                            c <- e .: "code"
                            m <- e .: "message"
                            d <- e .:? "data"
                            return $ ErrorResponse i c m d
                        Nothing -> mzero
            Nothing -> do
                e <- v .:? "error"  .!= Null
                r <- v .:? "result" .!= Null
                if (r == Null)
                    then do
                        guard $ e /= Null
                        return $ ErrorResponse1 i e
                    else do
                        guard $ e == Null
                        return $ Response1 i r
            _ -> mzero
    parseJSON _ = mzero

instance ToJSON Response where
    toJSON (Response i v) =
        object $
            [ "jsonrpc" .= ("2.0" :: String)
            , "id"      .= i
            , "result"  .= v
            ]
    toJSON (ErrorResponse i c m md) =
        object $
            [ "jsonrpc" .= ("2.0" :: String)
            , "id"      .= i
            , "error"   .= e
            ]
            where e = object $  [ "code"    .= c
                                , "message" .= m
                                ] ++ (maybeToList $ ("data".=) <$> md)
    toJSON (Response1 i v) =
        object $
            [ "id"      .= i
            , "result"  .= v
            , "error"   .= Null
            ]
    toJSON (ErrorResponse1 i e) =
        object $
            [ "id"      .= i
            , "error"   .= e
            , "result"  .= Null
            ]

instance FromJSON Message where
    parseJSON o@(Object _) = (return . MRequest  =<< parseJSON o)
                         <|> (return . MResponse =<< parseJSON o)
    parseJSON _ = mzero

instance ToJSON Message where
    toJSON (MRequest m) = toJSON m
    toJSON (MResponse m) = toJSON m

instance FromJSON Document where
    parseJSON o@(Object _) = case (fromJSON o) of
        Error _ -> return . Single $ Left o
        Success m -> return . Single $ Right m
    parseJSON v@(Array a)
        | V.null a = return . Single $ Left v
        | otherwise = return . Batch . V.toList . flip V.map a $
            \o -> case (fromJSON o) of
                Error _ -> Left o
                Success m -> Right m
    parseJSON v = return . Single $ Left v

instance ToJSON Document where
    toJSON (Batch ls) = toJSON . flip map ls
        $ \x -> case x of
            Left x -> toJSON x
            Right x -> toJSON x
    toJSON (Single (Right x)) = toJSON x
    toJSON (Single (Left x)) = toJSON x

paramsArray :: [Value] -> Params
paramsArray = PArray . V.fromList

paramsObject :: [Pair] -> Params
paramsObject = PObject . H.fromList

requestToVersion1 :: Request -> Request
requestToVersion1 (Request    i m mp) = Request1    i m mp
requestToVersion1 (Notification m mp) = Notification1 m mp
requestToVersion1 r = r

responseToVersion1 :: Response -> Response
responseToVersion1 (Response i v) = Response1 i v
responseToVersion1 (ErrorResponse i c m md) =
    ErrorResponse1 i . object $
        [ "code"    .= c
        , "message" .= m
        ] ++ (maybeToList $ ("data".=) <$> md)
responseToVersion1 r = r

parsingError :: Response
parsingError = ErrorResponse
    { responseID = NullID
    , errorCode = -32700 
    , errorMessage = "Parse error"
    , errorData = Nothing
    }

invalidRequest :: Response
invalidRequest = ErrorResponse
    { responseID = NullID
    , errorCode = -32600
    , errorMessage = "Invalid Request"
    , errorData = Nothing
    }

methodNotFound :: ID -> Response
methodNotFound i = ErrorResponse
    { responseID = i
    , errorCode = -32601
    , errorMessage = "Method not found"
    , errorData = Nothing
    }

invalidParams :: ID -> Response
invalidParams i = ErrorResponse
    { responseID = i
    , errorCode = -32602
    , errorMessage = "Invalid params"
    , errorData = Nothing
    }

internalError :: ID -> Response
internalError i = ErrorResponse
    { responseID = i
    , errorCode = -32603
    , errorMessage = "Invalid params"
    , errorData = Nothing
    }
