{-# LANGUAGE OverloadedStrings #-}
module Data.JSONRPC.Message
( ID(..)
, Params(..)
, Method
, Request(..)
, Response(..)
, Message(..)
, Document(..)
, paramsArray
, paramsObject
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

data Request = Request
                 { requestID :: ID
                 , requestMethod :: Method
                 , requestParams :: Maybe Params
                 }
             | Notification
                 { requestMethod :: Method
                 , requestParams :: Maybe Params
                 }
    deriving (Eq, Show)

data Response = Response
                  { responseID :: ID
                  , responseResult :: Value
                  }
              | RError
                  { errorID :: ID
                  , errorCode :: Int
                  , errorMessage :: String
                  , errorData :: Maybe Value
                  }
    deriving (Eq, Show)

data Message = MRequest Request
             | MResponse Response
    deriving (Eq, Show)

data Document = Batch [Either Value Message]
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
        j <- v .: "jsonrpc" :: Parser String
        guard (j == "2.0")
        m <- v .: "method"
        mp <- v .:? "params"
        mi <- v .:? "id"
        case mi of
            Just i  -> return $ Request i m mp
            Nothing -> return $ Notification m mp
    parseJSON _ = mzero

instance ToJSON Request where
    toJSON (Request i m mp) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("method"  .= m)
        ] ++ (maybeToList $ ("params".=) <$> mp)
    toJSON (Notification m mp) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("method"  .= m)
        ] ++ (maybeToList $ ("params".=) <$> mp)

instance FromJSON Response where
    parseJSON (Object v) = do
        j <- v .: "jsonrpc" :: Parser String
        guard (j == "2.0")
        i <- v .: "id"
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
                    md <- e .:? "data"
                    return $ RError i c m md
                Nothing -> mzero
    parseJSON _ = mzero

instance ToJSON Response where
    toJSON (Response i v) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("result"  .= v)
        ]
    toJSON (RError i c m md) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("error"   .= e)
        ]
        where e = object $
                    [ ("code"    .= c)
                    , ("message" .= m)
                    ] ++ (maybeToList $ ("data".=) <$> md)

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
paramsArray v = PArray $ V.fromList v

paramsObject :: [Pair] -> Params
paramsObject p = PObject $ H.fromList p
