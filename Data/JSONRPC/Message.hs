{-# LANGUAGE OverloadedStrings #-}
module Data.JSONRPC.Message
( Request(..)
, Response(..)
, Message(..)
, ID
, Method
, Params
, Value(..)
, Array
, Object
) where

import Prelude hiding (putStrLn)

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Vector (fromList)

import Data.Aeson.Types hiding (Error)
import Data.Attoparsec.Combinator
 
type ID = Maybe Int
type Method = String
type Params = Maybe Value

data Request = Request
                 { requestID :: ID
                 , requestMethod :: Method
                 , requestParams :: Params
                 }
             | Notification
                 { requestMethod :: Method
                 , requestParams :: Params
                 }
             deriving (Eq, Show)

data Response = Response
                  { responseID :: ID
                  , responseResult :: Value
                  }
              | Error
                  { errorID :: ID
                  , errorCode :: Int
                  , errorMessage :: String
                  , errorData :: Maybe Value
                  }
              deriving (Eq, Show)

data Message = MRequest   Request
             | MResponse  Response
             | BRequest   [Request]
             | BResponse  [Response]
    deriving (Eq, Show)

instance FromJSON Message where
    parseJSON v = (return . MRequest  =<< parseJSON v)
              <|> (return . MResponse =<< parseJSON v)
              <|> (return . BRequest  =<< parseJSON v)
              <|> (return . BResponse =<< parseJSON v)

instance FromJSON Request where
    parseJSON = withObject "JSONRPC Request" $ \v -> do
        j <- v .: "jsonrpc" :: Parser String
        guard (j == "2.0")
        m <- v .: "method"
        guard (length m > 0)
        mp <- v .:? "params"
        guard $ maybe True isStructured mp
        mi <- v .:? "id"
        case mi of
            Just i  -> return $ Request i m mp
            Nothing -> return $ Notification m mp

instance FromJSON Response where
    parseJSON = withObject "JSONRPC Response" $ \v -> do
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
                    return $ Error i c m md
                Nothing -> mzero

instance ToJSON Request where
    toJSON (Request i m mp) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("method"  .= m)
        ] ++ (maybeToList $ ("params".=) . forceStructured <$> mp)
    toJSON (Notification m mp) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("method"  .= m)
        ] ++ (maybeToList $ ("params".=) . forceStructured <$> mp)

instance ToJSON Response where
    toJSON (Response i v) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("result"  .= v)
        ]
    toJSON (Error i c m md) = object $
        [ ("jsonrpc" .= ("2.0" :: String))
        , ("id"      .= i)
        , ("error"   .= e)
        ]
        where e = object $
                    [ ("code"    .= c)
                    , ("message" .= m)
                    ] ++ (maybeToList $ ("data".=) <$> md)

instance ToJSON Message where
    toJSON (MResponse r) = toJSON r
    toJSON (MRequest r) =  toJSON r
    toJSON (BRequest b) =  toJSON $ map toJSON b
    toJSON (BResponse b) = toJSON $ map toJSON b

forceStructured :: Value -> Value
forceStructured v | isStructured v = v
                  | otherwise      = Array $ fromList [v]

isObject :: Value -> Bool
isObect (Object _) = True
isObject _ = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

isPrimitive :: Value -> Bool
isPrimitive p = isObject p || isArray p

isStructured :: Value -> Bool
isStructured = not . isPrimitive
