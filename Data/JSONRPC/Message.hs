{-# LANGUAGE OverloadedStrings #-}
module Data.JSONRPC.Message (Message(..)) where

import Prelude hiding (putStrLn)
import Data.Vector (fromList)
import Data.Aeson.Types hiding (Error)

type Method = String
type ID = Maybe Int
type Params = Value

data Message = Request  { requestID     :: ID
                        , requestMethod :: Method
                        , requestParams :: Params
                        }
             | Response { responseID    :: ID
                        , responseValue :: Value
                        }
             | Error    { errorID       :: ID
                        , errorCode     :: Int
                        , errorMessage  :: String
                        , errorData     :: Value
                        }
    deriving (Eq, Show)

instance ToJSON Message where
    toJSON (Request i method params) = object
            [ "jsonrpc" .= ("2.0" :: String)
            , ("id"      .= i)
            , ("method"  .= method)
            , ("params"  .= buildParams params)
            ]
        where
            buildParams p = case p of
                Array _ -> p
                Object _ -> p
                _ -> Array $ fromList [p]
    toJSON (Response i value) = object
            [ "jsonrpc" .= ("2.0" :: String)
            , ("id"      .= i)
            , ("result"  .= value)
            ]
    toJSON (Error i code message value) = object
            [ "jsonrpc" .= ("2.0" :: String)
            , ("id"      .= i)
            , ("error"   .= e)
            ]
        where
            e = object [ ("code"    .= code)
                       , ("message" .= message)
                       , ("data"    .= value)
                       ]
