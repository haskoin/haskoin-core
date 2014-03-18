{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.JSONRPC.Message
    (
      -- Data
      Method
    , ID(..)
    , Params(..)
    , ErrorObj(..)

      -- Messages
    , Request(..)
    , Response(..)
    , Message(..)
    
    -- Helper functions
    , paramAry
    , paramObj
    
    -- Errors
    , errParse
    , errRequest
    , errMethod
    , errParams
    , errInternal
    ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson.Types

import qualified Data.HashMap.Strict    as H
import qualified Data.Text              as T
import qualified Data.Vector            as V

 
type Method = String

data ID = IntID Integer
        | TxtID T.Text
        deriving (Eq, Show)

data Params = ParamObj Object
            | ParamAry Array
            deriving (Eq, Show)

data ErrorObj = ErrorVal Value
              | ErrorObj
                  { errCode     :: Int
                  , errMessage  :: String
                  , errData     :: Value
                  }
              deriving (Eq, Show)

data Request = Request
                 { reqMethod  :: Method
                 , reqParams  :: Params
                 , reqID      :: ID
                 }
             | Notification
                 { reqMethod  :: Method
                 , reqParams  :: Params
                 }
             deriving (Eq, Show)

data Response = Response
                  { resResult :: Value
                  , resID     :: ID
                  }
              | ErrorResponse
                  { errObj    :: ErrorObj
                  , errID     :: Maybe ID
                  }
              deriving (Eq, Show)

data Message = MRequest Request
             | MResponse Response
             deriving (Eq, Show)

instance FromJSON ID where
    parseJSON t@(String _) = parseJSON t >>= return . TxtID
    parseJSON i@(Number _) = parseJSON i >>= return . IntID
    parseJSON _ = mzero

instance ToJSON ID where
    toJSON (TxtID s) = toJSON s
    toJSON (IntID i)  = toJSON i

instance FromJSON Params where
    parseJSON (Object o) = return $ ParamObj o
    parseJSON (Array a)  = return $ ParamAry a
    parseJSON _ = mzero

instance ToJSON Params where
    toJSON (ParamObj o) = toJSON o
    toJSON (ParamAry a) = toJSON a

instance FromJSON ErrorObj where
    parseJSON v@(Object o) = do
        mc <- o .:? "code"
        mm <- o .:? "message"
        d  <- o .:? "data" .!= Null
        case (mc, mm) of
            (Just c, Just m) -> return $ ErrorObj c m d
            _                -> return $ ErrorVal v
    parseJSON v = return $ ErrorVal v

instance ToJSON ErrorObj where
    toJSON (ErrorObj c m d) = object [ "code"    .= c
                                     , "message" .= m
                                     , "data"    .= d
                                     ]
    toJSON (ErrorVal v) = toJSON v

instance FromJSON Request where
    parseJSON (Object v) = do
        m <- v .:  "method"
        p <- v .:? "params" .!= paramAry []
        i <- v .:? "id" .!= Null
        case i of
            Null ->                 return $ Notification m p
            _    -> parseJSON i >>= return . Request m p
    parseJSON _ = mzero

instance ToJSON Request where
    toJSON (Request m p i) = object [ "jsonrpc" .= ("2.0" :: String)
                                    , "method"  .= m
                                    , "params"  .= p
                                    , "id"      .= i
                                    ]
    toJSON (Notification m p) = object [ "jsonrpc" .= ("2.0" :: String)
                                       , "method"  .= m
                                       , "params"  .= p
                                       ]

instance FromJSON Response where
    parseJSON (Object v) = do
        i <- v .:? "id"     .!= Null
        e <- v .:? "error"  .!= Null
        r <- v .:? "result" .!= Null
        case (r, e) of
            (Null, Null) -> mzero
            (Null, _)    -> do o <- parseJSON e
                               n <- parseJSON i
                               return $ ErrorResponse o n
            (_, Null)    -> parseJSON i >>= return . Response r
            _            -> mzero
    parseJSON _ = mzero

instance ToJSON Response where
    toJSON (Response v i) =
        object $
            [ "jsonrpc" .= ("2.0" :: String)
            , "id"      .= i
            , "result"  .= v
            ]
    toJSON (ErrorResponse e mi) =
        object $
            [ "jsonrpc" .= ("2.0" :: String)
            , "id"      .= mi
            , "error"   .= e
            ]

instance FromJSON Message where
    parseJSON o@(Object _)  =  (return . MRequest  =<< parseJSON o)
                           <|> (return . MResponse =<< parseJSON o)
    parseJSON _ = mzero

instance ToJSON Message where
    toJSON (MRequest m)  = toJSON m
    toJSON (MResponse m) = toJSON m

paramAry :: [Value] -> Params
paramAry = ParamAry . V.fromList

paramObj :: [Pair] -> Params
paramObj = ParamObj . H.fromList

errParse    :: ErrorObj
errRequest  :: ErrorObj
errMethod   :: ErrorObj
errParams   :: ErrorObj
errInternal :: ErrorObj

errParse    = ErrorObj (-32700) "Parse error"       Null
errRequest  = ErrorObj (-32600) "Invalid Request"   Null
errMethod   = ErrorObj (-32601) "Method not found"  Null
errParams   = ErrorObj (-32602) "Invalid params"    Null
errInternal = ErrorObj (-32603) "Internal error"    Null
