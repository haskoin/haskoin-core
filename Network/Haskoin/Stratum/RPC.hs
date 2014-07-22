{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
-- | Implementation of basic JSON-RPC data types.
module Network.Haskoin.Stratum.RPC
( -- * JSON-RPC Low-Level Interface

  -- ** Requests
  RPCReq(..)
  -- *** Parsing
, FromRPCReq
, fromRPCReqParams
, parseRPCReq
  -- *** Encoding
, ToRPCReq
, rpcReqMethod
, buildRPCReq

  -- ** Responses
, RPCRes(..)
  -- *** Parsing
, FromRPCResult
, parseRPCResult
, parseRPCRes

  -- ** Notifications
, RPCNotif(..)
  -- *** Parsing
, FromRPCNotif
, fromRPCNotifParams
, parseRPCNotif
  -- *** Encoding
, ToRPCNotif
, rpcNotifMethod
, buildRPCNotif

  -- ** Errors
, RPCErr(..)
, RPCErrObj(..)
, RPCEither
  -- *** Parsing
, parseRPCErr
  -- *** Error Messages
, rpcParseError
, rpcInvalidError
, rpcParamsError
, rpcMethodError
, rpcIdError

  -- ** Other JSON-RPC Types
, RPCMsg(..)
, Method
, RPCId(..)

) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (when, guard, mzero)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


--
-- RPC Requests
--

data RPCReq q
    = RPCReq1   { getReqMethod   :: !Method
                , getReqParams   :: !q
                , getReqId       :: !RPCId
                }
    | RPCReq    { getReqMethod   :: !Method
                , getReqParams   :: !q
                , getReqId       :: !RPCId
                }
    deriving (Eq, Show, Read)

instance NFData q => NFData (RPCReq q) where
    rnf (RPCReq  m q i) = rnf m `seq` rnf q `seq` rnf i
    rnf (RPCReq1 m q i) = rnf m `seq` rnf q `seq` rnf i

instance ToJSON q => ToJSON (RPCReq q) where
    toJSON (RPCReq m p i) = object
        [jr2, "method" .= m, "params" .= p, "id" .= i]
    toJSON (RPCReq1 m p i) = object
        ["method" .= m, "params" .= p, "id" .= i]

class FromRPCReq q where
    fromRPCReqParams :: Method -> Maybe (Value -> Parser q)

instance FromRPCReq Value where
    fromRPCReqParams _ = Just return

instance FromRPCReq () where
    fromRPCReqParams _ = Nothing

parseRPCReq :: FromRPCReq q => Value -> Parser (RPCEither (RPCReq q))
parseRPCReq = withObject "request" $ \o -> do
    rM <- o .:? "jsonrpc"
    i  <- o .:  "id"
    when (i == RPCIdNull) $ fail "Request must have non-null id"
    m  <- o .:  "method"
    p  <- o .:? "params" .!= Null
    let r1 = rM /= Just ("2.0" :: Text)
    case fromRPCReqParams m of
        Nothing -> return (Left   $  rpcMethodError r1 m (Just i))
        Just  x ->        (Right <$> parseIt r1 m i x p)
               <|> return (Left   $  rpcParamsError r1 p (Just i))
  where
    parseIt r1 m i x p = x p >>= \y ->
        return $ if r1 then RPCReq1 m y i else RPCReq  m y i

class ToRPCReq q where
    rpcReqMethod :: q -> Method

instance ToRPCReq Value where
    rpcReqMethod _ = ""

instance ToRPCReq () where
    rpcReqMethod _ = undefined

buildRPCReq :: ToRPCReq q => q -> RPCReq q
buildRPCReq q = RPCReq (rpcReqMethod q) q RPCIdNull

--
-- RPC Responses
--

data RPCRes r
    = RPCRes1   { getResult    :: !r
                , getResId     :: !RPCId
                }
    | RPCRes    { getResult    :: !r
                , getResId     :: !RPCId
                }
    deriving (Eq, Show, Read)

instance NFData r => NFData (RPCRes r) where
    rnf (RPCRes1 r i) = rnf r `seq` rnf i
    rnf (RPCRes  r i) = rnf r `seq` rnf i

instance ToJSON r => ToJSON (RPCRes r) where
    toJSON (RPCRes1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (RPCRes r i) = object
        [jr2, "id" .= i, "result" .= r]

class FromRPCResult r where
    parseRPCResult :: Method -> Value -> Parser r

instance FromRPCResult Value where
    parseRPCResult _ = return

instance FromRPCResult () where
    parseRPCResult _ _ = return ()

parseRPCRes :: FromRPCResult r
            => RPCReq q -> Value -> Parser (RPCEither (RPCRes r))
parseRPCRes rq = withObject "response" $ \o -> do
    let m  = getReqMethod rq
        qi = getReqId rq
    rM <- o .:? "jsonrpc"
    i  <- o .: "id"
    when (i == RPCIdNull) $ fail "Response must have non-null id"
    when (qi /= i) $ fail "Response id mismatch"
    let r1 = rM /= Just ("2.0" :: Text)
    (Right <$> parseRes r1 i m o) <|> (Left <$> parseRPCErr (Just i) o)
  where
    parseRes r1 i m o = do
        v <- o .:? "result" .!= Null
        guard $ v /= Null
        r <- parseRPCResult m v
        return $ if r1 then RPCRes1 r i else RPCRes r i

--
-- RPC Notifications
--

data RPCNotif n
    = RPCNotif1 { getNotifMethod :: !Method
                , getNotifParams :: !n
                }
    | RPCNotif  { getNotifMethod :: !Method
                , getNotifParams :: !n
                }
    deriving (Eq, Show, Read)

instance NFData n => NFData (RPCNotif n) where
    rnf (RPCNotif  m n) = rnf m `seq` rnf n
    rnf (RPCNotif1 m n) = rnf m `seq` rnf n

instance ToJSON n => ToJSON (RPCNotif n) where
    toJSON (RPCNotif m p) = object
        [jr2, "method" .= m, "params" .= p]
    toJSON (RPCNotif1 m p) = object
        ["method" .= m, "params" .= p, "id" .= Null]

class FromRPCNotif n where
    -- | Parse params field from JSON-RPC notification for given method.
    fromRPCNotifParams :: Method -> Maybe (Value -> Parser n)

instance FromRPCNotif Value where
    fromRPCNotifParams _ = Just return

instance FromRPCNotif () where
    fromRPCNotifParams _ = Nothing

parseRPCNotif :: FromRPCNotif n => Value -> Parser (RPCEither (RPCNotif n))
parseRPCNotif = withObject "notification" $ \o -> do
    rM <- o .:? "jsonrpc"
    i  <- o .:? "id" .!= RPCIdNull
    m  <- o .:  "method"
    p  <- o .:? "params" .!= Null
    guard $ i == RPCIdNull
    let r1 = rM /= Just ("2.0" :: Text)
    case fromRPCNotifParams m of
        Nothing -> return (Left   $  rpcMethodError r1 m Nothing)
        Just  x ->        (Right <$> parseIt r1 m x p)
               <|> return (Left   $  rpcParamsError r1 p Nothing)
  where
    parseIt r1 m x p = x p >>= \y ->
        return $ if r1 then RPCNotif1 m y else RPCNotif m y

class ToRPCNotif n where
    rpcNotifMethod :: n -> Method

instance ToRPCNotif Value where
    rpcNotifMethod _ = ""

instance ToRPCNotif () where
    rpcNotifMethod _ = undefined

buildRPCNotif :: ToRPCNotif n => n -> RPCNotif n
buildRPCNotif n = RPCNotif (rpcNotifMethod n) n

--
-- RPC Errors
--

data RPCErr
    = RPCErr1   { getErrMsg    :: !String
                , getErrId     :: !(Maybe RPCId)
                }
    | RPCErr    { getErrObj    :: !RPCErrObj
                , getErrId     :: !(Maybe RPCId)
                }
    deriving (Eq, Show)

instance NFData RPCErr where
    rnf (RPCErr1 e i) = rnf e `seq` rnf i
    rnf (RPCErr  o i) = rnf o `seq` rnf i

data RPCErrObj
    = RPCErrObj { getErrObjMsg  :: !String
                , getErrObjCode :: !Int
                , getErrObjData :: !Value
                }
    deriving (Eq, Show)

instance NFData RPCErrObj where
    rnf (RPCErrObj m c d) = rnf m `seq` rnf c `seq` rnf d

instance FromJSON RPCErrObj where
    parseJSON = withObject "error" $ \o ->
        RPCErrObj <$> o .: "message"
                  <*> o .: "code"
                  <*> o .:? "data" .!= Null

instance ToJSON RPCErrObj where
    toJSON (RPCErrObj m c d) = case d of
        Null -> object ["code" .= c, "message" .= m]
        _    -> object ["code" .= c, "message" .= m, "data" .= d]

instance ToJSON RPCErr where
    toJSON (RPCErr1 e i) = object
        ["id" .= i, "error" .= e, "result" .= Null]
    toJSON (RPCErr o i) = object
        [jr2, "id" .= i, "error" .= o]

type RPCEither a = Either RPCErr a

parseRPCErr :: Maybe RPCId -> Object -> Parser RPCErr
parseRPCErr iM o = parseErr <|> parseErr1 <|> parseRes1
  where
    parseErr1 = o .: "error"  >>= \e -> return $ RPCErr1 e iM
    parseRes1 = o .: "result" >>= \e -> return $ RPCErr1 e iM
    parseErr  = o .: "error"  >>= \e -> return $ RPCErr  e iM

rpcParseError :: Bool      -- ^ RPCv1
              -> String -> RPCErr
rpcParseError r1 s = if r1
    then RPCErr1 "Parse error" Nothing
    else RPCErr (RPCErrObj "Parse error" (-32700) (toJSON s)) Nothing 

rpcInvalidError :: Bool    -- ^ RPCv1
                -> Value -> RPCErr
rpcInvalidError r1 v = if r1
    then RPCErr1 "Invalid request" Nothing
    else RPCErr (RPCErrObj "Invalid request" (-32600) v) Nothing

rpcParamsError :: Bool     -- ^ RPCv1
            -> Value -> Maybe RPCId -> RPCErr
rpcParamsError r1 p iM = if r1
    then RPCErr1 "Invalid params" iM
    else RPCErr (RPCErrObj "Invalid params" (-32602) p) iM

rpcMethodError :: Bool     -- ^ RPCv1
               -> Method -> Maybe RPCId -> RPCErr
rpcMethodError r1 m iM = if r1
    then RPCErr1 ("Method not found: " ++ T.unpack m) iM
    else RPCErr (RPCErrObj "Method not found" (-32601) (toJSON m)) iM

rpcIdError :: Bool    -- ^ RPCv1
           -> RPCId -> RPCErr
rpcIdError r1 i = if r1
  then
    RPCErr1 ("Id not recognized: " ++ L8.unpack (encode (toJSON i))) Nothing
  else
    RPCErr (RPCErrObj "Id not recognized" (-32000) (toJSON i)) Nothing

--
-- RPC Message
--

data RPCMsg q n r
    = RPCMReq   { getMReq    :: !(RPCReq   q) }
    | RPCMNotif { getMNotif  :: !(RPCNotif n) }
    | RPCMRes   { getMRes    :: !(RPCRes   r) }
    | RPCMErr   { getMErr    :: !RPCErr       }
    deriving (Eq, Show)

instance (NFData q, NFData n, NFData r) => NFData (RPCMsg q n r) where
    rnf (RPCMReq   q) = rnf q
    rnf (RPCMNotif n) = rnf n
    rnf (RPCMRes   r) = rnf r
    rnf (RPCMErr   e) = rnf e

instance (ToJSON a, ToJSON n, ToJSON r) => ToJSON (RPCMsg a n r) where
    toJSON (RPCMReq   ra) = toJSON ra
    toJSON (RPCMNotif rn) = toJSON rn
    toJSON (RPCMRes   rs) = toJSON rs
    toJSON (RPCMErr    e) = toJSON e

--
-- RPC Types
--

type Method = Text

data RPCId = RPCIdInt { getIdInt :: !Int }
           | RPCIdTxt { getIdTxt :: !Text }
           | RPCIdNull
    deriving (Eq, Show, Read, Generic)

instance Hashable RPCId

instance NFData RPCId where
    rnf (RPCIdInt i) = rnf i
    rnf (RPCIdTxt t) = rnf t
    rnf _ = ()

instance Enum RPCId where
    toEnum i = RPCIdInt i
    fromEnum (RPCIdInt i) = i
    fromEnum _ = error $ "Can't enumerate non-integral ids"

instance FromJSON RPCId where
    parseJSON s@(String _) = RPCIdTxt <$> parseJSON s
    parseJSON n@(Number _) = RPCIdInt <$> parseJSON n
    parseJSON Null = return $ RPCIdNull
    parseJSON _ = mzero

instance ToJSON RPCId where
    toJSON (RPCIdTxt s) = toJSON s
    toJSON (RPCIdInt n) = toJSON n
    toJSON RPCIdNull = Null

--
-- Helpers
--

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

