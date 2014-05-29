{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Network.Haskoin.Stratum.Conduit
( -- * Types
  Session
  -- * Functions
, initSession
, newReq
, newNotif
, reqSource
, resConduit
) where

import Prelude hiding (lines, lookup, map, null)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Exception (throw, throwIO)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (append, fromStrict, toStrict)
import Data.Conduit (Conduit, Source, ($=), (=$=), await, yield)
import Data.Conduit.List (sourceList, map)
import Data.IntMap.Strict (IntMap, delete, empty, insert, lookup, null)
import Network.Haskoin.Stratum.Exceptions
import Network.Haskoin.Stratum.Message

-- | Parse JSON-RPC ResponseValue to expected Response data type.
type ResponseParser r e v = ResponseValue -> Parser (Response r e v)

-- | Parse JSON-RPC RequestValue to expectedo Request data type.
type RequestParser j = RequestValue -> Parser (Request j)

-- | Map of ids to result parsers.
type ParserMap r e v = IntMap (ResponseParser r e v)

-- | Session state.
data Session q r e v j = Session
    (Chan q)                  -- Requests to be sent.
    (MVar Int)                -- Last id of sent request.
    (MVar (ParserMap r e v))  -- Map of parsers for response result data.
    (Maybe (RequestParser j)) -- Parser for requests or notifications.

-- | Create initial session.
initSession :: MonadIO m
            => Maybe (RequestParser j)
            -- ^ Parse incoming requests and notifications.
            -- Keep connection open.
            -> m (Session q r e v j)
initSession d = liftIO $ Session
    <$> newChan
    <*> newMVar 0
    <*> newMVar empty
    <*> return d

-- | Send a new request. Goes to a channel that is read from reqSource.
newReq :: MonadIO m
       => Session q r e v j     -- ^ Session state.
       -> (Int -> q)            -- ^ Request builder.
       -> ResponseParser r e v  -- ^ Parser for response.
       -> m q                   -- ^ Sent request.
newReq (Session rc iv pv _) f g = liftIO $ do
    i <- (+1) <$> takeMVar iv
    p <- takeMVar pv
    putMVar pv (insert i g p)
    putMVar iv i
    let q = f i
    writeChan rc q
    return q

-- | New notification, or request with no id tracking.
newNotif :: MonadIO m
         => Session q r e v j  -- ^ Session state.
         -> q                  -- ^ Request to send to the network.
         -> m ()               -- ^ No meaningful output.
newNotif (Session rc _ _ _) v = liftIO $ writeChan rc v

-- | Source of requests to send to the network.
reqSource :: (MonadIO m, ToJSON q)
          => Session q r e v j     -- ^ Session state.
          -> Source m ByteString   -- ^ Source with serialized requests.
reqSource (Session rc _ _ _) = do
    rs <- liftIO $ getChanContents rc
    sourceList rs $= map (toStrict . flip append "\n" . encode)

-- | Conduit that parses messages from network.
resConduit :: (MonadIO m)
           => Session q r e v j -- ^ Session state.
           -> Conduit ByteString m (Msg j r e v)
           -- ^ Returns Conduit with parsed data.
resConduit (Session _ _ pv d) =
    stopOnNull pv (maybe True (const False) d) =$= decodeConduit pv d

decodeConduit :: (MonadIO m)
              => MVar (ParserMap r e v)
              -> Maybe (RequestParser j)
              -> Conduit ByteString m (Msg j r e v)
decodeConduit pv d = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            p <- liftIO $ takeMVar pv
            let (i, t) = decodeMsg p d x
            case i of
                Nothing -> liftIO $ putMVar pv p
                Just n -> liftIO $ putMVar pv $ n `delete` p
            yield t
            decodeConduit pv d

stopOnNull :: MonadIO m
           => MVar (IntMap a)
           -> Bool
           -> Conduit i m i
stopOnNull pv d = do
    b <- null <$> liftIO (readMVar pv)
    unless (d && b) $ await >>= maybe e (\x -> yield x >> stopOnNull pv d)
  where
    e = liftIO . throwIO $ ConnectException "Connection closed unexpectedly."

decodeMsg :: ParserMap r e v
          -> Maybe (RequestParser j)
          -> ByteString
          -> (Maybe Int, Msg j r e v)
decodeMsg p d x =
    case y of
        MsgRequest r ->
            let m = case d of
                    Nothing -> throw $
                        NoRequestsException "No parser for requests"
                    Just l -> either (throw . ParseException) id $
                        parseEither l r
            in (Nothing, MsgRequest m)
        MsgResponse r ->
            let (i, z) = decodeRes p r
            in (Just i, MsgResponse z)
  where
    y = either (throw . ParseException) id $ eitherDecode (fromStrict x)

decodeRes :: ParserMap r e v
          -> ResponseValue
          -> (Int, (Response r e v))
decodeRes p r = (i, t)
  where
    i = case resId r of
        Just n -> either (throw . NoNumericIdException) id $ numericId n
        Nothing -> throw $ NoIdException "Id is not set."
    a = maybe (throw . ParserNotFound $ e i) id $ lookup i p
    t = either (throw . ParseException) id $ parseEither a r
    e n = "Parser not found for response id " ++ show n ++ "."
