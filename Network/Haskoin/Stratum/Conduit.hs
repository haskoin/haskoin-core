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

import Prelude hiding (lines, lookup, map, map, null)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (append, fromStrict, toStrict)
import Data.Conduit (Conduit, Source, ($=), (=$=), await, yield)
import Data.Conduit.List (sourceList, map)
import Data.IntMap.Strict (IntMap, delete, empty, insert, lookup, null)
import Network.Haskoin.Stratum.Message
import Network.Haskoin.Util (maybeToEither)

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
       -> m Int                 -- ^ Output ID of sent request.
newReq (Session rc iv pv _) f g = liftIO $ do
    i <- (+1) <$> takeMVar iv
    p <- takeMVar pv
    putMVar pv (insert i g p)
    putMVar iv i
    writeChan rc (f i)
    return i

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
           -> Conduit ByteString m (Either String (Msg j r e v))
           -- ^ Returns Conduit with parsed data or parsing errors.
resConduit (Session _ _ pv d) =
    stopOnNull pv (maybe True (const False) d) =$= decodeConduit pv d

decodeConduit :: (MonadIO m)
              => MVar (ParserMap r e v)
              -> Maybe (RequestParser j)
              -> Conduit ByteString m (Either String (Msg j r e v))
decodeConduit pv d = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            p <- liftIO $ takeMVar pv
            let m = decodeMsg p d x
            case m of
                Left e -> yield $ Left e
                Right (i, t) -> case i of
                    Nothing -> do
                        liftIO $ putMVar pv p
                        yield $ Right t
                    Just n -> do
                        liftIO $ putMVar pv $ n `delete` p
                        yield $ Right t
            decodeConduit pv d

stopOnNull :: MonadIO m
           => MVar (IntMap a)
           -> Bool
           -> Conduit i m i
stopOnNull pv d = do
    b <- null <$> liftIO (readMVar pv)
    unless (d && b) $ await >>= maybe e (\x -> yield x >> stopOnNull pv d)
  where
    e = error "Connection closed unexpectedly."

decodeMsg :: ParserMap r e v
          -> Maybe (RequestParser j)
          -> ByteString
          -> Either String (Maybe Int, Msg j r e v)
decodeMsg p d x = do
    y <- eitherDecode (fromStrict x)
    case y of
        MsgRequest r -> do
            m <- case d of
                Nothing -> Left "No parser for requests"
                Just l -> parseEither l r
            return $ (Nothing, MsgRequest m)
        MsgResponse r -> do
            (i, z) <- decodeRes p r
            return $ (Just i, MsgResponse z)

decodeRes :: ParserMap r e v
          -> ResponseValue
          -> Either String (Int, (Response r e v))
decodeRes p r = do
    i <- case resId r of
        Just n -> numericId n
        Nothing -> Left "Id is not set."
    a <- maybeToEither (e i) (lookup i p)
    t <- parseEither a r
    return (i, t)
  where
    e i = "Parser not found for response id " ++ show i ++ "."
