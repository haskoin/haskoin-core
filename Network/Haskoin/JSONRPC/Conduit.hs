{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Network.Haskoin.JSONRPC.Conduit
( -- * Data types
  Session
  -- * Functions
, initSession
, newReq
, reqSource
, resConduit
) where

import Prelude hiding (lines, lookup, map, map, null)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, ToJSON, FromJSON, eitherDecode, encode)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (append, fromStrict, toStrict)
import Data.Conduit (Conduit, Source, ($=), (=$=), await, yield)
import Data.Conduit.List (sourceList, map)
import Data.IntMap.Strict (IntMap, delete, empty, insert, lookup, null)
import Network.Haskoin.JSONRPC (Id(IntId), JSONRes(JSONRes), resId)
import Network.Haskoin.Util (maybeToEither)

-- | Session state in concurrent-friedly MVars.
-- Contains a Chan q where q is a data type for requests to be sent.
-- Also an MVar Int holds the last ID to be used to build a request.
-- Finally an IntMap holds parsers to apply to responses.
data Session q r = Session
    (Chan q)
    (MVar Int)
    (MVar (IntMap (Value -> Parser r)))

initSession :: MonadIO m => m (Session q r)
initSession = liftIO $ Session
    <$> newChan
    <*> newMVar 0
    <*> newMVar empty

newReq :: MonadIO m
       => Session q r
       -> (Int -> q)
       -> (Value -> Parser r)
       -> m ()
newReq (Session rc iv pv) f g = liftIO $ do
    i <- (+1) <$> takeMVar iv
    p <- takeMVar pv
    putMVar pv (insert i g p)
    putMVar iv i
    writeChan rc (f i)

reqSource :: (MonadIO m, ToJSON q)
          => Session q r
          -> Source m ByteString
reqSource (Session rc _ _) = do
    reqs <- liftIO $ getChanContents rc
    sourceList reqs $= map (toStrict . flip append "\n" . encode)

resConduit :: (MonadIO m, FromJSON e, FromJSON v, Show j, Show e, Show v)
           => Session q j
           -> Conduit ByteString m (Either String (JSONRes j e v))
resConduit (Session _ _ pv) = stopOnNull pv =$= decodeConduit pv

decodeConduit :: (MonadIO m, FromJSON e, FromJSON v)
              => MVar (IntMap (Value -> Parser r))
              -> Conduit ByteString m (Either String (JSONRes r e v))
decodeConduit pv = await >>= \mx -> case mx of
    Nothing -> return ()
    Just x -> do
        p <- liftIO $ takeMVar pv
        case decodeRes p x of
            Right (i, t) -> do
                liftIO $ putMVar pv (i `delete` p)
                yield $ Right t
            Left e -> do
                liftIO $ putMVar pv p
                yield $ Left e
        decodeConduit pv

stopOnNull :: (MonadIO m)
           => MVar (IntMap a)
           -> Conduit i m i
stopOnNull pv = liftIO (null <$> readMVar pv) >>= \b -> case b of
    True -> return ()
    False -> await >>= maybe e yield >> stopOnNull pv
  where
    e = error "Connection closed with pending requests."

decodeRes :: (FromJSON e, FromJSON v)
          => IntMap (Value -> Parser r)
          -> ByteString
          -> Either String (Int, JSONRes r e v)
decodeRes p x = do
    r <- eitherDecode (fromStrict x)
    i <- case resId r of
        Just (IntId n) -> Right n
        Nothing -> Left "Id is not set."
        _  -> Left "Non-integer id not supported."
    l <- maybeToEither "Parser not found." (lookup i p)
    t <- parseEither (transRes l) r
    return (i, t)

transRes :: (Value -> Parser r)
         -> JSONRes Value e v
         -> Parser (JSONRes r e v)
transRes f (JSONRes (Right v) i) = f v >>= \r -> return (JSONRes (Right r) i)
transRes _ (JSONRes (Left e) i) = return (JSONRes (Left e) i)
