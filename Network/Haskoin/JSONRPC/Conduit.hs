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
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (append, fromStrict, toStrict)
import Data.Conduit (Conduit, Source, ($=), (=$=), await, yield)
import Data.Conduit.List (sourceList, map)
import Data.IntMap.Strict (IntMap, delete, empty, insert, lookup, null)
import Network.Haskoin.JSONRPC
import Network.Haskoin.Util (maybeToEither)

-- | Parse result inside JSON-RPC response to send to conduit output.
type ResultParser r = ResultValue -> Parser r

-- | Map of ids to result parsers.
type ParserMap r = IntMap (ResultParser r)

-- | Session state.
data Session q r = Session (Chan q) (MVar Int) (MVar (ParserMap r))

-- | Empty initial session.
initSession :: MonadIO m => m (Session q r)
initSession = liftIO $ Session <$> newChan <*> newMVar 0 <*> newMVar empty

-- | Send a new request. Goes to a channel that is read from reqSource.
newReq :: MonadIO m
       => Session q r     -- ^ Session state.
       -> (Int -> q)      -- ^ Request builder.
       -> ResultParser r  -- ^ Parser for result data.
       -> m ()            -- ^ No meaningful output.
newReq (Session rc iv pv) f g = liftIO $ do
    i <- (+1) <$> takeMVar iv
    p <- takeMVar pv
    putMVar pv (insert i g p)
    putMVar iv i
    writeChan rc (f i)

-- | Source of requests.
reqSource :: (MonadIO m, ToJSON q)
          => Session q r           -- ^ Session state.
          -> Source m ByteString   -- ^ Source with serialized requests.
reqSource (Session rc _ _) = do
    rs <- liftIO $ getChanContents rc
    sourceList rs $= map (toStrict . flip append "\n" . encode)

-- | Conduit for that parses responses into appropriate data type.
resConduit :: MonadIO m
           => Session q r -- ^ Session state.
           -> Conduit ByteString m (Either String r)
           -- ^ Returns Conduit with parsed data or parsing errors.
resConduit (Session _ _ pv) = stopOnNull pv =$= decodeConduit pv

decodeConduit :: MonadIO m
              => MVar (ParserMap r)
              -> Conduit ByteString m (Either String r)
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

stopOnNull :: MonadIO m
           => MVar (IntMap a)
           -> Conduit i m i
stopOnNull pv = liftIO (null <$> readMVar pv) >>= \b -> case b of
    True -> return ()
    False -> await >>= maybe e yield >> stopOnNull pv
  where
    e = error "Connection closed with pending requests."

decodeRes :: ParserMap r
          -> ByteString
          -> Either String (Int, r)
decodeRes m x = do
    r <- eitherDecode (fromStrict x)
    i <- case resId r of
        Just (IntId n) -> Right n
        Nothing -> Left "Id is not set."
        _  -> Left "Non-integer id not supported."
    p <- maybeToEither "Parser not found for received response." (lookup i m)
    t <- parseEither p (resResult r)
    return (i, t)
