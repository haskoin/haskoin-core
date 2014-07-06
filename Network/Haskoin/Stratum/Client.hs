{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.Haskoin.Stratum.Client
( -- ** High-level client
  StratumClient
, StratumClientState(..)
, queryStratumTCP
, runStratumTCP
) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (async, link)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, readMVar, putMVar)
import Control.Exception (throw, throwIO)
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.STM (atomically)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Conduit (Conduit, Producer, Consumer, ($$), ($=), (=$))
import qualified Data.Conduit as Conduit
import Data.Conduit.TMChan (newTBMChan, sinkTBMChan, sourceTBMChan)
import qualified Data.Conduit.Binary as ConduitBinary
import qualified Data.Conduit.List as ConduitList
import Data.Conduit.Network
import Network.Haskoin.Stratum.RPC
import Network.Haskoin.Stratum.Types

-- | Session state.
data ClientSession = ClientSession
    (MVar Int)  -- ^ Last used id
    (MVar (IntMap (Value -> Parser (Either String StratumResponse))))
    -- ^ Parser map

-- | Create initial session.
initClientSession :: MonadIO m
                  => m ClientSession
initClientSession = liftIO $ ClientSession
    <$> newMVar 0
    <*> newMVar IntMap.empty

-- | Prepare new request, adding it to map and creating Aeson Value.
newRequest :: MonadIO m
           => ClientSession     -- ^ Session state
           -> StratumRequest    -- ^ Request
           -> m Value           -- ^ JSON value
newRequest (ClientSession iV mV) a = liftIO $ do
    i <- (+1) <$> takeMVar iV
    m <- takeMVar mV
    putMVar mV $ IntMap.insert i (parseResponse a) m
    putMVar iV i
    return $ encodeRequest a i

-- | Conduit that serializes requests.
outgoingConduit :: (MonadIO m, ToJSON j) => Conduit j m ByteString
outgoingConduit = ConduitList.map (L8.toStrict . flip L8.append "\n" . encode)

-- | Conduit that decodes responses only.
incomingConduit :: MonadIO m
                => Bool             -- ^ Do not disconnect
                -> ClientSession
                -> Conduit ByteString m StratumMessage
                -- ^ Returns Conduit with parsed data
incomingConduit n s@(ClientSession _ mV) = do
    b <- if n then return False else IntMap.null <$> liftIO (readMVar mV)
    unless b $ Conduit.await >>= \xM -> case xM of
        Nothing -> if n
            then return ()
            else liftIO . throwIO $ ConnectException "connection lost"
        Just x -> do
            let vM = decodeStrict' x
            v <- maybe pe return vM
            m <- liftIO $ takeMVar mV
            let (pM, m') = pm m v
            liftIO $ putMVar mV m'
            case pM of
                Nothing -> do
                    case parse parseNotif v of
                        Error e -> pn e
                        Success o -> do
                            Conduit.yield $ StratumMsgNotif o
                            incomingConduit n s
                Just p -> do
                    let rE = parseEither p v
                    Conduit.yield $ case rE of
                        Left e -> StratumMsgError e
                        Right r -> either StratumMsgError StratumMsgResponse r
                    incomingConduit n s
  where
    pe   = throw $ ParseException $ "incomingConduit: invalid JSON"
    pn e = throw $ ParseException $ "incomingConduit: " ++ e
    pm m v = case parseMaybe (pa m) v of
        Just (p, m') -> (Just p, m')
        Nothing -> (Nothing, m)
    pa m = withObject "response" $ \o -> do
        i <- o .: "id"
        f <- maybe pnf return $ IntMap.lookup i m
        let m' = IntMap.delete i m
        return (f, m')
    pnf = throw $ UnknownId "parseWithMap: unknown response id"

-- | Stratum client context.
type StratumClient m = ReaderT (StratumClientState m, ClientSession) m

-- | Conduits for the Stratum application.
data StratumClientState m = StratumClientStateTCP
    { stratumSrc  :: Producer m StratumMessage
    , stratumSink :: Consumer StratumRequest m ()
    }

-- | Connect via TCP to Stratum server and run batch of queries.
queryStratumTCP :: (MonadIO m, MonadBaseControl IO m)
             => ClientSettings       -- ^ Server configuration
             -> [StratumRequest]     -- ^ Batch of queries
             -> m [StratumMessage]   -- ^ Batch of responses (in any order)
queryStratumTCP cs as = runStratumTCP False cs $ do
    st <- ask >>= return . fst
    lift $ ConduitList.sourceList as $$ stratumSink st
    lift $ stratumSrc st $$ ConduitList.consume

-- | Execute Stratum TCP client.
runStratumTCP :: (MonadIO m, MonadBaseControl IO m)
              => Bool                -- ^ Handle notifications.
              -> ClientSettings      -- ^ TCP client settings data structure.
              -> StratumClient m a   -- ^ Computation to run.
              -> m a                 -- ^ Result from computation.
runStratumTCP n cs cl = control $ \r -> runTCPClient cs $ \ad -> do
    s <- initClientSession
    c <- atomically $ newTBMChan 128
    a <- async $ sourceTBMChan c $= outgoingConduit $$ appSink ad
    link a
    let snk = Conduit.transPipe liftIO . Conduit.toConsumer $
            ConduitList.mapM (newRequest s)
            =$ sinkTBMChan c True
        src = Conduit.transPipe liftIO . Conduit.toProducer $
            appSource ad
            $= ConduitBinary.lines
            $= incomingConduit n s
    r $ runReaderT cl (StratumClientStateTCP src snk, s)
