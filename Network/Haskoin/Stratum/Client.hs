{-# LANGUAGE RankNTypes #-}
module Network.Haskoin.Stratum.Client
( StratumClient
, StratumSrc(..)
, getSrc
, sendReq
, runStratumTCP
) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Conduit (Producer, ($$), ($=))
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network
    ( ClientSettings
    , appSource
    , appSink
    , runTCPClient
    )
import Network.Haskoin.Stratum.Conduit
import Network.Haskoin.Stratum.Types

data StratumSrc m = StratumSrc { stratumSrc :: Producer m MsgStratum }

-- | Get source for server responses and notifications.
getSrc :: MonadIO m => StratumClient m (StratumSrc m)
getSrc = ask >>= \(src, _) -> return src

-- | Stratum client context.
type StratumClient m = ReaderT (StratumSrc m, StratumSession) m

-- | Send a Stratum request.
sendReq :: MonadIO m => StratumQuery -> StratumClient m RequestStratum
sendReq q = ask >>= \(_, s) -> lift $ newStratumReq s q

-- | Execute Stratum TCP client.
runStratumTCP :: MonadIO m
              => Bool                -- ^ Handle notifications.
              -> ClientSettings      -- ^ TCP client settings data structure.
              -> StratumClient IO a  -- ^ Computation to run.
              -> m a                 -- ^ Result from computation.
runStratumTCP n cs cl = liftIO $ runTCPClient cs $ \ad -> do
    s <- sess
    _ <- forkIO $ reqSource s $$ appSink ad
    let as = appSource ad $= CB.lines $= resConduit s
    runReaderT cl (StratumSrc as, s)
  where
    sess | n = initSession $ Just parseNotif
         | otherwise = initSession Nothing

