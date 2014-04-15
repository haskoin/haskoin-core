{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Haskoin.JSONRPC.Conduit
( AppTCP
, ContextTCP
, runAppTCP
, newID
, jsonSource
, jsonSink
) where

import Control.Applicative
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Binary
import Data.Conduit.Attoparsec

type ContextTCP a = MonadIO m => StateT Integer m a
type AppTCP = AppData IO -> ContextTCP ()

jsonSink :: (MonadThrow m, FromJSON j) => Consumer BS.ByteString m (Result j)
jsonSink = sinkParser $ fromJSON <$> json'

jsonSource :: (Monad m, ToJSON j) => j -> Producer m BS.ByteString
jsonSource j = sourceLbs $ encode j `BL.append` "\n"

runAppTCP :: ClientSettings IO -> AppTCP -> IO ()
runAppTCP cs app = runTCPClient cs (\ad -> evalStateT (app ad) 1)

newID :: Monad m => (Integer -> a) -> StateT Integer m a
newID f = state (\s -> (s, s + 1)) >>= return . f
