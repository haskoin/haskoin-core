{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.Haskoin.Stratum.Client
( -- ** High-level client
  StratumClient
, StratumClientState(..)
, getStratumClient
, queryStratumTCP
, runStratumTCP
, genReq
  -- ** Lower-level client helpers
, toRequest
, parseResult
, parseNotif
, newStratumReq
, msgToStratumResponse
, msgToStratumNotif
, stratumMethod
) where

import Control.Concurrent.Async (async, link)
import Control.Exception (throw)
import Control.Monad (mzero)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.STM (atomically)
import Data.Aeson.Types (Parser, Value (Array), parseJSON, withText)
import Data.Conduit
    ( Consumer
    , Producer
    , ($$), ($=)
    , toProducer
    , toConsumer
    , transPipe
    )
import Data.Conduit.TMChan (newTBMChan, sinkTBMChan, sourceTBMChan)
import qualified Data.Conduit.Binary as ConduitBinary
import qualified Data.Conduit.List as ConduitList
import Data.Conduit.Network
    ( ClientSettings
    , appSource
    , appSink
    , runTCPClient
    )
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Stratum.Exceptions
import Network.Haskoin.Stratum.JSONRPC.Conduit
import Network.Haskoin.Stratum.JSONRPC.Message
import Network.Haskoin.Stratum.Types
import Network.Haskoin.Util

-- | Stratum client context.
type StratumClient m = ReaderT (StratumClientState m, StratumSession) m

-- | Conduits for the Stratum application.
data StratumClientState m = StratumClientStateTCP
    { stratumSrc  :: Producer m MsgStratum
    , stratumSink :: Consumer RequestStratum m ()
    }

-- | Get Stratum client state.
getStratumClient :: MonadIO m => StratumClient m (StratumClientState m)
getStratumClient = ask >>= return . fst

-- | Generate a Stratum request.
genReq :: MonadIO m => StratumQuery -> StratumClient m RequestStratum
genReq q = ask >>= \(_, s) -> lift $ newStratumReq s q

-- | Connect via TCP to Stratum server and run batch of queries.
queryStratumTCP :: (MonadIO m, MonadBaseControl IO m)
             => ClientSettings       -- ^ Server configuration.
             -> [StratumQuery]       -- ^ Batch of queries.
             -> m [StratumResponse]  -- ^ Batch of responses (in any order).
queryStratumTCP cs qs = runStratumTCP False cs $ do
    rs <- mapM genReq qs
    st <- getStratumClient
    lift $ ConduitList.sourceList rs $$ stratumSink st
    lift $ stratumSrc st $= ConduitList.map msgToStratumResponse
                         $$ ConduitList.consume

-- | Execute Stratum TCP client.
runStratumTCP :: (MonadIO m, MonadBaseControl IO m)
              => Bool                -- ^ Handle notifications.
              -> ClientSettings      -- ^ TCP client settings data structure.
              -> StratumClient m a  -- ^ Computation to run.
              -> m a                 -- ^ Result from computation.
runStratumTCP n cs cl = control $ \r -> runTCPClient cs $ \ad -> do
    s <- initSession $ if n then Just parseNotif else Nothing
    c <- atomically $ newTBMChan 128
    a <- async $ sourceTBMChan c $= reqConduit $$ appSink ad
    link a
    let snk = transPipe liftIO . toConsumer $ sinkTBMChan c True
        src = transPipe liftIO . toProducer $ appSource ad
            $= ConduitBinary.lines
            $= resConduit s
    r $ runReaderT cl (StratumClientStateTCP src snk, s)

{- Lower-level helpers -}

stratumMethod :: StratumQuery -> Text
stratumMethod (QueryVersion _ _) = "server.version"
stratumMethod (QueryHistory _) = "blockchain.address.get_history"
stratumMethod (QueryBalance _) = "blockchain.address.get_balance"
stratumMethod (QueryUnspent _) = "blockchain.address.get_unspent"
stratumMethod (QueryTx _) = "blockchain.transaction.get"
stratumMethod (QueryBroadcast _) = "blockchain.transaction.broadcast"
stratumMethod (SubAddress _) = "blockchain.address.subscribe"

-- | Create a JSON-RPC request from a Stratum request.
newStratumReq :: MonadIO m
              => StratumSession
              -> StratumQuery
              -> m RequestStratum
newStratumReq s q = newReq s (toRequest q) $ \(Response r i) ->
    parseResult q r >>= \x -> return $ Response x i

toRequest :: StratumQuery          -- ^ Stratum request data.
          -> Int                   -- ^ JSON-RPC request id.
          -> RequestStratum   -- ^ Returns JSON-RPC request object.
toRequest s i = Request (stratumMethod s) (Just s) (Just (IntId i))

-- | Parse result from JSON-RPC response into a Stratum response.
parseResult :: StratumQuery -- ^ StratumQuery used in corresponding request.
            -> ResultValue -- ^ Result from JSON-RPC response
            -> Parser ResultStratum -- ^ Returns Aeson parser.
parseResult q (Right v) = parseHelper q v >>= return . Right
parseResult _ (Left e) = return $ Left e

parseHelper :: StratumQuery -> Value -> Parser StratumResponse
parseHelper (QueryVersion _ _) v = parseJSON v >>= return . ServerVersion
parseHelper (QueryHistory _) v = parseJSON v >>= return . AddressHistory
parseHelper (QueryBalance _) v = parseJSON v >>= return . AddressBalance
parseHelper (QueryUnspent _) v = parseJSON v >>= return . AddressUnspent
parseHelper (QueryTx _) v = txParse v >>= return . Transaction
parseHelper (QueryBroadcast _) v = txidParse v >>= return . BroadcastId
parseHelper (SubAddress _) v = hashParse v >>= return . AddrStatus

parseNotifHelper :: Method
                 -> Value
                 -> Parser StratumNotif
parseNotifHelper "blockchain.address.subscribe" (Array v) = do
    a <- parseJSON (Vector.head v)
    s <- hashParse (Vector.head $ Vector.tail v)
    return $ NotifAddress a s
parseNotifHelper _ _ = mzero

-- | Parse notification from JSON-RPC request into Stratum format.
parseNotif :: RequestValue          -- ^ Request to parse.
             -> Parser NotifStratum -- ^ Parser to Stratum request format
parseNotif (Request m (Just p) i) =
    parseNotifHelper m p >>= \s -> return $ Request m (Just s) i
parseNotif _ = mzero

txParse :: Value -> Parser Tx
txParse = withText "bitcoin transaction" $
    return . decode' . fromJust . hexToBS . Text.unpack

txidParse :: Value -> Parser Hash256
txidParse = withText "transaction id" $
    return . fromJust . decodeTxid . Text.unpack

hashParse :: Value -> Parser Hash256
hashParse = withText "hash" $
    return . decode' . fromJust . hexToBS . Text.unpack

msgToStratumResponse :: MsgStratum -> StratumResponse
msgToStratumResponse (MsgResponse (Response (Right l) _)) = l
msgToStratumResponse (MsgResponse (Response (Left (ErrVal s)) _)) =
    throw $ ErrorResponseException s
msgToStratumResponse (MsgResponse (Response (Left (ErrObj i s _)) _)) =
    throw $ ErrorResponseException $ show i ++ ": " ++ s
msgToStratumResponse _ = throw $ ParseException "Unexpected message type"

msgToStratumNotif :: MsgStratum -> StratumNotif
msgToStratumNotif (MsgRequest (Request _ (Just p) Nothing)) = p
msgToStratumNotif _ = throw $ ParseException "Unexpected message type"
