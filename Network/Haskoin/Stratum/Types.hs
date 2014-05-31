{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Network.Haskoin.Stratum.Types
( -- ** Bitcoin Data
  Balance(..)
, Coin(..)
, TxHeight(..)
  -- ** Stratum Types
, StratumNotif(..)
, StratumQuery(..)
, StratumResponse(..)
, StratumSession
  -- ** JSON-RPC Stratum Messages
, MsgStratum
, NotifStratum
, RequestStratum
, ResponseStratum
, ResultStratum
  -- ** Helper Functions
, toRequest
, parseResult
, parseNotif
, newStratumReq
, msgToStratumResponse
, msgToStratumNotif
) where

import Control.Exception (throw)
import Control.Monad (mzero)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
    ( FromJSON
    , ToJSON
    , Value (Array, Object, String)
    , (.:), (.=)
    , object
    , parseJSON
    , toJSON
    , withText
    )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import Data.Word (Word, Word64)
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Stratum.Exceptions
import Network.Haskoin.Stratum.JSONRPC.Message
import Network.Haskoin.Stratum.JSONRPC.Conduit
import Network.Haskoin.Util

-- | JSON-RPC request with Stratum payload.
type RequestStratum = Request StratumQuery
-- | JSON-RPC notification with Stratum payload.
type NotifStratum = Request StratumNotif
-- | JSON-RPC response with Stratum payload.
type ResponseStratum = Response StratumResponse Value String
-- | Stratum result in JSON-RPC response.
type ResultStratum = Result StratumResponse Value String
-- | Message from Stratum JSON-RPC server.
type MsgStratum = Msg StratumNotif StratumResponse Value String
-- | Session type for JSON-RPC conduit.
type StratumSession = Session StratumResponse Value String StratumNotif

-- | Transaction height and ID pair. Used in history responses.
data TxHeight = TxHeight
    { txHeightBlock :: Word  -- ^ Block height.
    , txHeightId :: Hash256 -- ^ Transaction id.
    } deriving (Show, Eq)

-- | Bitcoin outpoint information.
data Coin = Coin
    { coinOutPoint :: OutPoint   -- ^ Coin data.
    , coinTxHeight :: TxHeight   -- ^ Transaction information.
    , coinValue :: Word64        -- ^ Output vale.
    } deriving (Show, Eq)

-- | Balance information.
data Balance = Balance
    { balConfirmed :: Word64   -- ^ Confirmed balance.
    , balUnconfirmed :: Word64 -- ^ Unconfirmed balance.
    } deriving (Show, Eq)

-- | Stratum Request data. To be placed inside JSON request.
data StratumQuery
    = QueryVersion { queryClientVer :: Text, queryProtoVer :: Text }
    | QueryHistory { queryAddr :: Address }
    | QueryBalance { queryAddr :: Address }
    | QueryUnspent { queryAddr :: Address }
    | QueryTx { queryTxid :: Hash256 }
    | QueryBroadcast { queryTx :: Tx }
    | SubAddress { queryAddr :: Address }
    deriving (Eq, Show)

-- | Stratum Response Result data.
data StratumResponse
    = ServerVersion { stratumServerVer :: String }
    | AddressHistory { stratumAddrHist :: [TxHeight] }
    | AddressBalance { stratumBalance :: Balance }
    | AddressUnspent { stratumCoins :: [Coin] }
    | Transaction { stratumTx :: Tx }
    | BroadcastId { stratumTxid :: Hash256 }
    | AddrStatus { stratumAddrStatus :: Hash256 }
    deriving (Eq, Show)

data StratumNotif
    = NotifAddress { notifAddr :: Address, notifAddrStatus :: Hash256 }
    deriving (Eq, Show)

instance ToJSON StratumNotif where
    toJSON (NotifAddress a t) = toJSON (a, hashToJSON t)

instance ToJSON StratumQuery where
    toJSON (QueryVersion c p) = toJSON (c, p)
    toJSON (QueryHistory a) = toJSON [a]
    toJSON (QueryUnspent a) = toJSON [a]
    toJSON (QueryBalance a) = toJSON [a]
    toJSON (QueryTx i) = toJSON [txidToJSON i]
    toJSON (QueryBroadcast t) = txToJSON t
    toJSON (SubAddress a) = toJSON [a]

instance FromJSON Balance where
    parseJSON (Object o) = do
        c <- o .: "confirmed"
        u <- o .: "unconfirmed"
        return $ Balance c u
    parseJSON _ = mzero

instance FromJSON TxHeight where
    parseJSON (Object v) = do
        h <- v .: "height"
        t <- v .: "tx_hash"
        i <- txidParse t
        return $ TxHeight h i
    parseJSON _ = mzero

instance ToJSON TxHeight where
    toJSON x = object
        [ "height" .= txHeightBlock x
        , "tx_hash" .= txidToJSON (txHeightId x)
        ]

instance FromJSON Coin where
    parseJSON (Object o) = do
        h <- o .: "height"
        v <- o .: "value"
        t <- o .: "tx_hash"
        p <- o .: "tx_pos"
        i <- txidParse t
        let op = OutPoint i p
            th = TxHeight h i
        return $ Coin op th v
    parseJSON _ = mzero

instance ToJSON Coin where
    toJSON x = object
        [ "height" .= txHeightBlock (coinTxHeight x)
        , "value" .= coinValue x
        , "tx_hash" .= txidToJSON (txHeightId $ coinTxHeight x)
        , "tx_pos" .= outPointIndex (coinOutPoint x)
        ]

method :: StratumQuery -> Text
method (QueryVersion _ _) = "server.version"
method (QueryHistory _) = "blockchain.address.get_history"
method (QueryBalance _) = "blockchain.address.get_balance"
method (QueryUnspent _) = "blockchain.address.get_unspent"
method (QueryTx _) = "blockchain.transaction.get"
method (QueryBroadcast _) = "blockchain.transaction.broadcast"
method (SubAddress _) = "blockchain.address.subscribe"

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
toRequest s i = Request (method s) (Just s) (Just (IntId i))

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
    a <- parseJSON (V.head v)
    s <- hashParse (V.head $ V.tail v)
    return $ NotifAddress a s
parseNotifHelper _ _ = mzero

-- | Parse notification from JSON-RPC request into Stratum format.
parseNotif :: RequestValue          -- ^ Request to parse.
             -> Parser NotifStratum -- ^ Parser to Stratum request format
parseNotif (Request m (Just p) i) =
    parseNotifHelper m p >>= \s -> return $ Request m (Just s) i
parseNotif _ = mzero

txidToJSON :: Hash256 -> Value
txidToJSON = String . pack . encodeTxid

txToJSON :: Tx -> Value
txToJSON = String . pack . bsToHex . encode'

txParse :: Value -> Parser Tx
txParse = withText "bitcoin transaction" $
    return . decode' . fromJust . hexToBS . unpack

txidParse :: Value -> Parser Hash256
txidParse = withText "transaction id" $
    return . fromJust . decodeTxid . unpack

hashToJSON :: Hash256 -> Value
hashToJSON = String . pack . bsToHex . encode'

hashParse :: Value -> Parser Hash256
hashParse = withText "hash" $ return . decode' . fromJust . hexToBS . unpack

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
