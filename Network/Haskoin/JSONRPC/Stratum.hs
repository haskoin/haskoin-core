{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Network.Haskoin.JSONRPC.Stratum
( -- * Data
  Balance(..)
, StratumCoin(..)
, StratumData(..)
, StratumNotif(..)
, StratumQuery(..)
, TxHeight(..)
, StratumNotifRequest
, StratumResult
, StratumQueryRequest
, StratumResponse
, StratumMessage
, StratumSession
  -- * Functions
, toRequest
, parseResult
, parseNotif
, newStratumReq
) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
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
import Network.Haskoin.Crypto (Address, Hash256)
import Network.Haskoin.Protocol
    ( OutPoint (OutPoint)
    , Tx
    , decodeTxid
    , encodeTxid
    , outPointHash
    , outPointIndex
    )
import Network.Haskoin.JSONRPC
    ( Id (IntId)
    , Message
    , Method
    , Request (Request)
    , Response (Response)
    , RequestValue
    , ResultValue
    , Result
    )
import Network.Haskoin.JSONRPC.Conduit (Session, newReq)
import Network.Haskoin.Util
    ( bsToHex
    , decode'
    , encode'
    , hexToBS
    )

-- | JSON-RPC request with Stratum payload.
type StratumQueryRequest = Request StratumQuery

-- | JSON-RPC notification with Stratum payload.
type StratumNotifRequest = Request StratumNotif

-- | JSON-RPC response with Stratum payload.
type StratumResponse = Response StratumData Value String

-- | Stratum result in JSON-RPC response.
type StratumResult = Result StratumData Value String

-- | Message from Stratum JSON-RPC server.
type StratumMessage = Message StratumNotif StratumData Value String

-- | Session type for JSON-RPC conduit.
type StratumSession
    = Session StratumQueryRequest StratumData Value String StratumNotif

-- | Transaction height and ID pair. Used in history responses.
data TxHeight = TxHeight
    { blockHeight :: Word  -- ^ Block height.
    , txHash :: Hash256 -- ^ Transaction id.
    } deriving (Show, Eq)

-- | Bitcoin outpoint information.
data StratumCoin = StratumCoin
    { outPoint :: OutPoint   -- ^ Coin data.
    , coinTxHeight :: TxHeight   -- ^ Transaction information.
    , coinValue :: Word64        -- ^ Output vale.
    } deriving (Show, Eq)

-- | Balance information.
data Balance = Balance
    { confirmed :: Word64   -- ^ Confirmed balance.
    , unconfirmed :: Word64 -- ^ Unconfirmed balance.
    } deriving (Show, Eq)

-- | Stratum Request data. To be placed inside JSON request.
data StratumQuery
    = QueryVersion { queryClientVer :: Text, queryProtoVer :: Text }
    | QueryHistory { queryAddr :: Address }
    | QueryBalance { queryAddr :: Address }
    | QueryUnspent { queryAddr :: Address }
    | QueryTx { queryTxid :: Hash256 }
    | QueryBroadcast { queryTx :: Tx }
    deriving (Eq, Show)

-- | Stratum Response data.
data StratumData
    = ServerVersion { dataVersion :: String }
    | AddressHistory { dataHistory :: [TxHeight] }
    | AddressBalance { dataBalance :: Balance }
    | AddressUnspent { dataCoins :: [StratumCoin] }
    | Transaction { dataTx :: Tx }
    | BroadcastId { dataTxid :: Hash256 }
    deriving (Eq, Show)

data StratumNotif
    = NotifAddress { notifAddr :: Address, notifTxid :: Hash256 }
    deriving (Eq, Show)

instance ToJSON StratumNotif where
    toJSON (NotifAddress a t) = toJSON (a, txidToJSON t)

instance ToJSON StratumQuery where
    toJSON (QueryVersion c p) = toJSON (c, p)
    toJSON (QueryHistory a) = toJSON [a]
    toJSON (QueryUnspent a) = toJSON [a]
    toJSON (QueryBalance a) = toJSON [a]
    toJSON (QueryTx i) = toJSON [txidToJSON i]
    toJSON (QueryBroadcast t) = txToJSON t

instance FromJSON Balance where
    parseJSON (Object o) = do
        c <- o .: "confirmed"
        u <- o .: "unconfirmed"
        return $ Balance { confirmed = c, unconfirmed = u }
    parseJSON _ = mzero

instance FromJSON TxHeight where
    parseJSON (Object v) = do
        h <- v .: "height"
        t <- v .: "tx_hash"
        i <- txidParse t
        return $ TxHeight { blockHeight = h, txHash = i }
    parseJSON _ = mzero

instance ToJSON TxHeight where
    toJSON x = object
        [ "height" .= blockHeight x
        , "tx_hash" .= txidToJSON (txHash x)
        ]

instance FromJSON StratumCoin where
    parseJSON (Object o) = do
        h <- o .: "height"
        v <- o .: "value"
        t <- o .: "tx_hash"
        p <- o .: "tx_pos"
        i <- txidParse t
        let op = OutPoint { outPointHash = i, outPointIndex = p }
            th = TxHeight { blockHeight = h, txHash = i }
        return $ StratumCoin
            { outPoint = op
            , coinTxHeight = th
            , coinValue = v
            }
    parseJSON _ = mzero

instance ToJSON StratumCoin where
    toJSON x = object
        [ "height" .= blockHeight (coinTxHeight x)
        , "value" .= coinValue x
        , "tx_hash" .= txidToJSON (txHash $ coinTxHeight x)
        , "tx_pos" .= outPointIndex (outPoint x)
        ]

method :: StratumQuery -> Text
method (QueryVersion _ _) = "server.version"
method (QueryHistory _) = "blockchain.address.get_history"
method (QueryBalance _) = "blockchain.address.get_balance"
method (QueryUnspent _) = "blockchain.address.get_unspent"
method (QueryTx _) = "blockchain.transaction.get"
method (QueryBroadcast _) = "blockchain.transaction.broadcast"

-- | Create a JSON-RPC request from a Stratum request.
toRequest :: StratumQuery          -- ^ Stratum request data.
          -> Int                   -- ^ JSON-RPC request id.
          -> StratumQueryRequest   -- ^ Returns JSON-RPC request object.
toRequest s i = Request (method s) (Just s) (Just (IntId i))

-- | Parse result from JSON-RPC response into a Stratum response.
parseResult :: StratumQuery -- ^ StratumQuery used in corresponding request.
            -> ResultValue -- ^ Result from JSON-RPC response
            -> Parser StratumResult -- ^ Returns Aeson parser.
parseResult q (Right v) = parseHelper q v >>= return . Right
parseResult _ (Left e) = return $ Left e

parseHelper :: StratumQuery -> Value -> Parser StratumData
parseHelper (QueryVersion _ _) v = parseJSON v >>= return . ServerVersion
parseHelper (QueryHistory _) v = parseJSON v >>= return . AddressHistory
parseHelper (QueryBalance _) v = parseJSON v >>= return . AddressBalance
parseHelper (QueryUnspent _) v = parseJSON v >>= return . AddressUnspent
parseHelper (QueryTx _) v = txParse v >>= return . Transaction
parseHelper (QueryBroadcast _) v = txidParse v >>= return . BroadcastId

parseNotifHelper :: Method
                 -> Value
                 -> Parser StratumNotif
parseNotifHelper "blockchain.address.subscribe" (Array v) = do
    a <- parseJSON (V.head v)
    t <- txidParse (V.head $ V.tail v)
    return $ NotifAddress a t
parseNotifHelper _ _ = mzero

-- | Parse notification from JSON-RPC request into Stratum format.
parseNotif :: RequestValue
             -- ^ Request to parse.
             -> Parser StratumNotifRequest
             -- ^ Parser to Stratum request format
parseNotif (Request m (Just p) i) =
    parseNotifHelper m p >>= \s -> return $ Request m (Just s) i
parseNotif _ = mzero

-- | Helper function for Network.Haskoin.JSONRPC.Conduit
newStratumReq :: MonadIO m
              => StratumSession
              -> StratumQuery
              -> m Int
newStratumReq s q = newReq s (toRequest q) p
  where
    p (Response r i) = do
        x <- parseResult q r
        return $ Response x i

txidToJSON :: Hash256 -> Value
txidToJSON = String . pack . encodeTxid

txToJSON :: Tx -> Value
txToJSON = String . pack . bsToHex . encode'

txParse :: Value -> Parser Tx
txParse = withText "bitcoin transaction" $
    return . decode' . fromJust . hexToBS . unpack

txidParse :: Value -> Parser Hash256
txidParse = withText "transaction id" $ return .
    maybe (error "Could not decode transaction id.") id . decodeTxid . unpack
