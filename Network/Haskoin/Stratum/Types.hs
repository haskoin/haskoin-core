{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
) where

import Control.Monad (mzero)
import Data.Aeson
    ( FromJSON
    , ToJSON
    , Value (Object)
    , (.:), (.=)
    , object
    , parseJSON
    , toJSON
    )
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Word (Word, Word64)
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util
import Network.Haskoin.Stratum.JSONRPC.Message
import Network.Haskoin.Stratum.JSONRPC.Conduit

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
    { txHeightBlock :: !Word  -- ^ Block height.
    , txHeightId :: !Hash256 -- ^ Transaction id.
    } deriving (Show, Eq)

-- | Bitcoin outpoint information.
data Coin = Coin
    { coinOutPoint :: !OutPoint   -- ^ Coin data.
    , coinTxHeight :: !TxHeight   -- ^ Transaction information.
    , coinValue :: !Word64        -- ^ Output vale.
    } deriving (Show, Eq)

-- | Balance information.
data Balance = Balance
    { balConfirmed :: !Word64   -- ^ Confirmed balance.
    , balUnconfirmed :: !Word64 -- ^ Unconfirmed balance.
    } deriving (Show, Eq)

-- | Stratum Request data. To be placed inside JSON request.
data StratumQuery
    = QueryVersion { queryClientVer :: !Text, queryProtoVer :: !Text }
    | QueryHistory { queryAddr :: !Address }
    | QueryBalance { queryAddr :: !Address }
    | QueryUnspent { queryAddr :: !Address }
    | QueryTx { queryTxid :: !Hash256 }
    | QueryBroadcast { queryTx :: !Tx }
    | SubAddress { queryAddr :: !Address }
    deriving (Eq, Show)

-- | Stratum Response Result data.
data StratumResponse
    = ServerVersion { stratumServerVer :: !String }
    | AddressHistory { stratumAddrHist :: ![TxHeight] }
    | AddressBalance { stratumBalance :: !Balance }
    | AddressUnspent { stratumCoins :: ![Coin] }
    | Transaction { stratumTx :: !Tx }
    | BroadcastId { stratumTxid :: !Hash256 }
    | AddrStatus { stratumAddrStatus :: !Hash256 }
    deriving (Eq, Show)

data StratumNotif
    = NotifAddress { notifAddr :: !Address, notifAddrStatus :: !Hash256 }
    deriving (Eq, Show)

instance ToJSON StratumNotif where
    toJSON (NotifAddress a t) = toJSON (a, bsToHex $ encode' t)

instance ToJSON StratumQuery where
    toJSON (QueryVersion c p) = toJSON (c, p)
    toJSON (QueryHistory a) = toJSON [a]
    toJSON (QueryUnspent a) = toJSON [a]
    toJSON (QueryBalance a) = toJSON [a]
    toJSON (QueryTx i) = toJSON [encodeTxid i]
    toJSON (QueryBroadcast t) = toJSON [bsToHex $ encode' t]
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
        let i = fromJust . decodeTxid $ unpack t
        return $ TxHeight h i
    parseJSON _ = mzero

instance ToJSON TxHeight where
    toJSON x = object
        [ "height" .= txHeightBlock x
        , "tx_hash" .= encodeTxid (txHeightId x)
        ]

instance FromJSON Coin where
    parseJSON (Object o) = do
        h <- o .: "height"
        v <- o .: "value"
        t <- o .: "tx_hash"
        p <- o .: "tx_pos"
        let i  = fromJust . decodeTxid $ unpack t
            op = OutPoint i p
            th = TxHeight h i
        return $ Coin op th v
    parseJSON _ = mzero

instance ToJSON Coin where
    toJSON x = object
        [ "height" .= txHeightBlock (coinTxHeight x)
        , "value" .= coinValue x
        , "tx_hash" .= encodeTxid (txHeightId $ coinTxHeight x)
        , "tx_pos" .= outPointIndex (coinOutPoint x)
        ]
