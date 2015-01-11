module Network.Haskoin.Stratum
( -- * Stratum JSON-RPC Message Types
  StratumRequest(..)
, StratumNotif(..)
, StratumResult(..)
  -- * Stratum Internal Types
, StratumTxInfo(..)
, StratumCoin(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero)

import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word, Word64)

import Network.JsonRpc

import Network.Haskoin.Crypto
import Network.Haskoin.Transaction.Types

--
-- Stratum Request
--

-- | Stratum Request data. To be placed inside JSON request.
data StratumRequest
    = StratumReqVersion { stratumReqClientVer     :: !Text
                        , stratumReqProtoVer      :: !Text
                        }
    | StratumReqHistory { stratumReqAddr          :: !Address
                        }
    | StratumReqBalance { stratumReqAddr          :: !Address
                        }
    | StratumReqUnspent { stratumReqAddr          :: !Address
                        }
    | StratumReqTx      { stratumReqTxid          :: !TxHash
                        }
    | StratumBcastTx    { stratumReqTx            :: !Tx
                        }
    | StratumSubAddr    { stratumReqAddr          :: !Address
                        }
    deriving (Eq, Show)

instance NFData StratumRequest where
    rnf (StratumReqVersion c p) = rnf c `seq` rnf p
    rnf (StratumReqHistory   a) = rnf a
    rnf (StratumReqBalance   a) = rnf a
    rnf (StratumReqUnspent   a) = rnf a
    rnf (StratumReqTx        i) = rnf i
    rnf (StratumBcastTx      t) = rnf t
    rnf (StratumSubAddr      a) = rnf a

instance ToJSON StratumRequest where
    toJSON (StratumReqVersion c p) = toJSON (c, p)
    toJSON (StratumReqHistory   a) = toJSON [a]
    toJSON (StratumReqBalance   a) = toJSON [a]
    toJSON (StratumReqUnspent   a) = toJSON [a]
    toJSON (StratumReqTx        i) = toJSON [i]
    toJSON (StratumBcastTx      t) = toJSON [t]
    toJSON (StratumSubAddr      a) = toJSON [a]

instance ToRequest StratumRequest where
    requestMethod (StratumReqVersion _ _) = "server.version"
    requestMethod (StratumReqHistory   _) = "blockchain.address.get_history"
    requestMethod (StratumReqBalance   _) = "blockchain.address.get_balance"
    requestMethod (StratumReqUnspent   _) = "blockchain.address.listunspent"
    requestMethod (StratumReqTx        _) = "blockchain.transaction.get"
    requestMethod (StratumBcastTx      _) = "blockchain.transaction.broadcast"
    requestMethod (StratumSubAddr      _) = "blockchain.address.subscribe"

instance FromRequest StratumRequest where
    paramsParser "server.version" = Just $ \x ->
        fmap (\(c, p) -> StratumReqVersion c p) $ parseJSON x

    paramsParser "blockchain.address.get_history" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumReqHistory) . listToMaybe

    paramsParser "blockchain.address.get_balance" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumReqBalance) . listToMaybe

    paramsParser "blockchain.address.listunspent" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumReqUnspent) . listToMaybe

    paramsParser "blockchain.address.subscribe" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumSubAddr) . listToMaybe

    paramsParser "blockchain.transaction.get" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumReqTx) . listToMaybe

    paramsParser "blockchain.transaction.broadcast" = Just $ \x ->
        parseJSON x >>= maybe mzero (return . StratumBcastTx) . listToMaybe

    paramsParser _ = Nothing

--
-- Stratum Notifications
--

data StratumNotif
    = StratumNotifAddr  { stratumNotifAddr        :: !Address
                        , stratumNotifAddrStatus  :: !Word256
                        }
    deriving (Eq, Show)

instance NFData StratumNotif where
    rnf (StratumNotifAddr  a t) = rnf a `seq` rnf t

instance ToJSON StratumNotif where
    toJSON (StratumNotifAddr  a t) = toJSON (a, t)

instance ToNotif StratumNotif where
    notifMethod (StratumNotifAddr  _ _) = "blockchain.address.subscribe"

instance FromNotif StratumNotif where
    notifParamsParser "blockchain.address.subscribe" = Just $ \x ->
        fmap (\(a, s) -> StratumNotifAddr a s) $ parseJSON x

    notifParamsParser _ = Nothing

--
-- Stratum Responses
--

-- | Stratum Response Result data.
data StratumResult
    = StratumSrvVersion     { stratumSrvVersion     :: !String
                            }
    | StratumAddrHistory    { stratumAddrHist       :: ![StratumTxInfo]
                            }
    | StratumAddrBalance    { stratumConfirmed      :: !Word64
                            , stratumUnconfirmed    :: !Word64
                            }
    | StratumAddrUnspent    { stratumCoins          :: ![StratumCoin]
                            }
    | StratumAddrStatus     { stratumAddrStatus     :: !Word256
                            }
    | StratumTx             { stratumTx             :: !Tx
                            }
    | StratumBcastId        { stratumTxId           :: !TxHash
                            }
    deriving (Eq, Show)

instance NFData StratumResult where
    rnf (StratumSrvVersion    s) = rnf s
    rnf (StratumAddrHistory  ts) = rnf ts
    rnf (StratumAddrBalance c u) = rnf c `seq` rnf u
    rnf (StratumAddrUnspent  cs) = rnf cs
    rnf (StratumAddrStatus    s) = rnf s
    rnf (StratumTx            t) = rnf t
    rnf (StratumBcastId       i) = rnf i

instance ToJSON StratumResult where
    toJSON (StratumSrvVersion    v) = toJSON v
    toJSON (StratumAddrHistory  ts) = toJSON ts
    toJSON (StratumAddrBalance c u) = object
        ["confirmed" .= c, "unconfirmed" .= u]
    toJSON (StratumAddrUnspent  cs) = toJSON cs
    toJSON (StratumAddrStatus    s) = toJSON s
    toJSON (StratumTx            t) = toJSON t
    toJSON (StratumBcastId       i) = toJSON i

instance FromResponse StratumResult where
    parseResult "server.version" =
        fmap StratumSrvVersion . parseJSON
    parseResult "blockchain.address.get_history" =
        fmap StratumAddrHistory . parseJSON
    parseResult "blockchain.address.get_balance" =
        withObject "balance" $ \o ->
            StratumAddrBalance <$> o .: "confirmed" <*> o .: "unconfirmed"
    parseResult "blockchain.address.listunspent" =
        fmap StratumAddrUnspent . parseJSON
    parseResult "blockchain.transaction.get" =
        fmap StratumTx . parseJSON
    parseResult "blockchain.transaction.broadcast" =
        fmap StratumBcastId . parseJSON
    parseResult "blockchain.address.subscribe" =
        fmap StratumAddrStatus . parseJSON
    parseResult m = const . fail $
        "Unknown method: " ++ T.unpack m

--
-- Stratum Types
--

-- | Transaction height and ID pair. Used in history responses.
data StratumTxInfo = StratumTxInfo
    { stratumTxInfoHeight   :: !Word   -- ^ Block height.
    , stratumTxInfoId       :: !TxHash -- ^ Transaction id.
    } deriving (Show, Eq)

instance NFData StratumTxInfo where
    rnf (StratumTxInfo h i) = rnf h `seq` rnf i

instance FromJSON StratumTxInfo where
    parseJSON = withObject "txheight" $ \o ->
        StratumTxInfo <$> o .: "height" <*> o .: "tx_hash"

instance ToJSON StratumTxInfo where
    toJSON (StratumTxInfo h i) = object ["height" .= h, "tx_hash" .= i]

-- | Bitcoin outpoint information.
data StratumCoin = StratumCoin
    { stratumCoinOutPoint  :: !OutPoint         -- ^ Coin data.
    , stratumCoinTxInfo    :: !StratumTxInfo    -- ^ Transaction information.
    , stratumCoinValue     :: !Word64           -- ^ Output vale.
    } deriving (Show, Eq)

instance NFData StratumCoin where
    rnf (StratumCoin o t v) = rnf o `seq` rnf t `seq` rnf v

instance FromJSON StratumCoin where
    parseJSON = withObject "coin" $ \o -> do
        h <- o .: "height"
        v <- o .: "value"
        p <- o .: "tx_pos"
        i <- o .: "tx_hash"
        return $ StratumCoin (OutPoint i p) (StratumTxInfo h i) v

instance ToJSON StratumCoin where
    toJSON (StratumCoin (OutPoint _ p) (StratumTxInfo h i) v) = object
        [ "height"   .= h
        , "value"    .= v
        , "tx_hash"  .= i
        , "tx_pos"   .= p ]

