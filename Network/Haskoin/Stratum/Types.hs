{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Haskoin.Stratum.Types
( -- ** Stratum Types
  StratumMessage(..)
, StratumNotif(..)
, StratumRequest(..)
, StratumResponse(..)
  -- ** Bitcoin Data
, StratumTxInfo(..)
, StratumCoin(..)
  -- ** Exceptions
, StratumException(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData, rnf)
import Control.Exception (Exception)
import Control.Monad (when)

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Data.Word (Word, Word64)

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util
import Network.Haskoin.Stratum.RPC

import Data.Typeable (Typeable)

data StratumException
    = ParseException String
    | UnknownId String
    | ConnectException String
    deriving (Eq, Read, Show, Typeable)

instance Exception StratumException

-- | Stratum message.
data StratumMessage
    = StratumMsgRequest  { stratumMsgRequest  :: !StratumRequest  }
    | StratumMsgNotif    { stratumMsgNotif    :: !StratumNotif    }
    | StratumMsgResponse { stratumMsgResponse :: !StratumResponse }
    | StratumMsgError    { stratumMsgError    :: !String          }
    deriving (Eq, Show)

instance NFData StratumMessage where
    rnf (StratumMsgRequest r)  = rnf r
    rnf (StratumMsgNotif n)    = rnf n
    rnf (StratumMsgResponse r) = rnf r
    rnf (StratumMsgError e)    = rnf e

-- | Stratum Request data. To be placed inside JSON request.
data StratumRequest
    = RequestVersion { requestClientVer :: !Text, requestProtoVer :: !Text }
    | RequestHistory { requestAddr :: !Address }
    | RequestBalance { requestAddr :: !Address }
    | RequestUnspent { requestAddr :: !Address }
    | RequestTx      { requestTxid :: !TxHash  }
    | BroadcastTx    { requestTx   :: !Tx      }
    | SubscribeAddr  { requestAddr :: !Address }
    deriving (Eq, Show)

instance NFData StratumRequest where
    rnf (RequestVersion c p) = rnf c `seq` rnf p
    rnf (RequestHistory a) = rnf a
    rnf (RequestBalance a) = rnf a
    rnf (RequestUnspent a) = rnf a
    rnf (RequestTx i) = rnf i
    rnf (BroadcastTx t) = rnf t
    rnf (SubscribeAddr a) = rnf a

instance ToJSON StratumRequest where
    toJSON (RequestVersion c p) = toJSON (c, p)
    toJSON (RequestHistory a) = toJSON [a]
    toJSON (RequestBalance a) = toJSON [a]
    toJSON (RequestUnspent a) = toJSON [a]
    toJSON (RequestTx i) = toJSON [encodeTxHashLE i]
    toJSON (BroadcastTx t) = toJSON [bsToHex $ encode' t]
    toJSON (SubscribeAddr a) = toJSON [a]

instance RPCRequest StratumRequest String StratumResponse where
    rpcMethod (RequestVersion _ _)  = "server.version"
    rpcMethod (RequestHistory _)    = "blockchain.address.get_history"
    rpcMethod (RequestBalance _)    = "blockchain.address.get_balance"
    rpcMethod (RequestUnspent _)    = "blockchain.address.listunspent"
    rpcMethod (RequestTx _)         = "blockchain.transaction.get"
    rpcMethod (BroadcastTx _)       = "blockchain.transaction.broadcast"
    rpcMethod (SubscribeAddr _)     = "blockchain.address.subscribe"

    parseParams "server.version" =
        withArray "version" $ \v -> do
            when (Vector.length v < 2) $ fail "parseParams: not enough elements"
            c <- parseJSON $ v ! 0
            p <- parseJSON $ v ! 1
            return $ RequestVersion c p

    parseParams "blockchain.address.get_history" =
        withArray "history" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            RequestHistory <$> parseJSON (Vector.head v)

    parseParams "blockchain.address.get_balance" =
        withArray "balance" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            RequestBalance <$> parseJSON (Vector.head v)

    parseParams "blockchain.address.listunspent" =
        withArray "unspent" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            RequestUnspent <$> parseJSON (Vector.head v)

    parseParams "blockchain.address.subscribe" =
        withArray "subscribe" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            SubscribeAddr <$> parseJSON (Vector.head v)

    parseParams "blockchain.transaction.get" =
        withArray "get transaction" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            s <- parseJSON $ Vector.head v
            i <- maybe ei return $ decodeTxHashLE s
            return $ RequestTx i
      where
        ei = fail "parseParams: could not decode transaction id"

    parseParams "blockchain.transaction.broadcast" =
        withArray "broadcast" $ \v -> do
            when (Vector.null v) $ fail "parseParams: empty array"
            hM <- hexToBS <$> parseJSON (Vector.head v)
            h <- maybe et return hM
            t <- either fail return $ decodeToEither h
            return $ BroadcastTx t
      where
        et = fail "parseParams: could not decode transaction hex"

    parseParams m = fail $ "parseParams: unknown method " ++ unpack m

    parseResult (RequestVersion _ _) = withText "version" $ \t ->
        return $ ServerVersion $ unpack t

    parseResult (RequestHistory a) = withArray "history" $ \v ->
        AddrHistory a <$> parseJSON (Array v)

    parseResult (RequestBalance a) = withObject "balance" $ \o ->
        AddrBalance a <$> o .: "confirmed" <*> o .: "unconfirmed"

    parseResult (RequestUnspent a) = withArray "unspent" $ \v ->
        AddrUnspent a <$> parseJSON (Array v)

    parseResult (RequestTx i) = withText "transaction" $ \t -> do
        h <- maybe eh return $ hexToBS $ unpack t
        x <- either fail return $ decodeToEither h
        return $ Transaction i x
      where
        eh = fail "parseResult: could not decode hex transaction"

    parseResult (BroadcastTx x) = withText "txid" $ \t -> do
        let iM = decodeTxHashLE $ unpack t
        i <- maybe ei return iM
        return $ BroadcastId i x
      where
        ei = fail "parseResult: could not parse transaction id"

    parseResult (SubscribeAddr a) = withText "status" $ \t -> do
        b <- maybe eh return $ hexToBS $ unpack t
        h <- either fail return $ decodeToEither b
        return $ AddrStatus a h
      where
        eh = fail "parseResult: failed to read status from hex"

    parseError _ = withText "error" $ return . unpack

data StratumNotif = NotifAddress
    { notifAddr         :: !Address
    , notifAddrStatus   :: !Word256
    } deriving (Eq, Show)

instance NFData StratumNotif where
    rnf (NotifAddress a s) = rnf a `seq` rnf s

instance ToJSON StratumNotif where
    toJSON (NotifAddress a t) = toJSON (a, bsToHex $ encode' t)

instance RPCNotif StratumNotif where
    rpcNotifMethod (NotifAddress _ _) = "blockchain.address.subscribe"

    parseNotifParams "blockchain.address.subscribe" =
        withArray "blockchain.address.subscribe" $ \v -> do
            a <- parseJSON (v ! 0)
            s <- f $ v ! 1
            return $ NotifAddress a s
      where
        f = withText "status" $ \t -> do
            let bsM = hexToBS $ unpack t
            bs <- maybe ebs return bsM
            either fail return $ decodeToEither bs
        ebs = fail "parseNotifParams: could not parse status hex"

    parseNotifParams m = fail $ "parseNotifParams: unknown method " ++ unpack m

-- | Stratum Response Result data.
data StratumResponse
    = ServerVersion
        { stratumVersion        :: !String
        }
    | AddrHistory
        { stratumAddr           :: !Address
        , stratumAddrHist       :: ![StratumTxInfo]
        }
    | AddrBalance
        { stratumAddr           :: !Address
        , stratumConfirmed      :: !Word64
        , stratumUnconfirmed    :: !Word64
        }
    | AddrUnspent
        { stratumAddr           :: !Address
        , stratumCoins          :: ![StratumCoin]
        }
    | AddrStatus
        { stratumAddr           :: !Address
        , stratumAddrStatus     :: !Word256
        }
    | Transaction
        { stratumTxId           :: !TxHash
        , stratumTx             :: !Tx
        }
    | BroadcastId
        { stratumTxId           :: !TxHash
        , stratumTx             :: !Tx
        }
    deriving (Eq, Show)

instance NFData StratumResponse where
    rnf (ServerVersion s)       = rnf s
    rnf (AddrHistory a ts)      = rnf a `seq` rnf ts
    rnf (AddrBalance a c u)     = rnf a `seq` rnf c `seq` rnf u
    rnf (AddrUnspent a cs)      = rnf a `seq` rnf cs
    rnf (AddrStatus a s)        = rnf a `seq` rnf s
    rnf (Transaction i t)       = rnf i `seq` rnf t
    rnf (BroadcastId i t)       = rnf i `seq` rnf t

-- | Transaction height and ID pair. Used in history responses.
data StratumTxInfo = StratumTxInfo
    { stratumTxInfoHeight   :: !Word   -- ^ Block height.
    , stratumTxInfoId       :: !TxHash -- ^ Transaction id.
    } deriving (Show, Eq)

instance NFData StratumTxInfo where
    rnf (StratumTxInfo h i) = rnf h `seq` rnf i

instance FromJSON StratumTxInfo where
    parseJSON = withObject "txheight" $ \o -> do
        h <- o .: "height"
        iM <- decodeTxHashLE <$> o .: "tx_hash"
        i <- maybe ei return iM
        return $ StratumTxInfo h i
      where
        ei = fail "cannot decode transaction id"

instance ToJSON StratumTxInfo where
    toJSON (StratumTxInfo h i) = object
        [ "height" .= h
        , "tx_hash" .= encodeTxHashLE i
        ]

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
        iM <- decodeTxHashLE <$> o .: "tx_hash"
        i <- maybe ei return iM
        let op = OutPoint i p
            th = StratumTxInfo h i
        return $ StratumCoin op th v
      where
        ei = fail "cannot decode transaction id"

instance ToJSON StratumCoin where
    toJSON (StratumCoin (OutPoint _ p) (StratumTxInfo h i) v) = object
        [ "height" .= h
        , "value" .= v
        , "tx_hash" .= encodeTxHashLE i
        , "tx_pos" .= p
        ]
