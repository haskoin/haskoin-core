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
, StratumError(..)
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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Data.Word (Word, Word64)

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util
import Network.Haskoin.Stratum.RPC

import Data.Typeable (Typeable)

data StratumException
    = StratumParseException String
    | StratumUnknownId String
    | StratumConnectException String
    deriving (Eq, Read, Show, Typeable)

instance Exception StratumException

-- | Stratum message.
data StratumMessage
    = StratumMsgRequest  { stratumMsgRequest  :: !StratumRequest  }
    | StratumMsgNotif    { stratumMsgNotif    :: !StratumNotif    }
    | StratumMsgResponse { stratumMsgResponse :: !StratumResponse }
    | StratumMsgError    { stratumMsgError    :: !StratumError    }
    deriving (Eq, Show)

instance NFData StratumMessage where
    rnf (StratumMsgRequest  r) = rnf r
    rnf (StratumMsgNotif    n) = rnf n
    rnf (StratumMsgResponse r) = rnf r
    rnf (StratumMsgError    e) = rnf e

-- | Stratum Request data. To be placed inside JSON request.
data StratumRequest
    = StratumRequestVersion { stratumRequestClientVer :: !Text
                            , stratumRequestProtoVer  :: !Text    }
    | StratumRequestHistory { stratumRequestAddr      :: !Address }
    | StratumRequestBalance { stratumRequestAddr      :: !Address }
    | StratumRequestUnspent { stratumRequestAddr      :: !Address }
    | StratumRequestTx      { stratumRequestTxid      :: !TxHash  }
    | StratumBroadcastTx    { stratumRequestTx        :: !Tx      }
    | StratumSubscribeAddr  { stratumRequestAddr      :: !Address }
    deriving (Eq, Show)

instance NFData StratumRequest where
    rnf (StratumRequestVersion c p) = rnf c `seq` rnf p
    rnf (StratumRequestHistory   a) = rnf a
    rnf (StratumRequestBalance   a) = rnf a
    rnf (StratumRequestUnspent   a) = rnf a
    rnf (StratumRequestTx        i) = rnf i
    rnf (StratumBroadcastTx      t) = rnf t
    rnf (StratumSubscribeAddr    a) = rnf a

instance ToJSON StratumRequest where
    toJSON (StratumRequestVersion c p) = toJSON (c, p)
    toJSON (StratumRequestHistory   a) = toJSON [a]
    toJSON (StratumRequestBalance   a) = toJSON [a]
    toJSON (StratumRequestUnspent   a) = toJSON [a]
    toJSON (StratumRequestTx        i) = toJSON [encodeTxHashLE i]
    toJSON (StratumBroadcastTx      t) = toJSON [bsToHex $ encode' t]
    toJSON (StratumSubscribeAddr    a) = toJSON [a]

instance RPCRequest StratumRequest StratumError StratumResponse where
    rpcMethod (StratumRequestVersion _ _) = "server.version"
    rpcMethod (StratumRequestHistory   _) = "blockchain.address.get_history"
    rpcMethod (StratumRequestBalance   _) = "blockchain.address.get_balance"
    rpcMethod (StratumRequestUnspent   _) = "blockchain.address.listunspent"
    rpcMethod (StratumRequestTx        _) = "blockchain.transaction.get"
    rpcMethod (StratumBroadcastTx      _) = "blockchain.transaction.broadcast"
    rpcMethod (StratumSubscribeAddr    _) = "blockchain.address.subscribe"

    parseRPCParams "server.version" =
        withArray "version" $ \v -> do
            when (Vector.length v < 2) $
                fail "parseRPCParams: not enough elements"
            c <- parseJSON $ v ! 0
            p <- parseJSON $ v ! 1
            return $ StratumRequestVersion c p

    parseRPCParams "blockchain.address.get_history" =
        withArray "history" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            StratumRequestHistory <$> parseJSON (Vector.head v)

    parseRPCParams "blockchain.address.get_balance" =
        withArray "balance" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            StratumRequestBalance <$> parseJSON (Vector.head v)

    parseRPCParams "blockchain.address.listunspent" =
        withArray "unspent" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            StratumRequestUnspent <$> parseJSON (Vector.head v)

    parseRPCParams "blockchain.address.subscribe" =
        withArray "subscribe" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            StratumSubscribeAddr <$> parseJSON (Vector.head v)

    parseRPCParams "blockchain.transaction.get" =
        withArray "get transaction" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            s <- parseJSON $ Vector.head v
            i <- maybe ei return $ decodeTxHashLE s
            return $ StratumRequestTx i
      where
        ei = fail "parseParams: could not decode transaction id"

    parseRPCParams "blockchain.transaction.broadcast" =
        withArray "broadcast" $ \v -> do
            when (Vector.null v) $ fail "parseRPCParams: empty array"
            hM <- hexToBS <$> parseJSON (Vector.head v)
            h <- maybe et return hM
            t <- either fail return $ decodeToEither h
            return $ StratumBroadcastTx t
      where
        et = fail "parseRPCParams: could not decode transaction hex"

    parseRPCParams m = withArray "unknown method" $ \_ ->
        fail $ "parseRPCParams: unknown method " ++ Text.unpack m

    parseRPCResult (StratumRequestVersion _ _) = withText "version" $ \t ->
        return $ StratumServerVersion $ Text.unpack t

    parseRPCResult (StratumRequestHistory a) = withArray "history" $ \v ->
        StratumAddrHistory a <$> parseJSON (Array v)

    parseRPCResult (StratumRequestBalance a) = withObject "balance" $ \o ->
        StratumAddrBalance a <$> o .: "confirmed" <*> o .: "unconfirmed"

    parseRPCResult (StratumRequestUnspent a) = withArray "unspent" $ \v ->
        StratumAddrUnspent a <$> parseJSON (Array v)

    parseRPCResult (StratumRequestTx i) = withText "transaction" $ \t -> do
        h <- maybe eh return $ hexToBS $ Text.unpack t
        x <- either fail return $ decodeToEither h
        return $ StratumTransaction i x
      where
        eh = fail "parseRPCResult: could not decode hex transaction"

    parseRPCResult (StratumBroadcastTx x) = withText "txid" $ \t -> do
        let iM = decodeTxHashLE $ Text.unpack t
        i <- maybe ei return iM
        return $ StratumBroadcastId i x
      where
        ei = fail "parseRPCResult: could not parse transaction id"

    parseRPCResult (StratumSubscribeAddr a) = withText "status" $ \t -> do
        b <- maybe eh return $ hexToBS $ Text.unpack t
        h <- either fail return $ decodeToEither b
        return $ StratumAddrStatus a h
      where
        eh = fail "parseRPCResult: failed to read status from hex"

    parseRPCError x = withText "error" $ \u -> do
        let s = Text.unpack u
        return $ case x of
            StratumRequestVersion _ _ -> StratumErrVersion       s
            StratumRequestHistory   a -> StratumErrHistory       s a
            StratumRequestBalance   a -> StratumErrBalance       s a
            StratumRequestUnspent   a -> StratumErrUnspent       s a
            StratumRequestTx        i -> StratumErrTx            s i
            StratumBroadcastTx      t -> StratumErrBroadcastTx   s t
            StratumSubscribeAddr    a -> StratumErrSubscribeAddr s a

data StratumNotif = StratumNotifAddr
    { stratumNotifAddr       :: !Address
    , stratumNotifAddrStatus :: !Word256
    } deriving (Eq, Show)

instance NFData StratumNotif where
    rnf (StratumNotifAddr a s) = rnf a `seq` rnf s

instance ToJSON StratumNotif where
    toJSON (StratumNotifAddr a t) = toJSON (a, bsToHex $ encode' t)

instance RPCNotif StratumNotif where
    rpcNotifMethod (StratumNotifAddr _ _) = "blockchain.address.subscribe"

    parseRPCNotifParams "blockchain.address.subscribe" =
        withArray "blockchain.address.subscribe" $ \v -> do
            when (Vector.length v < 2) $
                fail "parseRPCNotifParams: array too small"
            a <- parseJSON (Vector.head v)
            s <- f $ v ! 1
            return $ StratumNotifAddr a s
      where
        f = withText "status" $ \t -> do
            let bsM = hexToBS $ Text.unpack t
            bs <- maybe ebs return bsM
            either fail return $ decodeToEither bs
        ebs = fail "parseRPCNotifParams: could not parse status hex"

    parseRPCNotifParams m = withArray "unknown method" $ \_ ->
        fail $ "parseRPCNotifParams: unknown method " ++ Text.unpack m

-- | Stratum Response Result data.
data StratumResponse
    = StratumServerVersion
        { stratumServerVersion  :: !String
        }
    | StratumAddrHistory
        { stratumAddr           :: !Address
        , stratumAddrHist       :: ![StratumTxInfo]
        }
    | StratumAddrBalance
        { stratumAddr           :: !Address
        , stratumConfirmed      :: !Word64
        , stratumUnconfirmed    :: !Word64
        }
    | StratumAddrUnspent
        { stratumAddr           :: !Address
        , stratumCoins          :: ![StratumCoin]
        }
    | StratumAddrStatus
        { stratumAddr           :: !Address
        , stratumAddrStatus     :: !Word256
        }
    | StratumTransaction
        { stratumTxId           :: !TxHash
        , stratumTx             :: !Tx
        }
    | StratumBroadcastId
        { stratumTxId           :: !TxHash
        , stratumTx             :: !Tx
        }
    deriving (Eq, Show)

instance NFData StratumResponse where
    rnf (StratumServerVersion    s) = rnf s
    rnf (StratumAddrHistory   a ts) = rnf a `seq` rnf ts
    rnf (StratumAddrBalance a c  u) = rnf a `seq` rnf c `seq` rnf u
    rnf (StratumAddrUnspent   a cs) = rnf a `seq` rnf cs
    rnf (StratumAddrStatus    a  s) = rnf a `seq` rnf s
    rnf (StratumTransaction   i  t) = rnf i `seq` rnf t
    rnf (StratumBroadcastId   i  t) = rnf i `seq` rnf t

data StratumError
    = StratumErrVersion
        { stratumErr               :: !String
        }
    | StratumErrHistory
        { stratumErr               :: !String
        , stratumErrAddr           :: !Address
        }
    | StratumErrBalance
        { stratumErr               :: !String
        , stratumErrAddr           :: !Address
        }
    | StratumErrUnspent
        { stratumErr               :: !String
        , stratumErrAddr           :: !Address
        }
    | StratumErrTx
        { stratumErr               :: !String
        , stratumErrTxId           :: !TxHash
        }
    | StratumErrBroadcastTx
        { stratumErr               :: !String
        , stratumErrTx             :: !Tx
        }
    | StratumErrSubscribeAddr
        { stratumErr               :: !String
        , stratumErrAddr           :: !Address
        }
    | StratumErrUnknown
        { stratumErr               :: !String
        }
    deriving (Eq, Show)

instance NFData StratumError where
    rnf (StratumErrVersion         s) = rnf s
    rnf (StratumErrHistory       s a) = rnf s `seq` rnf a
    rnf (StratumErrBalance       s a) = rnf s `seq` rnf a
    rnf (StratumErrUnspent       s a) = rnf s `seq` rnf a
    rnf (StratumErrTx            s i) = rnf s `seq` rnf i
    rnf (StratumErrBroadcastTx   s t) = rnf s `seq` rnf t
    rnf (StratumErrSubscribeAddr s a) = rnf s `seq` rnf a
    rnf (StratumErrUnknown         s) = rnf s

instance FromJSON StratumError where
    parseJSON = withText "error" $ return . StratumErrUnknown . Text.unpack

-- | Transaction height and ID pair. Used in history responses.
data StratumTxInfo = StratumTxInfo
    { stratumTxInfoHeight   :: !Word   -- ^ Block height.
    , stratumTxInfoId       :: !TxHash -- ^ Transaction id.
    } deriving (Show, Eq)

instance NFData StratumTxInfo where
    rnf (StratumTxInfo h i) = rnf h `seq` rnf i

instance FromJSON StratumTxInfo where
    parseJSON = withObject "txheight" $ \o -> do
        h  <- o .: "height"
        iM <- decodeTxHashLE <$> o .: "tx_hash"
        i  <- maybe ei return iM
        return $ StratumTxInfo h i
      where
        ei = fail "cannot decode transaction id"

instance ToJSON StratumTxInfo where
    toJSON (StratumTxInfo h i) = object
        [ "height"  .= h
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
        h  <- o .: "height"
        v  <- o .: "value"
        p  <- o .: "tx_pos"
        iM <- decodeTxHashLE <$> o .: "tx_hash"
        i  <- maybe ei return iM
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
