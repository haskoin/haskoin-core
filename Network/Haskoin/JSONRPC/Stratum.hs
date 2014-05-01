{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.JSONRPC.Stratum
( TxHeight(..)
, CoinHeight(..)
, Balance(..)
, StratumRequest(..)
, StratumResponse(..)
, toRequest
, fromResponse )
where

-- import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Word (Word, Word64)
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.JSONRPC
import Network.Haskoin.Util as U

newtype ServerVersion = ServerVersion String deriving (Show, Eq)

data TxHeight = TxHeight
    { txHeight :: Word
    , txHash :: Hash256 }
    deriving (Show, Eq)

data CoinHeight = CoinHeight
    { coinOutPoint :: OutPoint
    , coinTxHeight :: TxHeight
    , coinValue :: Word64 }
    deriving (Show, Eq)

data Balance = Balance
    { balConfirmed :: Word64
    , balUnconfirmed :: Word64 }
    deriving (Show, Eq)

data StratumRequest
    = ReqVersion { reqClientVer :: String, reqProtoVer :: String }
    | ReqHistory { reqAddress :: Address }
    | ReqBalance { reqAddress :: Address }
    | ReqUnspent { reqAddress :: Address }
    | ReqTx { reqTxId :: Hash256 }
    | ReqBroadcast { reqTx :: Tx }
    deriving (Eq, Show)

data StratumResponse
    = ResVersion { resVersion :: String }
    | ResHistory { resHistory :: [TxHeight] }
    | ResBalance { resBalance :: Balance }
    | ResUnspent { resUnspent :: [CoinHeight] }
    | ResTx { resTx :: Tx }
    | ResBroadcast { resTxId :: Hash256 }
    | ResError { resError :: ErrorObj }
    deriving (Eq, Show)

instance FromJSON Hash256 where
    parseJSON = withText "transaction id not a string" $ \s -> do
        return . fromJust . decodeTxid $ T.unpack s

instance ToJSON Hash256 where
    toJSON = toJSON . encodeTxid

instance FromJSON Address where
    parseJSON (String a) = do
        let addrS = T.unpack a
            addrM = base58ToAddr addrS
        case addrM of
            Nothing -> fail $ "Not a bitcoin address: " ++ addrS
            Just addr -> return addr
    parseJSON _ = mzero

instance ToJSON Address where
    toJSON = String . T.pack . addrToBase58

instance FromJSON Tx where
    parseJSON = withText "transaction not a string" $ \s -> do
        etx <- runEitherT $ do
            bs <- liftMaybe "invalid hex encoding" . hexToBS $ T.unpack s
            tx <- liftEither $ decodeToEither bs
            return tx
        let tx = fromRight etx
        return tx

instance ToJSON Tx where
    toJSON t = toJSON (encode' t)

instance FromJSON Balance where
    parseJSON (Object o) = do
        c <- o .: "confirmed"
        u <- o .: "unconfirmed"
        return $ Balance { balConfirmed = c, balUnconfirmed = u }
    parseJSON _ = mzero

instance FromJSON TxHeight where
    parseJSON (Object v) = do
        h <- v .: "height"
        t <- v .: "tx_hash"
        return $ TxHeight { txHeight = h, txHash = t }
    parseJSON _ = mzero

instance ToJSON TxHeight where
    toJSON x = object [ "height" .= txHeight x , "tx_hash" .= txHash x ]

instance FromJSON CoinHeight where
    parseJSON (Object o) = do
        h <- o .: "height"
        v <- o .: "value"
        t <- o .: "tx_hash"
        p <- o .: "tx_pos"
        let op = OutPoint { outPointHash = t, outPointIndex = p }
            th = TxHeight { txHeight = h, txHash = t }
        return $ CoinHeight
            { coinOutPoint = op
            , coinTxHeight = th
            , coinValue = v }
    parseJSON _ = mzero

instance ToJSON CoinHeight where
    toJSON x = object
        [ "height" .= txHeight (coinTxHeight x)
        , "value" .= coinValue x
        , "tx_hash" .= txHash (coinTxHeight x)
        , "tx_pos" .= outPointIndex (coinOutPoint x) ]

-- Convert a StratumRequest into a JSONRPC Request. An Int must be provided to
-- use as id for the Request.
toRequest :: StratumRequest -> Int -> Request
toRequest (ReqVersion c p) i
    = Request "server.version" (toJSON (c, p)) (IntId i)
toRequest (ReqHistory a) i
    = Request "blockchain.address.get_history" (toJSON [a]) (IntId i)
toRequest (ReqBalance a) i
    = Request "blockchain.address.get_balance" (toJSON [a]) (IntId i)
toRequest (ReqUnspent a) i
    = Request "blockchain.address.get_unspent" (toJSON [a]) (IntId i)
toRequest (ReqTx t) i
    = Request "blockchain.transaction.get" (toJSON [t]) (IntId i)
toRequest (ReqBroadcast t) i
    = Request "blockchain.transaction.broadcast" (toJSON [t]) (IntId i)

-- Pair a StratumRequest and a JSONRPC Response to obtain a StratumResponse. It
-- is suggested to partially apply to generate a function that will receive the
-- Response.
fromResponse :: StratumRequest -> Response -> Either String StratumResponse
fromResponse (ReqVersion _ _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResVersion
fromResponse (ReqHistory _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResHistory
fromResponse (ReqBalance _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResBalance
fromResponse (ReqUnspent _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResUnspent
fromResponse (ReqTx _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResTx
fromResponse (ReqBroadcast _) (Response r _) = do
    resultToEither $ fromJSON r >>= return . ResBroadcast
fromResponse _ (ErrorResponse e _) = return $ ResError e

resultToEither :: Result a -> Either String a
resultToEither (Success x) = Right x
resultToEither (Error x) = Left x
