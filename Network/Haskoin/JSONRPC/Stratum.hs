{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.JSONRPC.Stratum
( -- * Data
  TxHeight(..)
, CoinHeight(..)
, Balance(..)

  -- * Request and response data
, StratumReq(..)
, StratumRes(..)

  -- * Functions
, toJSONReq
, parseResult
) where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Word (Word, Word64)
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.JSONRPC
import Network.Haskoin.Util

-- | Server Version data.
newtype ServerVersion = ServerVersion String deriving (Show, Eq)

-- | Transaction height and ID pair. Used in history responses.
data TxHeight = TxHeight
    { txHeight :: Word  -- ^ Block height.
    , txHash :: Hash256 -- ^ Transaction id.
    } deriving (Show, Eq)

-- | Bitcion out point information.
data CoinHeight = CoinHeight
    { coinOutPoint :: OutPoint   -- ^ Coin data.
    , coinTxHeight :: TxHeight   -- ^ Transaction information.
    , coinValue :: Word64        -- ^ Output vale.
    } deriving (Show, Eq)

-- | Balance information.
data Balance = Balance
    { balConfirmed :: Word64   -- ^ Confirmed balance.
    , balUnconfirmed :: Word64 -- ^ Unconfirmed balance.
    } deriving (Show, Eq)

-- | Stratum Request data. To be placed inside JSONReq.
data StratumReq
    = ReqVersion { reqClientVer :: Text, reqProtoVer :: Text }
    | ReqHistory { reqAddress :: Address }
    | ReqBalance { reqAddress :: Address }
    | ReqUnspent { reqAddress :: Address }
    | ReqTx { reqTxId :: Hash256 }
    | ReqBcast { reqTx :: Tx }
    deriving (Eq, Show)

-- | Stratum Response data. Parse JSONRes result into this.
data StratumRes
    = ResVersion { resVersion :: String }
    | ResHistory { resHistory :: [TxHeight] }
    | ResBalance { resBalance :: Balance }
    | ResUnspent { resUnspent :: [CoinHeight] }
    | ResTx { resTx :: Tx }
    | ResBcast { resTxId :: Hash256 }
    deriving (Eq, Show)

instance ToJSON StratumReq where
    toJSON ( ReqVersion c p ) = toJSON (c, p)
    toJSON ( ReqHistory a   ) = toJSON [a]
    toJSON ( ReqUnspent a   ) = toJSON [a]
    toJSON ( ReqBalance a   ) = toJSON [a]
    toJSON ( ReqTx      i   ) = toJSON [i]
    toJSON ( ReqBcast   t   ) = toJSON [t]

instance FromJSON Hash256 where
    parseJSON = withText "transaction id not a string" $ \s -> do
        return . fromJust . decodeTxid $ unpack s

instance ToJSON Hash256 where
    toJSON = toJSON . encodeTxid

instance FromJSON Address where
    parseJSON (String a) = do
        let addrS = unpack a
            addrM = base58ToAddr addrS
        case addrM of
            Nothing -> fail $ "Not a bitcoin address: " ++ addrS
            Just addr -> return addr
    parseJSON _ = mzero

instance ToJSON Address where
    toJSON = String . pack . addrToBase58

instance FromJSON Tx where
    parseJSON = withText "transaction not a string" $ \s -> do
        etx <- runEitherT $ do
            bs <- liftMaybe "invalid hex encoding" . hexToBS $ unpack s
            tx <- liftEither $ decodeToEither bs
            return tx
        return (fromRight etx)

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

method :: StratumReq -> Text
method ( ReqVersion _ _ ) = "server.version"
method ( ReqHistory _   ) = "blockchain.address.get_history"
method ( ReqBalance _   ) = "blockchain.address.get_balance"
method ( ReqUnspent _   ) = "blockchain.address.get_unspent"
method ( ReqTx      _   ) = "blockchain.transaction.get"
method ( ReqBcast   _   ) = "blockchain.transaction.broadcast"

-- | Create a JSON-RPC request from a Stratum request.
toJSONReq :: StratumReq         -- ^ Stratum request data.
          -> Int                -- ^ JSON-RPC request id.
          -> JSONReq StratumReq -- ^ Returns JSON-RPC request object.
toJSONReq s i = JSONReq (method s) (Just s) (Just (IntId i))

-- | Parse result from JSON-RPC request into a Stratum response.
parseResult :: StratumReq         -- ^ Corresponding Stratum request.
           -> Value              -- ^ Result Value from JSONRes object.
           -> Parser StratumRes  -- ^ Returns Aeson Parser.
parseResult ( ReqVersion _ _ ) v = parseJSON v >>= return . ResVersion
parseResult ( ReqHistory _   ) v = parseJSON v >>= return . ResHistory
parseResult ( ReqBalance _   ) v = parseJSON v >>= return . ResBalance
parseResult ( ReqUnspent _   ) v = parseJSON v >>= return . ResUnspent
parseResult ( ReqTx      _   ) v = parseJSON v >>= return . ResTx
parseResult ( ReqBcast   _   ) v = parseJSON v >>= return . ResBcast
