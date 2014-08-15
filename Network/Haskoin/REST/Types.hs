{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.REST.Types 
( NewWallet(..) 
, MnemonicRes(..)
, NewAccount(..)
, AddressPageRes(..)
, TxPageRes(..)
, AddressData(..)
, TxAction(..)
, TxHashStatusRes(..)
, TxRes(..)
, TxStatusRes(..)
, BalanceRes(..)
, NodeAction(..)
, RescanRes(..)
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Word (Word32, Word64)
import Data.Maybe (isJust, fromJust)
import Data.Aeson 
    ( Value (..)
    , object
    , ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , withObject
    , (.:), (.:?), (.=)
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol

import Network.Haskoin.Wallet.Types

data NewWallet = NewWallet !WalletName !String !(Maybe String)
    deriving (Eq, Read, Show)

instance ToJSON NewWallet where
    toJSON (NewWallet n p m) = object $ concat
        [ [ "walletname" .= n
            , "passphrase" .= p
            ]
        , if isJust m then ["mnemonic" .= (fromJust m)] else []
        ]

instance FromJSON NewWallet where
    parseJSON = withObject "newwallet" $ \o -> NewWallet
        <$> o .: "walletname"
        <*> o .: "passphrase"
        <*> o .:? "mnemonic"

data MnemonicRes = MnemonicRes Mnemonic
    deriving (Eq, Read, Show)

instance ToJSON MnemonicRes where
    toJSON (MnemonicRes m) = object [ "mnemonic" .= m ]

instance FromJSON MnemonicRes where
    parseJSON = withObject "mnemonic" $ \o ->
        MnemonicRes <$> o .: "mnemonic"

data NewAccount
    = NewAccount !WalletName !AccountName
    | NewMSAccount !WalletName !AccountName !Int !Int ![XPubKey]
    | NewReadAccount !AccountName !XPubKey
    | NewReadMSAccount !AccountName !Int !Int ![XPubKey]
    deriving (Eq, Read, Show)

instance ToJSON NewAccount where
    toJSON acc = case acc of
        NewAccount w n -> object
            [ "type"        .= String "regular"
            , "walletname"  .= w
            , "accountname" .= n
            ]
        NewMSAccount w n r t ks -> object
            [ "type"        .= String "multisig"
            , "walletname"  .= w
            , "accountname" .= n
            , "required"    .= r
            , "total"       .= t
            , "keys"        .= map xPubExport ks
            ]
        NewReadAccount n k -> object
            [ "type"        .= String "readregular"
            , "accountname" .= n
            , "key"         .= xPubExport k
            ]
        NewReadMSAccount n r t ks -> object
            [ "type"        .= String "readmultisig"
            , "accountname" .= n
            , "required"    .= r
            , "total"       .= t
            , "keys"        .= map xPubExport ks
            ]

instance FromJSON NewAccount where
    parseJSON = withObject "newaccount" $ \o -> do
        (String t) <- o .: "type"
        case t of
            "regular" -> NewAccount
                <$> o .: "walletname"
                <*> o .: "accountname"
            "multisig" -> NewMSAccount
                <$> o .: "walletname"
                <*> o .: "accountname"
                <*> o .: "required"
                <*> o .: "total"
                <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
            "readregular" -> NewReadAccount
                <$> o .: "accountname"
                <*> (o .: "key" >>= maybe mzero return . xPubImport)
            "readmultisig" -> NewReadMSAccount
                <$> o .: "accountname"
                <*> o .: "required"
                <*> o .: "total"
                <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
            _ -> mzero

data AddressPageRes = AddressPageRes ![PaymentAddress] !Int
    deriving (Eq, Show, Read)

instance ToJSON AddressPageRes where
    toJSON (AddressPageRes as m) = object
        [ "addresspage" .= as
        , "maxpage"     .= m
        ]

instance FromJSON AddressPageRes where
    parseJSON = withObject "addresspageres" $ \o -> 
        AddressPageRes <$> o .: "addresspage"
                       <*> o .: "maxpage"

data TxPageRes = TxPageRes ![AccTx] !Int
    deriving (Eq, Show, Read)

instance ToJSON TxPageRes where
    toJSON (TxPageRes txs m) = object
        [ "txpage"  .= txs 
        , "maxpage" .= m
        ]

instance FromJSON TxPageRes where
    parseJSON = withObject "txpageres" $ \o -> 
        TxPageRes <$> o .: "txpage"
                  <*> o .: "maxpage"

data AddressData = AddressData !String
    deriving (Eq, Show, Read)

instance ToJSON AddressData where
    toJSON (AddressData s) = object [ "label" .= s ]

instance FromJSON AddressData where
    parseJSON = withObject "addressdata" $ \o ->
        AddressData <$> o .: "label"

data TxAction
    = SendCoins ![(Address, Word64)] !Word64
    | SignTx !Tx
    deriving (Eq, Read, Show)

instance ToJSON TxAction where
    toJSON action = case action of
        SendCoins rs f -> object
            [ "type"        .= String "send"
            , "recipients"  .= rs
            , "fee"         .= f
            ]
        SignTx tx -> object
            [ "type"        .= String "sign"
            , "tx"          .= tx
            ]

instance FromJSON TxAction where
    parseJSON = withObject "txaction" $ \o -> do
        (String t) <- o .: "type"
        case t of
            "send" -> SendCoins
                <$> o .: "recipients"
                <*> o .: "fee"
            "sign" -> SignTx
                <$> o .: "tx"
            _ -> mzero

data TxHashStatusRes = TxHashStatusRes !TxHash !Bool
    deriving (Eq, Show, Read)

instance ToJSON TxHashStatusRes where
    toJSON (TxHashStatusRes h b) = object
        [ "txhash"   .= h
        , "complete" .= b
        ]

instance FromJSON TxHashStatusRes where
    parseJSON = withObject "txhashstatusres" $ \o ->
        TxHashStatusRes <$> o .: "txhash"
                        <*> o .: "complete"

data TxRes = TxRes !Tx deriving (Eq, Show, Read)

instance ToJSON TxRes where
    toJSON (TxRes tx) = object [ "tx" .= tx ]

instance FromJSON TxRes where
    parseJSON = withObject "txres" $ \o -> TxRes <$> o .: "tx"
        
data TxStatusRes = TxStatusRes !Tx !Bool
    deriving (Eq, Show, Read)

instance ToJSON TxStatusRes where
    toJSON (TxStatusRes tx b) = object
        [ "tx"       .= tx
        , "complete" .= b
        ]

instance FromJSON TxStatusRes where
    parseJSON = withObject "txstatusres" $ \o ->
        TxStatusRes <$> o .: "tx"
                    <*> o .: "complete"

data BalanceRes = BalanceRes !Word64
    deriving (Eq, Show, Read)

instance ToJSON BalanceRes where
    toJSON (BalanceRes b) = object [ "balance" .= b ]

instance FromJSON BalanceRes where
    parseJSON = withObject "balanceres" $ \o ->
        BalanceRes <$> o .: "balance"

data NodeAction = Rescan !(Maybe Word32)
    deriving (Eq, Show, Read)

instance ToJSON NodeAction where
    toJSON a = case a of
        Rescan (Just t) -> object 
            [ "type"      .= String "rescan"
            , "timestamp" .= t 
            ]
        Rescan Nothing  -> object
            [ "type" .= String "rescan" ]
            

instance FromJSON NodeAction where
    parseJSON = withObject "nodeaction" $ \o -> do
        (String t) <- o .: "type"
        case t of
            "rescan" -> Rescan <$> o .:? "timestamp"
            _ -> mzero

data RescanRes = RescanRes !Word32
    deriving (Eq, Show, Read)

instance ToJSON RescanRes where
    toJSON (RescanRes t) = object [ "timestamp" .= t ]

instance FromJSON RescanRes where
    parseJSON = withObject "rescanres" $ \o ->
        RescanRes <$> o .: "timestamp"

