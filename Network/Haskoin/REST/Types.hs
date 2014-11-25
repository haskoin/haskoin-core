{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.REST.Types 
( NewWallet(..) 
, MnemonicRes(..)
, NewAccount(..)
, AddressPageRes(..)
, TxPageRes(..)
, AddressData(..)
, AccTxAction(..)
, TxAction(..)
, TxHashStatusRes(..)
, TxRes(..)
, TxStatusRes(..)
, BalanceRes(..)
, SpendableRes(..)
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

import Network.Haskoin.Transaction
import Network.Haskoin.Crypto

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

data MnemonicRes = MnemonicRes !Mnemonic
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
            [ "type"        .= String "read"
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
            "read" -> NewReadAccount
                <$> o .: "accountname"
                <*> (o .: "key" >>= maybe mzero return . xPubImport)
            "readmultisig" -> NewReadMSAccount
                <$> o .: "accountname"
                <*> o .: "required"
                <*> o .: "total"
                <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
            _ -> mzero

data AddressPageRes = AddressPageRes ![BalanceAddress] !Int
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

data AccTxAction
    = SendCoins ![(Address, Word64)] !Word64 !Word32
    | SignTx !Tx
    | SignSigBlob !SigBlob
    deriving (Eq, Read, Show)

instance ToJSON AccTxAction where
    toJSON action = case action of
        SendCoins rs f m -> object
            [ "type"        .= String "send"
            , "recipients"  .= rs
            , "fee"         .= f
            , "minconf"     .= m
            ]
        SignTx tx -> object
            [ "type"        .= String "sign"
            , "tx"          .= tx
            ]
        SignSigBlob blob -> object
            [ "type"        .= String "sigblob"
            , "sigblob"     .= blob
            ]

instance FromJSON AccTxAction where
    parseJSON = withObject "acctxaction" $ \o -> do
        (String t) <- o .: "type"
        case t of
            "send" -> SendCoins
                <$> o .: "recipients"
                <*> o .: "fee"
                <*> o .: "minconf"
            "sign" -> SignTx
                <$> o .: "tx"
            "sigblob" -> SignSigBlob
                <$> o .: "sigblob"
            _ -> mzero

data TxAction = ImportTx !Tx
    deriving (Eq, Show, Read)

instance ToJSON TxAction where
    toJSON action = case action of
        ImportTx tx -> object 
            [ "type" .= String "import"
            , "tx" .= tx 
            ]

instance FromJSON TxAction where
    parseJSON = withObject "txaction" $ \o -> do
        (String t) <- o .: "type"
        case t of
            "import" -> ImportTx <$> o .: "tx"
            _ -> mzero

data TxHashStatusRes = TxHashStatusRes !TxHash !Bool !(Maybe Tx)
    deriving (Eq, Show, Read)

instance ToJSON TxHashStatusRes where
    toJSON (TxHashStatusRes h b p) = object $ concat
        [ [ "txhash"   .= h
          , "complete" .= b
          ]
        , if isJust p then [ "proposition" .= (fromJust p) ] else []
        ]

instance FromJSON TxHashStatusRes where
    parseJSON = withObject "txhashstatusres" $ \o ->
        TxHashStatusRes <$> o .:  "txhash"
                        <*> o .:  "complete"
                        <*> o .:? "proposition"

data TxRes = TxRes !Tx deriving (Eq, Show, Read)

instance ToJSON TxRes where
    toJSON (TxRes tx) = object [ "tx" .= tx ]

instance FromJSON TxRes where
    parseJSON = withObject "txres" $ \o -> TxRes <$> o .: "tx"
        
data TxStatusRes = TxStatusRes !Tx !Bool !(Maybe Tx)
    deriving (Eq, Show, Read)

instance ToJSON TxStatusRes where
    toJSON (TxStatusRes tx b p) = object $ concat
        [ [ "tx"       .= tx
          , "complete" .= b
          ]
        , if isJust p then [ "proposition" .= (fromJust p) ] else []
        ]

instance FromJSON TxStatusRes where
    parseJSON = withObject "txstatusres" $ \o ->
        TxStatusRes <$> o .: "tx"
                    <*> o .: "complete"
                    <*> o .:? "proposition"

data BalanceRes = BalanceRes !Balance ![TxHash]
    deriving (Eq, Show, Read)

instance ToJSON BalanceRes where
    toJSON (BalanceRes b hs) = object 
        [ "balance"   .= b
        , "conflicts" .= hs
        ]

instance FromJSON BalanceRes where
    parseJSON = withObject "balanceres" $ \o -> do
        b  <- o .: "balance"
        hs <- o .: "conflicts"
        return $ BalanceRes b hs

data SpendableRes = SpendableRes !Word64
    deriving (Eq, Show, Read)

instance ToJSON SpendableRes where
    toJSON (SpendableRes b) = object [ "balance" .= b ]

instance FromJSON SpendableRes where
    parseJSON = withObject "spendableres" $ \o -> 
        SpendableRes <$> o .: "balance"

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

