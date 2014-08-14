{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.REST.Types 
( NewWallet(..) 
, MnemonicRes(..)
, NewAccount(..)
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

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

{- Request -}
{-

-- TODO: Create NFData intstances for WalletRequest and WalletResponse
data WalletRequest 
    = NewWallet !WalletName !String !(Maybe String)
    | GetWallet !WalletName
    | WalletList 
    | NewAccount !WalletName !AccountName
    | NewMSAccount !WalletName !AccountName !Int !Int ![XPubKey]
    | NewReadAccount !AccountName !XPubKey
    | NewReadMSAccount !AccountName !Int !Int ![XPubKey]
    | AddAccountKeys !AccountName ![XPubKey]
    | GetAccount !AccountName
    | AccountList
    | GenAddress !AccountName !Int
    | AddressLabel !AccountName !KeyIndex !String
    | AddressList !AccountName
    | AddressPage !AccountName !Int !Int
    | TxList !AccountName
    | TxPage !AccountName !Int !Int
    | TxSend !AccountName ![(Address, Word64)] !Word64
    | TxSign !AccountName !Tx
    | GetSigBlob !AccountName !TxHash
    | SignSigBlob !AccountName !SigBlob
    | TxGet !TxHash
    | Balance !AccountName
    | Rescan !(Maybe Word32)
    deriving (Eq, Show, Read)

instance ToJSON WalletRequest where
    toJSON req = case req of
        NewWallet n p m -> object $ concat
            [ [ "walletname" .= n
              , "passphrase" .= p
              ]
            , if isJust m then ["mnemonic" .= (fromJust m)] else []
            ]
        GetWallet n -> object [ "walletname" .= n ]
        WalletList -> Null
        NewAccount w n -> object
            [ "walletname"  .= w
            , "accountname" .= n
            ]
        NewMSAccount w n r t ks -> object
            [ "walletname"  .= w
            , "accountname" .= n
            , "required"    .= r
            , "total"       .= t
            , "keys"        .= map xPubExport ks
            ]
        NewReadAccount n k -> object
            [ "accountname" .= n
            , "key"         .= xPubExport k
            ]
        NewReadMSAccount n r t ks -> object
            [ "accountname" .= n
            , "required"    .= r
            , "total"       .= t
            , "keys"        .= map xPubExport ks
            ]
        AddAccountKeys n ks -> object
            [ "accountname" .= n
            , "keys"        .= map xPubExport ks
            ]
        GetAccount n -> object [ "accountname" .= n ]
        AccountList -> Null
        GenAddress n i -> object
            [ "accountname" .= n
            , "count"       .= i
            ]
        AddressLabel n i l -> object
            [ "accountname" .= n
            , "index"       .= i
            , "label"       .= l
            ]
        AddressList n -> object [ "accountname" .= n ]
        AddressPage n p a -> object
            [ "accountname" .= n
            , "page"        .= p
            , "addrperpage" .= a
            ]
        TxList n -> object [ "accountname" .= n]
        TxPage n p t -> object
            [ "accountname" .= n
            , "page"        .= p
            , "txperpage"   .= t
            ]
        TxSend n rs f -> object
            [ "accountname" .= n
            , "recipients"  .= rs
            , "fee"         .= f
            ]
        TxSign n tx -> object
            [ "accountname" .= n
            , "tx"          .= tx
            ]
        GetSigBlob n h -> object
            [ "accountname" .= n
            , "txhash"      .= h
            ]
        SignSigBlob n b -> object
            [ "accountname" .= n
            , "sigblob"     .= b
            ]
        TxGet h -> object [ "txhash" .= h ]
        Balance n -> object [ "accountname" .= n ]
        Rescan (Just t) -> object [ "timestamp" .= t ]
        Rescan Nothing  -> Null

data WalletResponse
    = ResMnemonic !String
    | ResWallet !Wallet
    | ResWalletList ![Wallet]
    | ResAccount !Account
    | ResAccountList ![Account]
    | ResAddress !PaymentAddress
    | ResAddressList ![PaymentAddress]
    | ResAddressPage ![PaymentAddress] !Int -- Int = Max page number
    | ResAccTxList ![AccTx]
    | ResAccTxPage ![AccTx] !Int -- Int = Max page number
    | ResTxStatus !Tx !Bool
    | ResTxHashStatus !TxHash !Bool
    | ResTx !Tx
    | ResSigBlob !SigBlob
    | ResBalance !Word64
    | ResRescan !Word32
    deriving (Eq, Show, Read)

instance ToJSON WalletResponse where
    toJSON res = case res of
        ResMnemonic s     -> object ["mnemonic" .= s]
        ResWallet w       -> object ["wallet" .= w]
        ResWalletList ws  -> object ["walletlist" .= ws]
        ResAccount a      -> object ["account" .= a]
        ResAccountList as -> object ["accountlist" .= as]
        ResAddressList as -> object ["addresslist" .= as]
        ResAddress a      -> object ["address" .= a]
        ResAddressPage as m -> object
            [ "addresspage" .= as
            , "maxpage"     .= m
            ]
        ResAccTxList xs -> object ["txlist" .= xs]
        ResAccTxPage xs m -> object
            [ "txpage"  .= xs
            , "maxpage" .= m
            ]
        ResTxStatus t b -> object 
            [ "tx"       .= t
            , "complete" .= b
            ]
        ResTxHashStatus h b -> object 
            [ "txhash"   .= h
            , "complete" .= b
            ]
        ResTx tx -> object [ "tx" .= tx ]
        ResSigBlob b -> object [ "sigblob" .= b ]
        ResBalance b -> object [ "balance" .= b ]
        ResRescan t -> object [ "timestamp" .= t ]

instance ToRequest WalletRequest where
    requestMethod   = walletMethod

instance FromRequest WalletRequest where
    paramsParser = parseWalletRequest

instance FromResponse WalletResponse where
    parseResult = parseWalletResponse

walletMethod :: WalletRequest -> Method
walletMethod wr = case wr of
    NewWallet _ _ _          -> "network.haskoin.wallet.newwallet"
    GetWallet _              -> "network.haskoin.wallet.getwallet"
    WalletList               -> "network.haskoin.wallet.walletlist"
    NewAccount _ _           -> "network.haskoin.wallet.newaccount"
    NewMSAccount _ _ _ _ _   -> "network.haskoin.wallet.newmsaccount"
    NewReadAccount _ _       -> "network.haskoin.wallet.newreadaccount"
    NewReadMSAccount _ _ _ _ -> "network.haskoin.wallet.newreadmsaccount"
    AddAccountKeys _ _       -> "network.haskoin.wallet.addaccountkeys"
    GetAccount _             -> "network.haskoin.wallet.getaccount"
    AccountList              -> "network.haskoin.wallet.accountlist"
    GenAddress _ _           -> "network.haskoin.wallet.genaddress"
    AddressLabel _ _ _       -> "network.haskoin.wallet.addresslabel"
    AddressList _            -> "network.haskoin.wallet.addresslist"
    AddressPage _ _ _        -> "network.haskoin.wallet.addresspage"
    TxList _                 -> "network.haskoin.wallet.txlist"
    TxPage _ _ _             -> "network.haskoin.wallet.txpage"
    TxSend _ _ _             -> "network.haskoin.wallet.txsend"
    TxSign _ _               -> "network.haskoin.wallet.txsign"
    GetSigBlob _ _           -> "network.haskoin.wallet.getsigblob"
    SignSigBlob _ _          -> "network.haskoin.wallet.signsigblob"
    TxGet _                  -> "network.haskoin.wallet.txget"
    Balance _                -> "network.haskoin.wallet.balance"
    Rescan _                 -> "network.haskoin.wallet.rescan"

parseWalletRequest :: Method -> Maybe (Value -> Parser WalletRequest)
parseWalletRequest "network.haskoin.wallet.newwallet" = Just $
    withObject "newwallet" $ \o ->
        NewWallet
            <$> o .: "walletname"
            <*> o .: "passphrase"
            <*> o .:? "mnemonic"
parseWalletRequest "network.haskoin.wallet.getwallet" = Just $
    withObject "getwallet" $ \o ->
        GetWallet
            <$> o .: "walletname"
parseWalletRequest "network.haskoin.wallet.walletlist" = Just $
    const $ return $
        WalletList
parseWalletRequest "network.haskoin.wallet.newaccount" = Just $
    withObject "newaccount" $ \o ->
        NewAccount
            <$> o .: "walletname"
            <*> o .: "accountname"
parseWalletRequest "network.haskoin.wallet.newmsaccount" = Just $
    withObject "newmsaccount" $ \o ->
        NewMSAccount
            <$> o .: "walletname"
            <*> o .: "accountname"
            <*> o .: "required"
            <*> o .: "total"
            <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
parseWalletRequest "network.haskoin.wallet.newreadaccount" = Just $
    withObject "newreadaccount" $ \o ->
        NewReadAccount
            <$> o .: "accountname"
            <*> (o .: "key" >>= maybe mzero return . xPubImport)
parseWalletRequest "network.haskoin.wallet.newreadmsaccount" = Just $
    withObject "newreadmsaccount" $ \o ->
        NewReadMSAccount
            <$> o .: "accountname"
            <*> o .: "required"
            <*> o .: "total"
            <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
parseWalletRequest "network.haskoin.wallet.addaccountkeys" = Just $
    withObject "addaccountkeys" $ \o ->
        AddAccountKeys
            <$> o .: "accountname"
            <*> (o .: "keys" >>= maybe mzero return . mapM xPubImport)
parseWalletRequest "network.haskoin.wallet.getaccount" = Just $
    withObject "getaccount" $ \o ->
        GetAccount
            <$> o .: "accountname"
parseWalletRequest "network.haskoin.wallet.accountlist" = Just $
    const $ return $
        AccountList
parseWalletRequest "network.haskoin.wallet.genaddress" = Just $
    withObject "genaddress" $ \o ->
        GenAddress
            <$> o .: "accountname"
            <*> o .: "count"
parseWalletRequest "network.haskoin.wallet.addresslabel" = Just $
    withObject "addresslabel" $ \o ->
        AddressLabel
            <$> o .: "accountname"
            <*> o .: "index"
            <*> o .: "label"
parseWalletRequest "network.haskoin.wallet.addresslist" = Just $
    withObject "addresslist" $ \o ->
        AddressList
            <$> o .: "accountname"
parseWalletRequest "network.haskoin.wallet.addresspage" = Just $
    withObject "addresspage" $ \o ->
        AddressPage
            <$> o .: "accountname"
            <*> o .: "page"
            <*> o .: "addrperpage"
parseWalletRequest "network.haskoin.wallet.txlist" = Just $
    withObject "txlist" $ \o ->
        TxList
            <$> o .: "accountname"
parseWalletRequest "network.haskoin.wallet.txpage" = Just $
    withObject "txpage" $ \o ->
        TxPage
            <$> o .: "accountname"
            <*> o .: "page"
            <*> o .: "txperpage"
parseWalletRequest "network.haskoin.wallet.txsend" = Just $
    withObject "txsend" $ \o ->
        TxSend
            <$> o .: "accountname"
            <*> o .: "recipients"
            <*> o .: "fee"
parseWalletRequest "network.haskoin.wallet.txsign" = Just $
    withObject "txsign" $ \o ->
        TxSign
            <$> o .: "accountname"
            <*> o .: "tx"
parseWalletRequest "network.haskoin.wallet.getsigblob" = Just $
    withObject "getsigblob" $ \o ->
        GetSigBlob
            <$> o .: "accountname"
            <*> o .: "txhash"
parseWalletRequest "network.haskoin.wallet.signsigblob" = Just $
    withObject "signsigblob" $ \o ->
        SignSigBlob
            <$> o .: "accountname"
            <*> o .: "sigblob"
parseWalletRequest "network.haskoin.wallet.txget" = Just $
    withObject "txget" $ \o ->
        TxGet
            <$> o .: "txhash"
parseWalletRequest "network.haskoin.wallet.balance" = Just $
    withObject "balance" $ \o ->
        Balance
            <$> o .: "accountname"
parseWalletRequest "network.haskoin.wallet.rescan" = Just $ \v -> case v of
    Null -> return $
        Rescan Nothing
    (Object o) ->
        Rescan
            <$> o .:? "timestamp"
    _ -> mzero
parseWalletRequest _ = mzero

parseWalletResponse :: Method -> Value -> Parser WalletResponse
parseWalletResponse "network.haskoin.wallet.newwallet" =
    withObject "newwallet" $ \o -> ResMnemonic <$> o .: "mnemonic"
parseWalletResponse "network.haskoin.wallet.getwallet" =
    withObject "getwallet" $ \o -> ResWallet <$> o .: "wallet"
parseWalletResponse "network.haskoin.wallet.walletlist" =
    withObject "walletlist" $ \o -> ResWalletList <$> o .: "walletlist"
parseWalletResponse "network.haskoin.wallet.newaccount" =
    withObject "newaccount" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.newmsaccount" =
    withObject "newmsaccount" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.newreadaccount" =
    withObject "newreadaccount" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.newreadmsaccount" =
    withObject "newreadmsaccount" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.addaccountkeys" =
    withObject "addaccountkeys" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.getaccount" =
    withObject "getaccount" $ \o -> ResAccount <$> o .: "account"
parseWalletResponse "network.haskoin.wallet.accountlist" =
    withObject "accountlist" $ \o -> ResAccountList <$> o .: "accountlist"
parseWalletResponse "network.haskoin.wallet.genaddress" =
    withObject "genaddress" $ \o -> ResAddressList <$> o .: "addresslist"
parseWalletResponse "network.haskoin.wallet.addresslabel" =
    withObject "addresslabel" $ \o -> ResAddress <$> o .: "address"
parseWalletResponse "network.haskoin.wallet.addresslist" =
    withObject "addresslist" $ \o -> ResAddressList <$> o .: "addresslist"
parseWalletResponse "network.haskoin.wallet.addresspage" =
    withObject "addresspage" $ \o -> ResAddressPage <$> o .: "addresspage"
                                                    <*> o .: "maxpage"
parseWalletResponse "network.haskoin.wallet.txlist" =
    withObject "txlist" $ \o -> ResAccTxList <$> o .: "txlist"
parseWalletResponse "network.haskoin.wallet.txpage" =
    withObject "txpage" $ \o -> ResAccTxPage <$> o .: "txpage"
                                             <*> o .: "maxpage"
parseWalletResponse "network.haskoin.wallet.txsend" =
    withObject "txsend" $ \o -> ResTxHashStatus <$> o .: "txhash"
                                                <*> o .: "complete"
parseWalletResponse "network.haskoin.wallet.txsign" =
    withObject "txsign" $ \o -> ResTxHashStatus <$> o .: "txhash"
                                                <*> o .: "complete"
parseWalletResponse "network.haskoin.wallet.getsigblob" =
    withObject "getsigblob" $ \o -> ResSigBlob <$> o .: "sigblob"
parseWalletResponse "network.haskoin.wallet.signsigblob" =
    withObject "signsigblob" $ \o -> ResTxStatus <$> o .: "tx"
                                                 <*> o .: "complete"
parseWalletResponse "network.haskoin.wallet.txget" =
    withObject "txget" $ \o -> ResTx <$> o .: "tx"
parseWalletResponse "network.haskoin.wallet.balance" =
    withObject "balance" $ \o -> ResBalance <$> o .: "balance"
parseWalletResponse "network.haskoin.wallet.rescan" =
    withObject "rescan" $ \o -> ResRescan <$> o .: "timestamp"
parseWalletResponse _ = const mzero

-}
