{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Haskoin.Server.Types 
( WalletRequest(..)
, WalletResponse(..)
, encodeWalletRequest
, decodeWalletRequest
, encodeWalletResponse
, decodeWalletResponse
)
where

import Control.Monad (mzero)

import Data.Word (Word32, Word64)
import Data.Maybe (isJust, fromJust)
import Data.Aeson 
    ( Value (Object, String, Null)
    , object
    , ToJSON
    , toJSON
    , encode
    , eitherDecodeStrict
    , (.:), (.:?), (.=)
    )
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Text as T (Text, unpack)
import qualified Data.ByteString as BS (ByteString)

import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Stratum
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Types

{- Request -}

-- TODO: Create NFData intstances for WalletRequest and WalletResponse
data WalletRequest 
    = NewWallet WalletName String (Maybe String)
    | GetWallet WalletName
    | WalletList 
    | NewAccount WalletName AccountName
    | NewMSAccount WalletName AccountName Int Int [XPubKey]
    | NewReadAccount AccountName XPubKey
    | AddAccountKeys AccountName [XPubKey]
    | GetAccount AccountName
    | AccountList
    | GenAddress AccountName Int
    | AddressLabel AccountName KeyIndex String
    | AddressList AccountName
    | AddressPage AccountName Int Int
    | TxList AccountName
    | TxPage AccountName Int Int
    | TxSend AccountName [(Address, Word64)] Word64
    | TxSign AccountName Tx
    | TxGet TxHash
    | Balance AccountName
    | Rescan (Maybe Word32)
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
        TxGet h -> object [ "txhash" .= h ]
        Balance n -> object [ "accountname" .= n ]
        Rescan (Just t) -> object [ "timestamp" .= t ]
        Rescan Nothing  -> Null

data WalletResponse
    = ResMnemonic String
    | ResWallet Wallet
    | ResWalletList [Wallet]
    | ResAccount Account
    | ResAccountList [Account]
    | ResAddress PaymentAddress
    | ResAddressList [PaymentAddress]
    | ResAddressPage [PaymentAddress] Int -- Int = Max page number
    | ResAccTxList [AccTx]
    | ResAccTxPage [AccTx] Int -- Int = Max page number
    | ResTxStatus TxHash Bool
    | ResTx Tx
    | ResBalance Word64
    | ResRescan Word32
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
        ResTxStatus h b -> object 
            [ "txhash"   .= h
            , "complete" .= b
            ]
        ResTx tx -> object [ "tx" .= tx ]
        ResBalance b -> object [ "balance" .= b ]
        ResRescan t -> object [ "timestamp" .= t ]

instance RPCRequest WalletRequest String WalletResponse where
    rpcMethod   = walletMethod
    parseRPCParams = parseWalletRequest
    parseRPCResult = parseWalletResponse
    parseRPCError _ (String s) = return $ T.unpack s
    parseRPCError _ _          = mzero

walletMethod :: WalletRequest -> T.Text
walletMethod wr = case wr of
    NewWallet _ _ _        -> "network.haskoin.wallet.newwallet"
    GetWallet _            -> "network.haskoin.wallet.getwallet"
    WalletList             -> "network.haskoin.wallet.walletlist"
    NewAccount _ _         -> "network.haskoin.wallet.newaccount"
    NewMSAccount _ _ _ _ _ -> "network.haskoin.wallet.newmsaccount"
    NewReadAccount _ _     -> "network.haskoin.wallet.newreadaccount"
    AddAccountKeys _ _     -> "network.haskoin.wallet.addaccountkeys"
    GetAccount _           -> "network.haskoin.wallet.getaccount"
    AccountList            -> "network.haskoin.wallet.accountlist"
    GenAddress _ _         -> "network.haskoin.wallet.genaddress"
    AddressLabel _ _ _     -> "network.haskoin.wallet.addresslabel"
    AddressList _          -> "network.haskoin.wallet.addresslist"
    AddressPage _ _ _      -> "network.haskoin.wallet.addresspage"
    TxList _               -> "network.haskoin.wallet.txlist"
    TxPage _ _ _           -> "network.haskoin.wallet.txpage"
    TxSend _ _ _           -> "network.haskoin.wallet.txsend"
    TxSign _ _             -> "network.haskoin.wallet.txsign"
    TxGet _                -> "network.haskoin.wallet.txget"
    Balance _              -> "network.haskoin.wallet.balance"
    Rescan _               -> "network.haskoin.wallet.rescan"

parseWalletRequest :: Method -> Value -> Parser WalletRequest
parseWalletRequest m v = case (m,v) of
    ("network.haskoin.wallet.newwallet", Object o) -> do
        n <- o .:  "walletname"
        p <- o .:  "passphrase"
        x <- o .:? "mnemonic"
        return $ NewWallet n p x
    ("network.haskoin.wallet.getwallet", Object o) -> do
        n <- o .: "walletname"
        return $ GetWallet n
    ("network.haskoin.wallet.walletlist", Null) -> return WalletList
    ("network.haskoin.wallet.newaccount", Object o) -> do
        w <- o .:  "walletname"
        n <- o .:  "accountname"
        return $ NewAccount w n
    ("network.haskoin.wallet.newmsaccount", Object o) -> do
        w  <- o .:  "walletname"
        n  <- o .:  "accountname"
        r  <- o .:  "required"
        t  <- o .:  "total"
        ks <- o .:  "keys"
        let keysM = mapM xPubImport ks
        maybe mzero (return . (NewMSAccount w n r t)) keysM
    ("network.haskoin.wallet.newreadaccount", Object o) -> do
        n <- o .: "accountname" 
        k <- o .: "key"
        let keyM = xPubImport k
        maybe mzero (return . NewReadAccount n) keyM
    ("network.haskoin.wallet.addaccountkeys", Object o) -> do
        n  <- o .: "accountname"
        ks <- o .: "keys"
        let keysM = mapM xPubImport ks
        maybe mzero (return . (AddAccountKeys n)) keysM
    ("network.haskoin.wallet.getaccount", Object o) -> do
        n <- o .: "accountname"
        return $ GetAccount n
    ("network.haskoin.wallet.accountlist", Null) -> return AccountList
    ("network.haskoin.wallet.genaddress", Object o) -> do
        n <- o .: "accountname"
        i <- o .: "count"
        return $ GenAddress n i
    ("network.haskoin.wallet.addresslabel", Object o) -> do
        n <- o .: "accountname"
        i <- o .: "index"
        l <- o .: "label"
        return $ AddressLabel n i l
    ("network.haskoin.wallet.addresslist", Object o) -> do
        n <- o .: "accountname"
        return $ AddressList n
    ("network.haskoin.wallet.addresspage", Object o) -> do
        n <- o .: "accountname"
        p <- o .: "page"
        a <- o .: "addrperpage"
        return $ AddressPage n p a
    ("network.haskoin.wallet.txlist", Object o) -> do
        n <- o .: "accountname"
        return $ TxList n
    ("network.haskoin.wallet.txpage", Object o) -> do
        n <- o .: "accountname"
        p <- o .: "page"
        t <- o .: "txperpage"
        return $ TxPage n p t
    ("network.haskoin.wallet.txsend", Object o) -> do
        n  <- o .: "accountname"
        xs <- o .: "recipients"
        f  <- o .: "fee"
        return $ TxSend n xs f
    ("network.haskoin.wallet.txsign", Object o) -> do
        n  <- o .: "accountname"
        tx <- o .: "tx"
        return $ TxSign n tx
    ("network.haskoin.wallet.txget", Object o) -> do
        h <- o .: "txhash"
        return $ TxGet h
    ("network.haskoin.wallet.balance", Object o) -> do
        n <- o .: "accountname"
        return $ Balance n
    ("network.haskoin.wallet.rescan", Object o) -> do
        t <- o .: "timestamp"
        return $ Rescan $ Just t
    ("network.haskoin.wallet.rescan", Null) -> return $ Rescan Nothing
    _ -> mzero

parseWalletResponse :: WalletRequest -> Value -> Parser WalletResponse
parseWalletResponse w v = case (w, v) of
    -- TODO: refactor this? We decode many times the same type.
    (NewWallet _ _ _, Object o) -> do
        m <- o .: "mnemonic" 
        return $ ResMnemonic m
    (GetWallet _, Object o) -> do
        x <- o .: "wallet"
        return $ ResWallet x
    (WalletList, Object o) -> do
        ws <- o .: "walletlist"
        return $ ResWalletList ws
    (NewAccount _ _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (NewMSAccount _ _ _ _ _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (NewReadAccount _ _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (AddAccountKeys _ _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (GetAccount _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (AccountList, Object o) -> do
        as <- o .: "accountlist"
        return $ ResAccountList as
    (GenAddress _ _, Object o) -> do
        as <- o .: "addresslist" 
        return $ ResAddressList as
    (AddressLabel _ _ _, Object o) -> do
        a <- o .: "address"
        return $ ResAddress a
    (AddressList _, Object o) -> do
        as <- o .: "addresslist"
        return $ ResAddressList as
    (AddressPage _ _ _, Object o) -> do
        as <- o .: "addresspage"
        m  <- o .: "maxpage"
        return $ ResAddressPage as m
    (TxList _, Object o) -> do
        xs <- o .: "txlist"
        return $ ResAccTxList xs
    (TxPage _ _ _, Object o) -> do
        xs <- o .: "txpage"
        m  <- o .: "maxpage"
        return $ ResAccTxPage xs m
    (TxSend _ _ _, Object o) -> do
        h <- o .: "txhash"
        b <- o .: "complete"
        return $ ResTxStatus h b
    (TxSign _ _, Object o) -> do
        h <- o .: "txhash"
        b <- o .: "complete"
        return $ ResTxStatus h b
    (TxGet _, Object o) -> do
        tx <- o .: "tx"
        return $ ResTx tx
    (Balance _, Object o) -> do
        b <- o .: "balance"
        return $ ResBalance b
    (Rescan _, Object o) -> do
        t <- o .: "timestamp"
        return $ ResRescan t
    _ -> mzero

encodeWalletRequest :: WalletRequest -> Int -> BS.ByteString
encodeWalletRequest wr i = toStrictBS $ encode $ encodeRPCRequest wr i

decodeWalletRequest :: BS.ByteString -> Either String (WalletRequest, Int)
decodeWalletRequest bs = parseEither parseRPCRequest =<< eitherDecodeStrict bs

encodeWalletResponse :: Either String WalletResponse -> Int -> BS.ByteString
encodeWalletResponse resE i = toStrictBS $ encode $ case resE of
    Left err  -> encodeRPCError err i
    Right res -> encodeRPCResponse res i

decodeWalletResponse :: WalletRequest -> BS.ByteString 
                     -> Either String (Either String WalletResponse, Int)
decodeWalletResponse req bs = do
    v <- eitherDecodeStrict bs
    parseEither (parseRPCResponse req) v

