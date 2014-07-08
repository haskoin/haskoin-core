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

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.DeepSeq (NFData, rnf)

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit 
    ( Sink
    , awaitForever
    , yield
    , addCleanup
    , ($$), ($=), (=$)
    )
import Data.Conduit.Network
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite

import Network.Haskoin.Crypto
import Network.Haskoin.Stratum
import Network.Haskoin.Util

import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

{- Request -}

-- TODO: Create NFData intstances for WalletRequest and WalletResponse
data WalletRequest 
    = NewFullWallet WalletName String (Maybe String)
    | NewReadWallet WalletName XPubKey
    | GetWallet WalletName
    | WalletList 
    | NewAccount WalletName AccountName
    | NewMSAccount WalletName AccountName Int Int [XPubKey]
    | AddAccountKeys AccountName [XPubKey]
    | GetAccount AccountName
    | AccountList
    | GenAddress AccountName Int
    | AddressLabel AccountName KeyIndex String
    | AddressList AccountName
    | AddressPage AccountName Int Int
    deriving (Eq, Show, Read)

instance ToJSON WalletRequest where
    toJSON req = case req of
        NewFullWallet n p m -> object $ concat
            [ [ "walletname" .= n
              , "passphrase" .= p
              ]
            , if isJust m then ["mnemonic" .= (fromJust m)] else []
            ]
        NewReadWallet n k -> object
            [ "walletname" .= n
            , "key"        .= xPubExport k
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

data WalletResponse
    = ResMnemonic String
    | ResWallet Wallet
    | ResWalletList [Wallet]
    | ResAccount Account
    | ResAccountList [Account]
    | ResAddress PaymentAddress
    | ResAddressList [PaymentAddress]
    | ResAddressPage [PaymentAddress] Int -- Int = Max page number
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
            [ "addresslist" .= as
            , "maxpage"     .= m
            ]

instance RPCRequest WalletRequest String WalletResponse where
    rpcMethod   = walletMethod
    parseParams = parseWalletRequest
    parseResult = parseWalletResponse
    parseError _ (String s) = return $ T.unpack s
    parseError _ _          = mzero

walletMethod :: WalletRequest -> T.Text
walletMethod wr = case wr of
    NewFullWallet _ _ _    -> "network.haskoin.wallet.newfullwallet"
    NewReadWallet _ _      -> error "not implemented"
    GetWallet _            -> "network.haskoin.wallet.getwallet"
    WalletList             -> "network.haskoin.wallet.walletlist"
    NewAccount _ _         -> "network.haskoin.wallet.newaccount"
    NewMSAccount _ _ _ _ _ -> "network.haskoin.wallet.newmsaccount"
    AddAccountKeys _ _     -> "network.haskoin.wallet.addaccountkeys"
    GetAccount _           -> "network.haskoin.wallet.getaccount"
    AccountList            -> "network.haskoin.wallet.accountlist"
    GenAddress _ _         -> "network.haskoin.wallet.genaddress"
    AddressLabel _ _ _     -> "network.haskoin.wallet.addresslabel"
    AddressList _          -> "network.haskoin.wallet.addresslist"
    AddressPage _ _ _      -> "network.haskoin.wallet.addresspage"

parseWalletRequest :: Method -> Value -> Parser WalletRequest
parseWalletRequest m v = case (m,v) of
    ("network.haskoin.wallet.newfullwallet", Object o) -> do
        n <- o .:  "walletname"
        p <- o .:  "passphrase"
        m <- o .:? "mnemonic"
        return $ NewFullWallet n p m
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
    _ -> mzero

parseWalletResponse :: WalletRequest -> Value -> Parser WalletResponse
parseWalletResponse w v = case (w, v) of
    -- TODO: refactor this? We decode many times the same type.
    (NewFullWallet _ _ _, Object o) -> do
        m <- o .: "mnemonic" 
        return $ ResMnemonic m
    (NewReadWallet _ _, _) -> error "Not implemented"
    (GetWallet _, Object o) -> do
        w <- o .: "wallet"
        return $ ResWallet w
    (WalletList, Object o) -> do
        ws <- o .: "walletlist"
        return $ ResWalletList ws
    (NewAccount _ _, Object o) -> do
        a <- o .: "account"
        return $ ResAccount a
    (NewMSAccount _ _ _ _ _, Object o) -> do
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
        as <- o .: "addresslist"
        m  <- o .: "maxpage"
        return $ ResAddressPage as m
    _ -> mzero

encodeWalletRequest :: WalletRequest -> Int -> BS.ByteString
encodeWalletRequest wr i = toStrictBS $ encode $ encodeRequest wr i

decodeWalletRequest :: BS.ByteString -> Either String (WalletRequest, Int)
decodeWalletRequest bs = parseEither parseRequest =<< eitherDecodeStrict bs

encodeWalletResponse :: Either String WalletResponse -> Int -> BS.ByteString
encodeWalletResponse resE i = toStrictBS $ encode $ case resE of
    Left err  -> encodeError err i
    Right res -> encodeResponse res i

decodeWalletResponse :: WalletRequest -> BS.ByteString 
                     -> Either String (Either String WalletResponse, Int)
decodeWalletResponse req bs = do
    v <- eitherDecodeStrict bs
    parseEither (parseResponse req) v

