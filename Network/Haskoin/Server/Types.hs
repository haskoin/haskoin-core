{-# LANGUAGE OverloadedStrings #-}
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
    | AccountList
    deriving (Eq, Show, Read)

encodeWalletRequest :: WalletRequest -> (Maybe Id) -> BS.ByteString
encodeWalletRequest wr i = 
    toStrictBS $ encode $ Request (walletMethod wr) (go wr) i
  where
    go (NewFullWallet n p m) = Just $ object $ concat
        [ [ "name"       .= n
          , "passphrase" .= p
          ]
        , if isJust m then ["mnemonic" .= (fromJust m)] else []
        ]
    go (NewReadWallet n k) = Just $ object
        [ "name" .= n
        , "key"  .= xPubExport k
        ]
    go (GetWallet n) = Just $ object [ "name" .= n ]
    go WalletList = Nothing
    go (NewAccount w n) = Just $ object
        [ "wallet" .= w
        , "name"   .= n
        ]
    go (NewMSAccount w n r t ks) = Just $ object
        [ "wallet"    .= w
        , "name"      .= n
        , "required" .= r
        , "total"     .= t
        , "keys"      .= map xPubExport ks
        ]
    go (AddAccountKeys n ks) = Just $ object
        [ "name" .= n
        , "keys" .= map xPubExport ks
        ]
    go AccountList = Nothing

walletMethod :: WalletRequest -> T.Text
walletMethod wr = case wr of
    NewFullWallet _ _ _    -> "network.haskoin.wallet.newfullwallet"
    NewReadWallet _ _      -> error "not implemented"
    GetWallet _            -> "network.haskoin.wallet.getwallet"
    WalletList             -> "network.haskoin.wallet.walletlist"
    NewAccount _ _         -> "network.haskoin.wallet.newaccount"
    NewMSAccount _ _ _ _ _ -> "network.haskoin.wallet.newmsaccount"
    AddAccountKeys _ _     -> "network.haskoin.wallet.addaccountkeys"
    AccountList            -> "network.haskoin.wallet.accountlist"

decodeWalletRequest :: BS.ByteString -> Either String (WalletRequest, Maybe Id)
decodeWalletRequest bs = 
    eitherDecode (toLazyBS bs) >>= \(Request m p i) -> do
        r <- parseEither (go m) p
        return (r,i)
  where
    go m p = case (m,p) of
        ("network.haskoin.wallet.newfullwallet", Just (Object o)) -> do
            n <- o .:  "name"
            p <- o .:  "passphrase"
            m <- o .:? "mnemonic"
            return $ NewFullWallet n p m
        ("network.haskoin.wallet.getwallet", (Just (Object o))) -> do
            n <- o .: "name"
            return $ GetWallet n
        ("network.haskoin.wallet.walletlist", Nothing) -> return WalletList
        ("network.haskoin.wallet.newaccount", Just (Object o)) -> do
            w <- o .:  "wallet"
            n <- o .:  "name"
            return $ NewAccount w n
        ("network.haskoin.wallet.newmsaccount", Just (Object o)) -> do
            w  <- o .:  "wallet"
            n  <- o .:  "name"
            r  <- o .:  "required"
            t  <- o .:  "total"
            ks <- o .:  "keys"
            let keysM = mapM xPubImport ks
            maybe mzero (return . (NewMSAccount w n r t)) keysM
        ("network.haskoin.wallet.addaccountkeys", Just (Object o)) -> do
            n  <- o .: "name"
            ks <- o .: "keys"
            let keysM = mapM xPubImport ks
            maybe mzero (return . (AddAccountKeys n)) keysM
        ("network.haskoin.wallet.accountlist", Nothing) -> return AccountList
        _ -> mzero

{- Response -}

data WalletResponse
    = ResMnemonic String
    | ResWallet Wallet
    | ResWalletList [Wallet]
    | ResAccount Account
    | ResAccountList [Account]
    deriving (Eq, Show, Read)

encodeWalletResponse :: (Either String WalletResponse, (Maybe Id)) 
                     -> BS.ByteString
encodeWalletResponse (wr, i) = toStrictBS $ encode $ 
    ( Response { resResult = go <$> (f wr)
               , resId     = i
               } :: Response Value String String
    )
  where
    f (Left err) = Left $ ErrVal err
    f (Right x)  = return x
    go (ResMnemonic s)     = object ["mnemonic" .= s]
    go (ResWallet w)       = object ["wallet" .= w]
    go (ResWalletList ws)  = object ["walletlist" .= ws]
    go (ResAccount a)      = object ["account" .= a]
    go (ResAccountList as) = object ["accountlist" .= as]

decodeWalletResponse :: BS.ByteString -> WalletRequest
                     -> Either String WalletResponse
decodeWalletResponse bs req = do
    response <- eitherDecode $ toLazyBS bs 
        :: Either String (Response Value Value String)
    let res = resResult response
    when (isLeft res) $ Left $ case fromLeft res of
        ErrObj _ err _ -> err
        ErrVal err     -> err
    parseEither (go req) $ fromRight res
  where
    go (NewFullWallet _ _ _) (Object o) = do
        m <- o .: "mnemonic" 
        return $ ResMnemonic m
    go (NewReadWallet _ _) _ = error "Not implemented"
    go (GetWallet _) (Object o) = do
        w <- o .: "wallet"
        return $ ResWallet w
    go WalletList (Object o) = do
        ws <- o .: "walletlist"
        return $ ResWalletList ws
    go (NewAccount _ _) (Object o) = do
        a <- o .: "account"
        return $ ResAccount a
    go (NewMSAccount _ _ _ _ _) (Object o) = do
        a <- o .: "account"
        return $ ResAccount a
    go (AddAccountKeys _ _) (Object o) = do
        a <- o .: "account"
        return $ ResAccount a
    go AccountList (Object o) = do
        as <- o .: "accountlist"
        return $ ResAccountList as
    go _ _ = mzero
    
