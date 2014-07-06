{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( Wallet(..)
, Account(..)
, PaymentAddress(..)
, AccTx(..)
, CoinStatus(..)
, catStatus
, WalletException(..)
) where

import Control.Monad (mzero, liftM2)
import Control.Exception (Exception)

import Data.Int (Int64)
import Data.Typeable (Typeable)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Data.Aeson
    ( Value (Object, String)
    , FromJSON
    , ToJSON
    , (.=)
    , (.:)
    , object
    , parseJSON
    , toJSON
    )

import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue 
    )
import Database.Persist.Types (PersistValue (PersistByteString))
import Database.Persist.TH (derivePersistField)
import Database.Persist.Sql (PersistFieldSql, SqlType (SqlBlob), sqlType)

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util

data WalletException = WalletException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

-- | Spent if a complete transaction spends this coin
-- Reserved if a partial transaction is spending these coins
-- Unspent if the coins are still available
-- The purpose of the Reserved status is to block this coin from being used in
-- subsequent coin selection algorithms. However, Reserved coins can always be
-- spent (set status to Spent) by complete transactions.
data CoinStatus = Spent TxHash | Reserved TxHash | Unspent
    deriving (Show, Read, Eq)

catStatus :: [CoinStatus] -> [TxHash]
catStatus = foldr f []
  where
    f (Spent h) acc    = h:acc
    f (Reserved h) acc = h:acc
    f _ acc            = acc

-- TODO: Add NFData instances for all those types
data Wallet 
    = WalletFull
        { walletName      :: String
        , walletMasterKey :: MasterKey
        } 
    | WalletRead
        { walletName      :: String
        , walletPubKey    :: XPubKey
        }
    deriving (Eq, Show, Read)

instance ToJSON Wallet where
    toJSON (WalletFull n k) = object
        [ "type"   .= String "full"
        , "name"   .= n
        , "master" .= (xPrvExport $ masterKey k)
        ]
    toJSON (WalletRead n k) = object
        [ "type"   .= String "read"
        , "name"   .= n
        , "key"    .= xPubExport k
        ]

instance FromJSON Wallet where
    parseJSON (Object o) = do
        x <- o .: "type"
        n <- o .: "name"
        case x of
            String "full" -> do
                m <- o .: "master" 
                let masterM = loadMasterKey =<< xPrvImport m
                maybe mzero (return . (WalletFull n)) masterM
            String "read" -> do
                k <- o .: "key"
                maybe mzero (return . (WalletRead n)) $ xPubImport k
            _ -> mzero
    parseJSON _ = mzero

data Account
    = RegularAccount 
        { accountName   :: String
        , accountWallet :: String
        , accountIndex  :: KeyIndex
        , accountKey    :: AccPubKey
        }
    | MultisigAccount
        { accountName     :: String
        , accountWallet   :: String
        , accountIndex    :: KeyIndex
        , accountKey      :: AccPubKey
        , accountRequired :: Int
        , accountTotal    :: Int
        , accountKeys     :: [XPubKey]
        }
    deriving (Eq, Show, Read)

instance ToJSON Account where
    toJSON (RegularAccount n w i k) = object
        [ "type"   .= String "regular"
        , "name"   .= n
        , "wallet" .= w
        , "index"  .= i
        , "key"    .= (xPubExport $ getAccPubKey k)
        ]
    toJSON (MultisigAccount n w i k r t ks) = object
        [ "type"     .= String "multisig"
        , "name"     .= n
        , "wallet"   .= w
        , "index"    .= i
        , "key"      .= (xPubExport $ getAccPubKey k)
        , "required" .= r
        , "total"    .= t
        , "keys"     .= map xPubExport ks
        ]

instance FromJSON Account where
    parseJSON (Object o) = do
        x <- o .: "type"
        n <- o .: "name"
        w <- o .: "wallet"
        i <- o .: "index"
        k <- o .: "key"
        let keyM = loadPubAcc =<< xPubImport k
        case x of
            String "regular" -> 
                maybe mzero (return . (RegularAccount n w i)) keyM
            String "multisig" -> do
                r  <- o .: "required"
                t  <- o .: "total"
                ks <- o .: "keys"
                let keysM      = mapM xPubImport ks
                    f (k',ks') = return $ MultisigAccount n w i k' r t ks'
                maybe mzero f $ liftM2 (,) keyM keysM
            _ -> mzero
      where
        
    parseJSON _ = mzero

data PaymentAddress = PaymentAddress 
    { paymentAddress :: Address
    , addressLabel   :: String
    , addressIndex   :: KeyIndex
    } deriving (Eq, Show, Read)

instance ToJSON PaymentAddress where
    toJSON (PaymentAddress a l i) = object
        [ "address" .= addrToBase58 a
        , "label"   .= l
        , "index"   .= i
        ]

instance FromJSON PaymentAddress where
    parseJSON (Object o) = do
        a <- o .: "address"
        l <- o .: "label"
        i <- o .: "index"
        let f add = return $ PaymentAddress add l i
        maybe mzero f $ base58ToAddr a
    parseJSON _ = mzero

data AccTx = AccTx
    { accTxHash          :: TxHash
    , accTxRecipients    :: [Address]
    , accTxValue         :: Int64
    , accTxOffline       :: Bool
    , accTxConfirmations :: Int
    } deriving (Eq, Show, Read)

instance ToJSON AccTx where
    toJSON (AccTx h as v x c) = object
        [ "txid"          .= encodeTxHashLE h 
        , "recipients"    .= map addrToBase58 as
        , "value"         .= v
        , "offline"       .= x
        , "confirmations" .= c
        ]

instance FromJSON AccTx where
    parseJSON (Object o) = do
        h  <- o .: "txid"
        as <- o .: "recipients"
        v  <- o .: "value"
        x  <- o .: "offline"
        c  <- o .: "confirmations"
        let txidM          = decodeTxHashLE h
            addrsM         = mapM base58ToAddr as
            f (tid, addrs) = return $ AccTx tid addrs v x c
        maybe mzero f $ liftM2 (,) txidM addrsM
    parseJSON _ = mzero

{- Instances for PersistField and PersistFieldSql -}

derivePersistField "CoinStatus"
derivePersistField "Coin"
derivePersistField "TxHash"
derivePersistField "BlockHash"
derivePersistField "Address"
derivePersistField "Wallet"
derivePersistField "Account"

instance PersistField Tx where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = case txE of
        Right tx -> Right tx
        Left str -> Left $ T.pack str
      where
        txE = decodeToEither bs
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob


