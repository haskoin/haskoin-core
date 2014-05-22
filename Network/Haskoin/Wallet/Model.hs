{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Haskoin.Wallet.Model 
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, DbTxGeneric(..)
, DbTxBlobGeneric(..)
, DbWalletId
, DbAccountId
, DbAddressId
, DbCoinId
, DbTxId
, DbTxBlobId
, EntityField(..)
, Unique(..)
, migrateAll
) where

import Data.Int (Int64)
import Data.Word (Word64)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.Sql ()
import Database.Persist.TH
    ( share
    , mkPersist
    , sqlSettings
    , mkMigrate
    , persistLowerCase
    )
import Network.Haskoin.Wallet.Types (CoinStatus)
import Network.Haskoin.Protocol (Script, Tx)
import Network.Haskoin.Crypto (MasterKey, AccPubKey, Hash256, XPubKey)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbWallet 
    name String
    type String
    master MasterKey 
    accIndex Int
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name
    deriving Show

DbAccount 
    name String
    index Int
    tree String
    key AccPubKey
    extIndex Int
    extGap Int
    intIndex Int
    intGap Int
    msRequired Int Maybe
    msTotal Int Maybe
    msKeys [XPubKey] 
    wallet DbWalletId
    created UTCTime default=CURRENT_TIME
    UniqueAccName name
    deriving Show

DbAddress 
    base58 String
    label String
    index Int
    tree String
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddress base58
    UniqueAddressKey account index internal
    deriving Show

DbCoin 
    txid Hash256
    pos Int
    value Word64
    script Script
    rdmScript Script Maybe
    address String 
    status CoinStatus
    account DbAccountId
    orphan Bool
    created UTCTime default=CURRENT_TIME
    CoinOutPoint txid pos
    deriving Show

DbTx 
    txid Hash256
    recipients [String]
    value Int64
    account DbAccountId
    orphan Bool
    partial Bool
    created UTCTime default=CURRENT_TIME
    UniqueTx txid account
    deriving Show

DbTxBlob
    txid Hash256
    value Tx
    created UTCTime default=CURRENT_TIME
    UniqueTxBlob txid
    deriving Show

|]

