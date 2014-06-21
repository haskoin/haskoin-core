{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Network.Haskoin.Wallet.Model 
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, DbAccTxGeneric(..)
, DbTxGeneric(..)
, DbConfigGeneric(..)
, DbWalletId
, DbAccountId
, DbAddressId
, DbCoinId
, DbAccTxId
, DbTxId
, DbConfigId
, EntityField(..)
, Unique(..)
, migrateWallet
) where

import Data.Int (Int64)
import Data.Word (Word32, Word64)
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
import Network.Haskoin.Wallet.Types 
import Network.Haskoin.Script
import Network.Haskoin.Protocol 
import Network.Haskoin.Crypto 

-- TODO: We only care about pubkeyhash and not pubkey. Should we do
-- something about it?

share [mkPersist sqlSettings, mkMigrate "migrateWallet"] [persistLowerCase|
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
    value Address
    label String
    index Int
    tree String
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddress value
    UniqueAddressKey account index internal
    deriving Show

DbCoin 
    txid Hash256
    pos Int
    value Word64
    script ScriptOutput
    rdmScript ScriptOutput Maybe
    address Address 
    status CoinStatus
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    CoinOutPoint txid pos
    deriving Show

DbAccTx
    txid Hash256
    recipients [Address]
    value Int64
    account DbAccountId
    partial Bool
    created UTCTime default=CURRENT_TIME
    UniqueAccTx txid account
    deriving Show

DbTx
    txid Hash256
    value Tx
    orphan Bool
    confirmedBy Hash256 Maybe
    confirmedHeight Word32 Maybe
    created UTCTime default=CURRENT_TIME
    UniqueTx txid
    deriving Show

DbConfig
    bestHeight Word32
    version Int
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

