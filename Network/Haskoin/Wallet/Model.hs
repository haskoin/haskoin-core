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
, DbTxConflictGeneric(..)
, DbOrphanGeneric(..)
, DbConfirmationGeneric(..)
, DbConfigGeneric(..)
, DbSpentCoinGeneric(..)
, DbWalletId
, DbAccountId
, DbAddressId
, DbCoinId
, DbAccTxId
, DbTxId
, DbTxConflictId
, DbSpentCoinId
, DbConfirmationId
, DbConfigId
, DbOrphanId
, EntityField(..)
, Unique(..)
, migrateWallet
) where

import Data.Int (Int64)
import Data.Word (Word32)
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
import Network.Haskoin.Transaction
import Network.Haskoin.Protocol 
import Network.Haskoin.Crypto 

-- TODO: We only care about pubkeyhash and not pubkey. Should we do
-- something about it?

share [mkPersist sqlSettings, mkMigrate "migrateWallet"] [persistLowerCase|
DbWallet 
    name String
    value Wallet
    accIndex KeyIndex Maybe
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name
    deriving Show

DbAccount 
    name String
    value Account
    extIndex KeyIndex Maybe
    extLookAhead KeyIndex Maybe
    intIndex KeyIndex Maybe
    intLookAhead KeyIndex Maybe
    wallet DbWalletId Maybe
    created UTCTime default=CURRENT_TIME
    UniqueAccName name
    deriving Show

DbAddress 
    value Address
    label String
    index KeyIndex
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddress value
    UniqueAddressKey account index internal
    deriving Show

DbCoin 
    hash TxHash
    pos Int
    value Coin
    address Address 
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    CoinOutPoint hash pos
    deriving Show

DbSpentCoin
    key OutPoint
    tx TxHash
    created UTCTime default=CURRENT_TIME
    deriving Show

DbTxConflict
    fst TxHash
    snd TxHash
    created UTCTime default=CURRENT_TIME
    UniqueConflict fst snd
    deriving Show

DbAccTx
    hash TxHash
    recipients [Address]
    value Int64
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    UniqueAccTx hash account
    deriving Show

DbTx
    hash TxHash
    value Tx
    confidence TxConfidence
    confirmedBy BlockHash Maybe
    confirmedHeight Word32 Maybe
    isCoinbase Bool
    created UTCTime default=CURRENT_TIME
    UniqueTx hash
    deriving Show

DbOrphan
    hash TxHash
    value Tx
    source TxSource
    created UTCTime default=CURRENT_TIME
    UniqueOrphan hash
    deriving Show

DbConfirmation
    tx TxHash
    block BlockHash
    deriving Show

DbConfig
    bestHeight Word32
    version Int
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

