{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.Haskoin.Wallet.Model 
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, DbCoinAccountGeneric(..)
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
, DbCoinAccountId
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
    , mpsGeneric
    , mkPersist
    , sqlSettings
    , mkMigrate
    , persistLowerCase
    )

import Network.Haskoin.Wallet.Types 
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto 

-- TODO: We only care about pubkeyhash and not pubkey. Should we do
-- something about it?

share [mkPersist (sqlSettings { mpsGeneric = True })
      , mkMigrate "migrateWallet"
      ] [persistLowerCase|
DbWallet 
    name String
    value Wallet
    accIndex KeyIndex Maybe
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name

DbAccount 
    wallet DbWalletId
    name String
    value Account
    gap Int
    created UTCTime default=CURRENT_TIME
    UniqueAccWalletName wallet name

DbAddress 
    value Address
    label String
    index KeyIndex
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddressAccount value account
    UniqueAddressKey account index internal

DbCoin 
    hash TxHash
    pos Int
    value Coin
    address Address 
    created UTCTime default=CURRENT_TIME
    CoinOutPoint hash pos

DbCoinAccount
    coin DbCoinId
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    UniqueCoinAccount coin account

DbSpentCoin
    key OutPoint
    tx TxHash
    created UTCTime default=CURRENT_TIME

DbTxConflict
    fst TxHash
    snd TxHash
    created UTCTime default=CURRENT_TIME
    UniqueConflict fst snd

DbAccTx
    hash TxHash
    recipients [Address]
    value Int64
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    UniqueAccTx hash account

DbTx
    hash TxHash
    value Tx
    confidence TxConfidence
    confirmedBy BlockHash Maybe
    confirmedHeight Word32 Maybe
    isCoinbase Bool
    nosigHash TxHash
    created UTCTime default=CURRENT_TIME
    UniqueTx hash

DbOrphan
    hash TxHash
    value Tx
    source TxSource
    created UTCTime default=CURRENT_TIME
    UniqueOrphan hash

DbConfirmation
    tx TxHash
    block BlockHash
    blockTimestamp Word32
    created UTCTime default=CURRENT_TIME
    UniqueConfirmation tx block

DbConfig
    bestHeight Word32
    bestBlockHash BlockHash
    version Int
    created UTCTime default=CURRENT_TIME
|]

