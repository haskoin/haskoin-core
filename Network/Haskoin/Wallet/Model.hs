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
, DbTokenGeneric(..)
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
, DbTokenId
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
    name String maxlen=200
    value Wallet
    accIndex KeyIndex Maybe
    created UTCTime
    UniqueWalletName name

DbAccount 
    wallet DbWalletId
    name String maxlen=200
    value Account
    gap Int
    created UTCTime
    UniqueAccWalletName wallet name

DbAddress 
    value Address maxlen=64
    label String
    index KeyIndex
    account DbAccountId
    internal Bool
    created UTCTime
    UniqueAddressAccount value account
    UniqueAddressKey account index internal

DbCoin 
    hash TxHash maxlen=200
    pos Int
    value Coin
    address Address maxlen=64
    created UTCTime
    CoinOutPoint hash pos

DbCoinAccount
    coin DbCoinId
    account DbAccountId
    created UTCTime
    UniqueCoinAccount coin account

DbSpentCoin
    key OutPoint maxlen=200
    tx TxHash maxlen=200
    created UTCTime

DbTxConflict
    fst TxHash maxlen=200
    snd TxHash maxlen=200
    created UTCTime
    UniqueConflict fst snd

DbAccTx
    hash TxHash maxlen=200
    recipients [Address]
    value Int64
    account DbAccountId
    created UTCTime
    UniqueAccTx hash account

DbTx
    hash TxHash maxlen=200
    value Tx
    confidence TxConfidence maxlen=16
    confirmedBy BlockHash Maybe maxlen=200
    confirmedHeight Word32 Maybe
    isCoinbase Bool
    nosigHash TxHash maxlen=200
    created UTCTime
    UniqueTx hash

DbOrphan
    hash TxHash maxlen=200
    value Tx
    source TxSource maxlen=16
    created UTCTime
    UniqueOrphan hash

DbConfirmation
    tx TxHash maxlen=200
    block BlockHash maxlen=200
    blockTimestamp Word32
    created UTCTime
    UniqueConfirmation tx block

DbConfig
    bestHeight Word32
    bestBlockHash BlockHash maxlen=200
    version Int
    created UTCTime

DbToken
    ident String maxlen=200
    secret String maxlen=200
    nonce Int
    expires UTCTime Maybe
    created UTCTime
    UniqueTokenIdent ident
|]

