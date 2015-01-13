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
import Data.Text (Text)
import Database.Persist (EntityField, Unique)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH 
    ( share
    , mkPersist
    , sqlSettings
    , mkMigrate
    , mpsGeneric
    , persistFileWith
    )

import Network.Haskoin.Wallet.Types 
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto 

share [ mkPersist sqlSettings{ mpsGeneric = True }
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

