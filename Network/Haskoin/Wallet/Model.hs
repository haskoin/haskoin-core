module Network.Haskoin.Wallet.Model 
( -- Database types
  KeyRing(..)
, KeyRingId
, KeyRingAccount(..)
, KeyRingAccountId
, KeyRingAddr(..)
, KeyRingAddrId
, KeyRingConfig(..)
, KeyRingConfigId
, KeyRingCoin(..)
, KeyRingCoinId
, KeyRingBalance(..)
, KeyRingBalanceId
, KeyRingTx(..)
, KeyRingTxId(..)
, EntityField(..)
, Unique(..)
, migrateWallet

-- JSON conversion
, toJsonKeyRing
, toJsonAccount
, toJsonAddr
, toJsonCoin
, toJsonTx

) where

import Data.Int (Int64)
import Data.Word (Word32, Word64)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), object, (.=))
import Data.Aeson.TH (deriveJSON)

import Database.Persist (Entity(..), EntityField, Unique)
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
import Network.Haskoin.Wallet.Types.DeriveJSON
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Block
import Network.Haskoin.Crypto 
import Network.Haskoin.Node

share [ mkPersist sqlSettings
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

{- JSON instances for database types -}

instance Coin (Entity KeyRingCoin) where
    coinValue (Entity _ coin) = keyRingCoinValue coin

{- JSON Types -}

toJsonKeyRing :: KeyRing -> JsonKeyRing
toJsonKeyRing KeyRing{..} =
    JsonKeyRing{..}
  where
    jsonKeyRingName    = keyRingName
    jsonKeyRingMaster  = keyRingMaster
    jsonKeyRingCreated = keyRingCreated

toJsonAccount :: KeyRingAccount -> JsonAccount
toJsonAccount KeyRingAccount{..} =
    JsonAccount{..}
  where
    jsonAccountName         = keyRingAccountName
    jsonAccountKeyRingName  = keyRingAccountKeyRingName
    jsonAccountType         = keyRingAccountType
    jsonAccountDerivation   = keyRingAccountDerivation
    jsonAccountKeys         = keyRingAccountKeys
    jsonAccountRequiredSigs = keyRingAccountRequiredSigs
    jsonAccountTotalKeys    = keyRingAccountTotalKeys
    jsonAccountGap          = keyRingAccountGap
    jsonAccountCreated      = keyRingAccountCreated

-- TODO: Add the address balance (Maybe Word64)
toJsonAddr :: KeyRingAddr -> JsonAddr
toJsonAddr KeyRingAddr{..} =
    JsonAddr{..}
  where
    jsonAddrKeyRingName       = keyRingAddrKeyRingName
    jsonAddrAccountName       = keyRingAddrAccountName
    jsonAddrAddress           = keyRingAddrAddress
    jsonAddrIndex             = keyRingAddrIndex
    jsonAddrType              = keyRingAddrType
    jsonAddrLabel             = keyRingAddrLabel
    jsonAddrRootDerivation    = keyRingAddrRootDerivation
    jsonAddrDerivation        = keyRingAddrDerivation
    jsonAddrRedeem            = keyRingAddrRedeem
    jsonAddrKey               = keyRingAddrKey
    jsonAddrInBalance         = keyRingAddrInBalance
    jsonAddrOutBalance        = keyRingAddrOutBalance
    jsonAddrInOfflineBalance  = keyRingAddrInOfflineBalance
    jsonAddrOutOfflineBalance = keyRingAddrOutOfflineBalance
    jsonAddrCreated           = keyRingAddrCreated

toJsonCoin :: BlockHeight -> KeyRingCoin -> JsonCoin
toJsonCoin currentHeight KeyRingCoin{..} =
    JsonCoin{..}
  where
    jsonCoinKeyRingName     = keyRingCoinKeyRingName
    jsonCoinAccountName     = keyRingCoinAccountName
    jsonCoinHash            = keyRingCoinHash
    jsonCoinPos             = keyRingCoinPos
    jsonCoinValue           = keyRingCoinValue
    jsonCoinScript          = keyRingCoinScript
    jsonCoinRedeem          = keyRingCoinRedeem
    jsonCoinRootDerivation  = keyRingCoinRootDerivation
    jsonCoinDerivation      = keyRingCoinDerivation
    jsonCoinKey             = keyRingCoinKey
    jsonCoinAddress         = keyRingCoinAddress
    jsonCoinAddressType     = keyRingCoinAddressType
    jsonCoinStatus          = keyRingCoinStatus
    jsonCoinSpentBy         = keyRingCoinSpentBy
    jsonCoinIsCoinbase      = keyRingCoinIsCoinbase
    jsonCoinConfidence      = keyRingCoinConfidence
    jsonCoinConfirmedBy     = keyRingCoinConfirmedBy
    jsonCoinConfirmedHeight = keyRingCoinConfirmedHeight
    jsonCoinConfirmedDate   = keyRingCoinConfirmedDate
    jsonCoinCreated         = keyRingCoinCreated
    jsonCoinConfirmations   = maybe 0 (f . fromIntegral) jsonCoinConfirmedHeight
    f confirmedHeight = 
        max 0 $ (fromIntegral currentHeight) - confirmedHeight + 1

toJsonTx :: BlockHeight -> KeyRingTx -> JsonTx
toJsonTx currentHeight KeyRingTx{..} =
    JsonTx{..}
  where
    jsonTxKeyRingName     = keyRingTxKeyRingName
    jsonTxAccountName     = keyRingTxAccountName
    jsonTxHash            = keyRingTxHash
    jsonTxNosigHash       = keyRingTxNosigHash
    jsonTxType            = keyRingTxType
    jsonTxInValue         = keyRingTxInValue
    jsonTxOutValue        = keyRingTxOutValue
    jsonTxValue           = keyRingTxValue
    jsonTxFrom            = keyRingTxFrom
    jsonTxTo              = keyRingTxTo
    jsonTxChange          = keyRingTxChange
    jsonTxTx              = keyRingTxTx
    jsonTxIsCoinbase      = keyRingTxIsCoinbase
    jsonTxConfidence      = keyRingTxConfidence
    jsonTxConfirmedBy     = keyRingTxConfirmedBy
    jsonTxConfirmedHeight = keyRingTxConfirmedHeight
    jsonTxConfirmedDate   = keyRingTxConfirmedDate
    jsonTxCreated         = keyRingTxCreated
    jsonTxConfirmations   = maybe 0 (f . fromIntegral) jsonTxConfirmedHeight
    f confirmedHeight = 
        max 0 $ (fromIntegral currentHeight) - confirmedHeight + 1

