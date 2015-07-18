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
, KeyRingSpentCoin(..)
, KeyRingSpentCoinId
, KeyRingTx(..)
, KeyRingTxId
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

import Data.Word (Word32, Word64)
import Data.Time (UTCTime)
import Data.Text (Text)

import Database.Persist (Entity(..), EntityField, Unique)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH 
    ( share
    , mkPersist
    , sqlSettings
    , mkMigrate
    , persistFileWith
    )

import Network.Haskoin.Wallet.Types 
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Block
import Network.Haskoin.Crypto 
import Network.Haskoin.Node

share [ mkPersist sqlSettings
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

{- JSON Types -}

toJsonKeyRing :: KeyRing -> (Maybe XPrvKey) -> (Maybe Mnemonic) -> JsonKeyRing
toJsonKeyRing keyRing masterM mnemonicM = JsonKeyRing
    { jsonKeyRingName     = keyRingName keyRing
    , jsonKeyRingMaster   = masterM
    , jsonKeyRingMnemonic = mnemonicM
    , jsonKeyRingCreated  = keyRingCreated keyRing
    }

toJsonAccount :: KeyRingAccount -> JsonAccount
toJsonAccount acc = JsonAccount
    { jsonAccountName         = keyRingAccountName acc
    , jsonAccountType         = keyRingAccountType acc
    , jsonAccountDerivation   = keyRingAccountDerivation acc
    , jsonAccountKeys         = keyRingAccountKeys acc
    , jsonAccountGap          = keyRingAccountGap acc
    , jsonAccountCreated      = keyRingAccountCreated acc
    }

toJsonAddr :: KeyRingAddr            -- ^ The address
           -> (Maybe AddressBalance) -- ^ The addresses balance
           -> JsonAddr
toJsonAddr addr balM = JsonAddr
    { jsonAddrAddress        = keyRingAddrAddress addr
    , jsonAddrIndex          = keyRingAddrIndex addr
    , jsonAddrType           = keyRingAddrType addr
    , jsonAddrLabel          = keyRingAddrLabel addr
    , jsonAddrFullDerivation = keyRingAddrFullDerivation addr
    , jsonAddrDerivation     = keyRingAddrDerivation addr
    , jsonAddrRedeem         = keyRingAddrRedeem addr
    , jsonAddrKey            = keyRingAddrKey addr
    , jsonAddrCreated        = keyRingAddrCreated addr
    , jsonAddrBalance        = balM
    }

toJsonTx :: KeyRingTx           -- ^ The transaction
         -> (Maybe BlockHeight) -- ^ The current best block height
         -> JsonTx
toJsonTx tx currentHeightM = JsonTx
    { jsonTxHash            = keyRingTxHash tx
    , jsonTxNosigHash       = keyRingTxNosigHash tx
    , jsonTxType            = keyRingTxType tx
    , jsonTxInValue         = keyRingTxInValue tx
    , jsonTxOutValue        = keyRingTxOutValue tx
    , jsonTxValue           = (fromIntegral $ keyRingTxInValue tx) -
                              (fromIntegral $ keyRingTxOutValue tx)
    , jsonTxInputs          = keyRingTxInputs tx
    , jsonTxOutputs         = keyRingTxOutputs tx
    , jsonTxChange          = keyRingTxChange tx
    , jsonTxTx              = keyRingTxTx tx
    , jsonTxIsCoinbase      = keyRingTxIsCoinbase tx
    , jsonTxConfidence      = keyRingTxConfidence tx
    , jsonTxConfirmedBy     = keyRingTxConfirmedBy tx
    , jsonTxConfirmedHeight = keyRingTxConfirmedHeight tx
    , jsonTxConfirmedDate   = keyRingTxConfirmedDate tx
    , jsonTxCreated         = keyRingTxCreated tx
    , jsonTxConfirmations   = f =<< keyRingTxConfirmedHeight tx
    }
  where
    f confirmedHeight = case currentHeightM of
        Just h -> return $ fromInteger $
            max 0 $ (toInteger h) - (toInteger confirmedHeight) + 1
        _ -> Nothing

toJsonCoin :: KeyRingCoin      -- ^ The coin
           -> (Maybe JsonTx)   -- ^ The coins transaction
           -> (Maybe JsonAddr) -- ^ The coins address
           -> (Maybe JsonTx)   -- ^ The coins spending transaction
           -> JsonCoin
toJsonCoin coin txM addrM spendM = JsonCoin
    { jsonCoinHash       = keyRingCoinHash coin
    , jsonCoinPos        = keyRingCoinPos coin
    , jsonCoinValue      = keyRingCoinValue coin
    , jsonCoinScript     = keyRingCoinScript coin
    , jsonCoinCreated    = keyRingCoinCreated coin
    -- Optional tx
    , jsonCoinTx         = txM
    -- Optional address
    , jsonCoinAddress    = addrM
    -- Optional spending tx
    , jsonCoinSpendingTx = spendM
    }

