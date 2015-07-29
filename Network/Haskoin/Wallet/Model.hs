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

import Control.DeepSeq (NFData(..))

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
import Network.Haskoin.Node.HeaderTree

share [ mkPersist sqlSettings
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

instance NFData e => NFData (Entity e) where
    rnf (Entity k e) = k `seq` rnf e

instance NFData KeyRing where
    rnf KeyRing{..} =
        rnf keyRingName `seq`
        rnf keyRingMaster `seq`
        rnf keyRingCreated

instance NFData KeyRingAccount where
    rnf KeyRingAccount{..} =
        keyRingAccountKeyRing `seq`
        rnf keyRingAccountName `seq`
        rnf keyRingAccountType `seq`
        rnf keyRingAccountDerivation `seq`
        rnf keyRingAccountKeys `seq`
        rnf keyRingAccountGap `seq`
        rnf keyRingAccountCreated

instance NFData KeyRingAddr where
    rnf KeyRingAddr{..} =
        keyRingAddrAccount `seq`
        rnf keyRingAddrAddress `seq`
        rnf keyRingAddrIndex `seq`
        rnf keyRingAddrType `seq`
        rnf keyRingAddrLabel `seq`
        rnf keyRingAddrFullDerivation `seq`
        rnf keyRingAddrDerivation `seq`
        rnf keyRingAddrRedeem `seq`
        rnf keyRingAddrKey `seq`
        rnf keyRingAddrCreated

instance NFData KeyRingTx where
    rnf KeyRingTx{..} =
        keyRingTxAccount `seq`
        rnf keyRingTxHash `seq`
        rnf keyRingTxNosigHash `seq`
        rnf keyRingTxType `seq`
        rnf keyRingTxInValue `seq`
        rnf keyRingTxOutValue `seq`
        rnf keyRingTxInputs `seq`
        rnf keyRingTxOutputs `seq`
        rnf keyRingTxChange `seq`
        rnf keyRingTxTx `seq`
        rnf keyRingTxIsCoinbase `seq`
        rnf keyRingTxConfidence `seq`
        rnf keyRingTxConfirmedBy `seq`
        rnf keyRingTxConfirmedHeight `seq`
        rnf keyRingTxConfirmedDate `seq`
        rnf keyRingTxCreated

instance NFData KeyRingCoin where
    rnf KeyRingCoin{..} =
        keyRingCoinAccount `seq`
        rnf keyRingCoinHash `seq`
        rnf keyRingCoinPos `seq`
        keyRingCoinTx `seq`
        keyRingCoinAddr `seq`
        rnf keyRingCoinValue `seq`
        rnf keyRingCoinScript `seq`
        rnf keyRingCoinCreated

instance NFData KeyRingSpentCoin where
    rnf KeyRingSpentCoin{..} =
        keyRingSpentCoinAccount `seq`
        rnf keyRingSpentCoinHash `seq`
        rnf keyRingSpentCoinPos `seq`
        keyRingSpentCoinSpendingTx `seq`
        rnf keyRingSpentCoinCreated

instance NFData KeyRingConfig where
    rnf KeyRingConfig{..} =
        rnf keyRingConfigHeight `seq`
        rnf keyRingConfigBlock `seq`
        rnf keyRingConfigBloomFilter `seq`
        rnf keyRingConfigBloomElems `seq`
        rnf keyRingConfigBloomFp `seq`
        rnf keyRingConfigVersion `seq`
        rnf keyRingConfigCreated

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

toJsonAddr :: KeyRingAddr         -- ^ The address
           -> (Maybe BalanceInfo) -- ^ The addresses balance
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

