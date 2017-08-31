module Network.Haskoin.Wallet.Model
( -- Database types
  Account(..)
, AccountId
, WalletAddr(..)
, WalletAddrId
, WalletState(..)
, WalletStateId
, WalletCoin(..)
, WalletCoinId
, SpentCoin(..)
, SpentCoinId
, WalletTx(..)
, WalletTxId
, EntityField(..)
, Unique(..)
, migrateWallet

-- JSON conversion
, toJsonAccount
, toJsonAddr
, toJsonCoin
, toJsonTx

) where

import Data.Word (Word32, Word64)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.String.Conversions (cs)

import Database.Persist (EntityField, Unique)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH
    ( share
    , mkPersist
    , sqlSettings
    , mkMigrate
    , persistFileWith
    )

import Network.Haskoin.Wallet.Types
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Network
import Network.Haskoin.Node.HeaderTree

share [ mkPersist sqlSettings
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

{- JSON Types -}

toJsonAccount :: Maybe Mnemonic -> Account -> JsonAccount
toJsonAccount msM acc = JsonAccount
    { jsonAccountName         = accountName acc
    , jsonAccountType         = accountType acc
    , jsonAccountMnemonic     = fmap cs msM
    , jsonAccountMaster       = accountMaster acc
    , jsonAccountKeys         = accountKeys acc
    , jsonAccountGap          = accountGap acc
    , jsonAccountCreated      = accountCreated acc
    }

toJsonAddr :: WalletAddr
           -> Maybe BalanceInfo
           -> JsonAddr
toJsonAddr addr balM = JsonAddr
    { jsonAddrAddress        = walletAddrAddress addr
    , jsonAddrIndex          = walletAddrIndex addr
    , jsonAddrType           = walletAddrType addr
    , jsonAddrLabel          = walletAddrLabel addr
    , jsonAddrRedeem         = walletAddrRedeem addr
    , jsonAddrKey            = walletAddrKey addr
    , jsonAddrCreated        = walletAddrCreated addr
    , jsonAddrBalance        = balM
    }

toJsonTx :: AccountName
         -> Maybe (BlockHash, BlockHeight) -- ^ Current best block
         -> WalletTx
         -> JsonTx
toJsonTx acc bbM tx = JsonTx
    { jsonTxHash            = walletTxHash tx
    , jsonTxNosigHash       = walletTxNosigHash tx
    , jsonTxType            = walletTxType tx
    , jsonTxInValue         = walletTxInValue tx
    , jsonTxOutValue        = walletTxOutValue tx
    , jsonTxValue           = fromIntegral (walletTxInValue tx) -
                              fromIntegral (walletTxOutValue tx)
    , jsonTxInputs          = walletTxInputs tx
    , jsonTxOutputs         = walletTxOutputs tx
    , jsonTxChange          = walletTxChange tx
    , jsonTxTx              = walletTxTx tx
    , jsonTxIsCoinbase      = walletTxIsCoinbase tx
    , jsonTxConfidence      = walletTxConfidence tx
    , jsonTxConfirmedBy     = walletTxConfirmedBy tx
    , jsonTxConfirmedHeight = walletTxConfirmedHeight tx
    , jsonTxConfirmedDate   = walletTxConfirmedDate tx
    , jsonTxCreated         = walletTxCreated tx
    , jsonTxAccount         = acc
    , jsonTxConfirmations   = f =<< walletTxConfirmedHeight tx
    , jsonTxBestBlock       = fst <$> bbM
    , jsonTxBestBlockHeight = snd <$> bbM
    }
  where
    f confirmedHeight = case bbM of
        Just (_, h) -> return $ fromInteger $
            max 0 $ toInteger h - toInteger confirmedHeight + 1
        _ -> Nothing

toJsonCoin :: WalletCoin
           -> Maybe JsonTx   -- ^ Coin’s transaction
           -> Maybe JsonAddr -- ^ Coin’s address
           -> Maybe JsonTx   -- ^ Coin’s spending transaction
           -> JsonCoin
toJsonCoin coin txM addrM spendM = JsonCoin
    { jsonCoinHash       = walletCoinHash coin
    , jsonCoinPos        = walletCoinPos coin
    , jsonCoinValue      = walletCoinValue coin
    , jsonCoinScript     = walletCoinScript coin
    , jsonCoinCreated    = walletCoinCreated coin
    -- Optional tx
    , jsonCoinTx         = txM
    -- Optional address
    , jsonCoinAddress    = addrM
    -- Optional spending tx
    , jsonCoinSpendingTx = spendM
    }

