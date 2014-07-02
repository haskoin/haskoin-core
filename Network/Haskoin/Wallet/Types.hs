{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( CoinStatus(..)
, WalletType(..)
, catStatus
, WalletException(..)
) where

import Control.Exception (Exception)

import qualified Data.Text as T
import Data.Typeable (Typeable)

import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue 
    )
import Database.Persist.Types (PersistValue (PersistByteString))
import Database.Persist.TH (derivePersistField)
import Database.Persist.Sql (PersistFieldSql, SqlType (SqlBlob), sqlType)

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util

data WalletException 
    = WalletException String
    | AccountSetupException String
    | InvalidAccountException String
    | InvalidPageException String
    | AddressGenerationException String
    | InvalidAddressException String
    | InvalidTransactionException String
    | DoubleSpendException String
    | CoinSelectionException String
    | TransactionBuildingException String
    | TransactionSigningException String
    | ParsingException String
    | InvalidCommandException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

-- | Spent if a complete transaction spends this coin
-- Reserved if a partial transaction is spending these coins
-- Unspent if the coins are still available
-- The purpose of the Reserved status is to block this coin from being used in
-- subsequent coin selection algorithms. However, Reserved coins can always be
-- spent (set status to Spent) by complete transactions.
data CoinStatus = Spent TxHash | Reserved TxHash | Unspent
    deriving (Show, Read, Eq)

catStatus :: [CoinStatus] -> [TxHash]
catStatus = foldr f []
  where
    f (Spent h) acc    = h:acc
    f (Reserved h) acc = h:acc
    f _ acc            = acc

data WalletType = WalletFull | WalletRead
    deriving (Show, Read, Eq)

{- Instances for PersistField and PersistFieldSql -}

derivePersistField "CoinStatus"
derivePersistField "WalletType"
derivePersistField "MasterKey"
derivePersistField "AccPubKey"
derivePersistField "XPubKey"
derivePersistField "Coin"
derivePersistField "OutPoint"
derivePersistField "TxHash"
derivePersistField "BlockHash"
derivePersistField "ScriptOutput"
derivePersistField "Address"

instance PersistField Tx where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = case txE of
        Right tx -> Right tx
        Left str -> Left $ T.pack str
      where
        txE = decodeToEither bs
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob


