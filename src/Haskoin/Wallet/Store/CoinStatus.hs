{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Wallet.Store.CoinStatus
( CoinStatus(..)
, catStatus
) where

import Control.Monad
import Control.Applicative

import Data.Yaml
import qualified Data.Text as T

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- |Spent if a complete transaction spends this coin
-- Reserved if a partial transaction is spending these coins
-- Unspent if the coins are still available
-- The purpose of the Reserved status is to block this coin from being used in
-- subsequent coin selection algorithms. However, Reserved coins can always be
-- spent (set status to Spent) by complete transactions.
data CoinStatus = Spent String | Reserved String | Unspent
    deriving (Show, Read, Eq)
derivePersistField "CoinStatus"

instance ToJSON CoinStatus where
    toJSON (Spent id) = 
        object [ "Status".= T.pack "Spent"
               , "Txid"  .= T.pack id 
               ]
    toJSON (Reserved id) = 
        object [ "Status".= T.pack "Reserved"
               , "Txid"  .= T.pack id 
               ]
    toJSON Unspent = object [ "Status".= T.pack "Unspent" ]

instance FromJSON CoinStatus where
    parseJSON (Object obj) = obj .: "Status" >>= \status -> case status of
        (String "Spent")    -> Spent    <$> obj .: "Txid"
        (String "Reserved") -> Reserved <$> obj .: "Txid"
        (String "Unspent")  -> return Unspent
        _                   -> mzero
    parseJSON _ = mzero

catStatus :: [CoinStatus] -> [String]
catStatus = foldr f []
  where
    f (Spent str) acc    = str:acc
    f (Reserved str) acc = str:acc
    f _ acc              = acc

