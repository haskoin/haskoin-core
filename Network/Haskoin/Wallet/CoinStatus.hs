{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.CoinStatus
( CoinStatus(..)
, catStatus
) where

import Control.Monad (mzero)
import Control.Applicative ((<$>))

import Database.Persist.TH (derivePersistField)

import Data.Yaml
import qualified Data.Text as T (pack)

-- | Spent if a complete transaction spends this coin
-- Reserved if a partial transaction is spending these coins
-- Unspent if the coins are still available
-- The purpose of the Reserved status is to block this coin from being used in
-- subsequent coin selection algorithms. However, Reserved coins can always be
-- spent (set status to Spent) by complete transactions.
data CoinStatus = Spent String | Reserved String | Unspent
    deriving (Show, Read, Eq)
derivePersistField "CoinStatus"

instance ToJSON CoinStatus where
    toJSON (Spent tid) = 
        object [ "Status".= T.pack "Spent"
               , "Txid"  .= T.pack tid 
               ]
    toJSON (Reserved tid) = 
        object [ "Status".= T.pack "Reserved"
               , "Txid"  .= T.pack tid 
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

