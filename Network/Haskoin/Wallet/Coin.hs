{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Coin 
( balance
, unspentCoins
, toCoin
) where

import Control.Applicative ((<$>))

import Data.Aeson (Value, (.=), object)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word64)
import Database.Persist
    ( PersistQuery
    , PersistUnique
    , PersistMonadBackend
    , Entity (Entity)
    , KeyBackend
    , SelectOpt (Asc)
    , (==.)
    , entityVal
    , selectList
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script
import Network.Haskoin.Util
import Network.Haskoin.Transaction
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account

toCoin :: DbCoinGeneric b -> Coin
toCoin c = Coin 
    (TxOut (fromIntegral $ dbCoinValue c) (encodeOutputBS $ dbCoinScript c))
    (OutPoint (dbCoinHash c) (fromIntegral $ dbCoinPos c))
    (encodeOutput <$> dbCoinRdmScript c)

-- | Returns the balance of an account.
balance :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
        => AccountName -- ^ Account name
        -> m Word64    -- ^ Account balance
balance name = do
    (Entity ai _) <- getAccountEntity name
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinStatus  ==. Unspent
        ] []
    return $ sum $ map (dbCoinValue . entityVal) coins

unspentCoins :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b) 
             => AccountName         -- ^ Account name
             -> m [DbCoinGeneric b] -- ^ List of unspent coins
unspentCoins name = do
    (Entity ai _) <- getAccountEntity name
    coins <- selectList 
        [ DbCoinAccount ==. ai
        , DbCoinStatus  ==. Unspent
        ] [Asc DbCoinCreated]
    return $ map entityVal coins


