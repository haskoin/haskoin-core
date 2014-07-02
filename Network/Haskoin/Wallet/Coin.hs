{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Coin 
( balance
, unspentCoins
) where

import Data.Word (Word64)
import Database.Persist
    ( PersistQuery
    , PersistUnique
    , PersistMonadBackend
    , Entity (Entity)
    , SelectOpt (Asc)
    , (==.)
    , entityVal
    , selectList
    )

import Network.Haskoin.Transaction
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account

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
    return $ sum $ map (coinValue . dbCoinValue . entityVal) coins

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


