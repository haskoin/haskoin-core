{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Root 
( getWalletEntity
, getWallet
, walletList
, newWallet
, initWalletDB
) where

import Control.Monad (liftM, when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (getCurrentTime)
import qualified Data.ByteString as BS

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , PersistStore
    , PersistMonadBackend
    , Entity(..)
    , getBy
    , insert_
    , selectList
    , selectFirst
    , entityVal
    , SelectOpt( Asc )
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

-- | Get a wallet by name
getWallet :: PersistUnique m => String -> m Wallet
getWallet name = liftM (dbWalletValue . entityVal) $ getWalletEntity name

getWalletEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
                => String -> m (Entity (DbWalletGeneric b))
getWalletEntity name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ WalletException $ 
            unwords ["Wallet", name, "does not exist"]

-- | Get a list of all the wallets
walletList :: PersistQuery m => m [Wallet]
walletList = 
    liftM (map f) $ selectList [] [Asc DbWalletCreated]
  where
    f = dbWalletValue . entityVal

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
newWallet :: PersistUnique m
          => String         -- ^ Wallet name
          -> BS.ByteString  -- ^ Secret seed
          -> m Wallet       -- ^ New wallet
newWallet wname seed 
    | BS.null seed = liftIO $ throwIO $ 
        WalletException "The seed is empty"
    | otherwise = do
        prevWallet <- getBy $ UniqueWalletName wname
        when (isJust prevWallet) $ liftIO $ throwIO $ WalletException $
            unwords [ "Wallet", wname, "already exists" ]
        time <- liftIO getCurrentTime
        let master = makeMasterKey seed
            wallet = Wallet wname $ fromJust master
        -- This should never happen
        when (isNothing master) $ liftIO $ throwIO $ WalletException
            "The seed derivation produced an invalid key. Use another seed."
        insert_ $ DbWallet wname wallet Nothing time
        return wallet

initWalletDB :: PersistQuery m => m ()
initWalletDB = do
    prevConfig <- selectFirst [] [Asc DbConfigCreated]
    when (isNothing prevConfig) $ do
        time <- liftIO getCurrentTime
        insert_ $ DbConfig 0 1 time

