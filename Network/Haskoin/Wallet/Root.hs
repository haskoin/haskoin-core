{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Root 
( isFullWallet
, getWalletEntity
, getWallet
, walletList
, newWalletMnemo
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
import Network.Haskoin.Util

isFullWallet :: Wallet -> Bool
isFullWallet (WalletFull _ _) = True
isFullWallet _                = False

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

-- | Initialize a wallet from an optional mnemonic seed and a passphrase,
-- which could be blank.
newWalletMnemo :: PersistUnique m
               => String
               -- ^ Wallet name
               -> String
               -- ^ Passphrase to protect mnemonic.
               -> Maybe String
               -- ^ Mnemonic sentence to initialize wallet with.
               -- Use entropy from /dev/random otherwise.
               -> m Mnemonic
               -- ^ Mnemonic sentence used to initialize wallet.
               -- Impossible to retrieve in the future.

newWalletMnemo wname p (Just s) = do
    let seedE = mnemonicToSeed p s
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        WalletException $ fromLeft seedE
    _ <- newWallet wname seed
    return s

newWalletMnemo wname p Nothing = do
    -- Check this to avoid an unnecessary call to /dev/random if it fails
    prevWallet <- getBy $ UniqueWalletName wname
    when (isJust prevWallet) $ liftIO $ throwIO $ WalletException $
        unwords [ "Wallet", wname, "already exists" ]
    ent <- liftIO $ devRandom 16
    let msSeedE = do
            m <- toMnemonic ent
            s <- mnemonicToSeed p m
            return (m, s)
    when (isLeft msSeedE) $ liftIO $ throwIO $
        WalletException $ fromLeft msSeedE
    let (ms, seed) = fromRight msSeedE
    _ <- newWallet wname seed
    return ms

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
newWallet :: PersistUnique m
          => String         -- ^ Wallet name
          -> BS.ByteString  -- ^ Secret seed
          -> m ()             
newWallet wname seed 
    | BS.null seed = liftIO $ throwIO $ 
        WalletException "The seed is empty"
    | otherwise = do
        prevWallet <- getBy $ UniqueWalletName wname
        when (isJust prevWallet) $ liftIO $ throwIO $ WalletException $
            unwords [ "Wallet", wname, "already exists" ]
        time <- liftIO getCurrentTime
        let master = makeMasterKey seed
            wallet = WalletFull wname $ fromJust master
        -- This should never happen
        when (isNothing master) $ liftIO $ throwIO $ WalletException
            "The seed derivation produced an invalid key. Use another seed."
        insert_ $ DbWallet wname wallet (-1) time

initWalletDB :: PersistQuery m => m ()
initWalletDB = do
    prevConfig <- selectFirst [] [Asc DbConfigCreated]
    when (isNothing prevConfig) $ do
        time <- liftIO getCurrentTime
        insert_ $ DbConfig 0 1 time

