{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Root 
( getWalletEntity
, getWallet
, newWalletMnemo
, newWallet
, initWalletDB
) where

import Control.Monad (liftM, unless, when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)

import Data.List (nub)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (getCurrentTime)
import qualified Data.Text as T (pack)
import qualified Data.ByteString as BS

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , PersistStore
    , PersistMonadBackend
    , PersistEntityBackend
    , PersistEntity
    , Entity(..)
    , getBy
    , get
    , replace
    , insert_
    , update
    , count
    , selectList
    , selectFirst
    , insertMany
    , entityVal
    , (=.), (==.), (<=.), (>.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Util

type AccountName = String

getWallet :: (PersistUnique m, PersistMonadBackend m ~ b)
          => String -> m (DbWalletGeneric b)
getWallet name = liftM entityVal $ getWalletEntity name

getWalletEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> m (Entity (DbWalletGeneric b))
getWalletEntity name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ WalletException $ 
            unwords ["Wallet", name, "is not initialized"]

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
    let seedE = mnemonicToSeed english (T.pack p) $ T.pack s
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        WalletException $ fromLeft seedE
    _ <- newWallet wname seed
    return $ T.pack s

newWalletMnemo wname p Nothing = do
    ent <- liftIO $ devRandom 16
    let msSeedE = do
            m <- toMnemonic english ent
            s <- mnemonicToSeed english (T.pack p) m
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
        -- This should never happen
        when (isNothing master) $ liftIO $ throwIO $ WalletException
            "The seed derivation produced an invalid key. Use another seed."
        insert_ $ DbWallet wname WalletFull (fromJust master) (-1) time

initWalletDB :: PersistQuery m => m ()
initWalletDB = do
    prevConfig <- selectFirst [] [Asc DbConfigCreated]
    when (isNothing prevConfig) $ do
        time <- liftIO getCurrentTime
        insert_ $ DbConfig 0 1 time

