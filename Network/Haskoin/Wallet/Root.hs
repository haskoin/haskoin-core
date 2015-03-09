module Network.Haskoin.Wallet.Root 
( getWalletEntity
, getWallet
, walletList
, newWallet
, initWalletDB
, resetRescan
, filterLen
) where

import Control.Monad (liftM, when)
import Control.Monad.Reader (ReaderT)
import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (getCurrentTime)
import qualified Data.Text as T (unpack)
import qualified Data.ByteString as BS

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , PersistStore
    , Entity(..)
    , Filter(..)
    , getBy
    , insert_
    , selectList
    , selectFirst
    , entityVal
    , SelectOpt( Asc )
    , updateWhere
    , deleteWhere
    , (=.)
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Node

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

-- | Get a wallet by name
getWallet :: (MonadIO m, PersistStore b, PersistUnique b)
          => WalletName         -- ^ Wallet name
          -> ReaderT b m Wallet -- ^ Wallet
getWallet name = liftM (dbWalletValue . entityVal) $ getWalletEntity name

getWalletEntity :: (MonadIO m, PersistStore b, PersistUnique b)
                => WalletName 
                -> ReaderT b m (Entity (DbWalletGeneric b))
getWalletEntity name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ WalletException $ 
            unwords ["Wallet", T.unpack name, "does not exist"]

-- | Get a list of all the wallets
walletList :: (MonadIO m, PersistQuery b) => ReaderT b m [Wallet]
walletList = 
    liftM (map f) $ selectList [] [Asc DbWalletCreated]
  where
    f = dbWalletValue . entityVal

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
newWallet :: (MonadIO m, PersistQuery b, PersistUnique b)
          => WalletName         -- ^ Wallet name
          -> BS.ByteString      -- ^ Secret seed
          -> ReaderT b m Wallet -- ^ New wallet
newWallet wname seed 
    | BS.null seed = liftIO $ throwIO $ 
        WalletException "The seed is empty"
    | otherwise = do
        prevWallet <- getBy $ UniqueWalletName wname
        when (isJust prevWallet) $ liftIO $ throwIO $ WalletException $
            unwords [ "Wallet", T.unpack wname, "already exists" ]
        time <- liftIO getCurrentTime
        let master = makeMasterKey seed
            wallet = Wallet wname $ fromJust master
        -- This should never happen
        when (isNothing master) $ liftIO $ throwIO $ WalletException
            "The seed derivation produced an invalid key. Use another seed."
        insert_ $ DbWallet wname wallet Nothing time
        return wallet

initWalletDB :: (MonadIO m, PersistQuery b) => Double -> ReaderT b m ()
initWalletDB fpRate = do
    prevConfig <- selectFirst [] [Asc DbConfigCreated]
    when (isNothing prevConfig) $ do
        time <- liftIO getCurrentTime
        -- Create an initial bloom filter
        -- TODO: Compute a random nonce 
        let bloom = bloomCreate (filterLen 0) fpRate 0 BloomUpdateNone
        insert_ $ DbConfig 0 (headerHash genesisHeader) bloom 0 fpRate 1 time

-- Remove transaction related data from the wallet
resetRescan :: (MonadIO m, PersistQuery b) => ReaderT b m ()
resetRescan = do
    -- Delete all coins/account relations
    deleteWhere ([] :: PersistQuery b => [Filter (DbCoinAccountGeneric b)])
    -- Delete all coins
    deleteWhere ([] :: PersistQuery b => [Filter (DbCoinGeneric b)])
    -- Delete all coins spent relations
    deleteWhere ([] :: PersistQuery b => [Filter (DbSpentCoinGeneric b)])
    -- Delete all conflict relations
    deleteWhere ([] :: PersistQuery b => [Filter (DbTxConflictGeneric b)])
    -- Delete all account transactions
    deleteWhere ([] :: PersistQuery b => [Filter (DbAccTxGeneric b)])
    -- Delete all transactions
    deleteWhere ([] :: PersistQuery b => [Filter (DbTxGeneric b)])
    -- Delete all orphan transactions
    deleteWhere ([] :: PersistQuery b => [Filter (DbOrphanGeneric b)])
    -- Reset best block information
    updateWhere [] [ DbConfigBestHeight =. 0 
                   , DbConfigBestBlock =. headerHash genesisHeader
                   ]

-- Compute the size of a filter given a number of elements. Scale
-- the filter length by powers of 2.
filterLen :: Int -> Int
filterLen = round . pow2 . ceiling . log2
  where
    pow2 x = (2 :: Double) ** (fromInteger x)
    log2 x = log (fromIntegral x) / log (2 :: Double)

