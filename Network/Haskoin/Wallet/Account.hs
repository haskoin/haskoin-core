{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Account 
( AccountName
, getAccount
, getAccountEntity
, getWalletEntity
, newAccount
, newMSAccount
, addAccountKeys
, accountList
, accountPrvKey
, isMSAccount
) where

import Control.Monad (liftM, unless, when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)

import Data.List (nub)
import Data.Maybe (fromJust, isJust )
import Data.Time (getCurrentTime)

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , getBy
    , get
    , replace
    , insert_
    , update
    , count
    , selectList
    , entityVal
    , (=.), (==.)
    , SelectOpt( Asc )
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

type AccountName = String

getAccountEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
             => AccountName -> m (Entity (DbAccountGeneric b))
getAccountEntity name = do
    entM <- getBy $ UniqueAccName name
    case entM of
        Just ent -> return ent
        Nothing   -> liftIO $ throwIO $ InvalidAccountException $ 
            unwords ["Account", name, "does not exist"]
                  
-- | Get an account by name
getAccount :: (PersistUnique m, PersistMonadBackend m ~ b)
           => AccountName             -- ^ Account name
           -> m (DbAccountGeneric b)  -- ^ Account
getAccount name = liftM entityVal $ getAccountEntity name

isMSAccount :: DbAccountGeneric b -> Bool
isMSAccount acc = (isJust $ dbAccountMsRequired acc) && 
              (isJust $ dbAccountMsTotal acc) 

-- | Create a new account from an account name. Accounts are identified by
-- their name and they must be unique. After creating a new account, you may
-- want to call 'setLookAhead'.
newAccount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b) 
             => String                 -- ^ Wallet name 
             -> String                 -- ^ Account name
             -> m (DbAccountGeneric b) -- ^ Returns the new account information
newAccount wname name = do
    prevAcc <- getBy $ UniqueAccName name
    when (isJust prevAcc) $ liftIO $ throwIO $ AccountSetupException $
        unwords [ "Account", name, "already exists" ]
    time <- liftIO getCurrentTime
    (Entity wk w) <- getWalletEntity wname
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys (dbWalletMaster w) deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          k
                          (-1) (-1) (-1) (-1)
                          Nothing Nothing [] wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    return acc

-- | Create a new multisignature account. The thirdparty keys can be provided
-- now or later using the 'cmdAddKeys' command. The number of thirdparty keys
-- can not exceed n-1 as your own account key will be used as well in the
-- multisignature scheme. If less than n-1 keys are provided, the account will
-- be in a pending state and no addresses can be generated.
--
-- In order to prevent usage mistakes, you can not create a multisignature 
-- account with other keys from your own wallet. Once the account is set up
-- with all the keys, you may want to call 'setLookAhead'.
newMSAccount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
             => String    -- ^ Wallet name
             -> String    -- ^ Account name
             -> Int       -- ^ Required number of keys (m in m of n)
             -> Int       -- ^ Total number of keys (n in m of n)
             -> [XPubKey] -- ^ Thirdparty public keys
             -> m (DbAccountGeneric b) -- ^ Returns the new account information
newMSAccount wname name m n mskeys = do
    prevAcc <- getBy $ UniqueAccName name
    when (isJust prevAcc) $ liftIO $ throwIO $ AccountSetupException $
        unwords [ "Account", name, "already exists" ]
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        AccountSetupException "Invalid multisig parameters"
    unless (length keys < n) $ liftIO $ throwIO $
        AccountSetupException "Too many keys"
    checkOwnKeys keys
    (Entity wk w) <- getWalletEntity wname
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys (dbWalletMaster w) deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          k
                          (-1) (-1) (-1) (-1) 
                          (Just m) (Just n) 
                          keys
                          wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    return acc

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. In order
-- to prevent usage mistakes, adding a key from your own wallet will fail.
addAccountKeys :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
               => AccountName -- ^ Account name
               -> [XPubKey]   -- ^ Thirdparty public keys to add
               -> m (DbAccountGeneric b) -- ^ Returns the account information
addAccountKeys name keys 
    | null keys = liftIO $ throwIO $
         AccountSetupException "Thirdparty key list can not be empty"
    | otherwise = do
        (Entity ai acc) <- getAccountEntity name
        unless (isMSAccount acc) $ liftIO $ throwIO $ AccountSetupException 
            "Can only add keys to a multisig account"
        checkOwnKeys keys
        let prevKeys = dbAccountMsKeys acc
        when (length prevKeys == (fromJust $ dbAccountMsTotal acc) - 1) $ 
            liftIO $ throwIO $ AccountSetupException 
                "The account is complete and no further keys can be added"
        let newKeys = nub $ prevKeys ++ keys
            newAcc  = acc { dbAccountMsKeys = newKeys }
        unless (length newKeys < (fromJust $ dbAccountMsTotal acc)) $
            liftIO $ throwIO $ AccountSetupException
                "Adding too many keys to the account"
        replace ai newAcc
        return newAcc

checkOwnKeys :: PersistQuery m => [XPubKey] -> m ()
checkOwnKeys keys = do
    -- TODO: Match PubKey instead of XPubKey to avoid playing 
    -- with height or other values
    exists <- mapM (\x -> count [DbAccountKey ==. AccPubKey x]) keys
    unless (sum exists == 0) $ liftIO $ throwIO $ AccountSetupException 
        "Can not add your own keys to a multisig account"

-- | Returns a list of all accounts in the wallet.
accountList :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
            => m [DbAccountGeneric b] -- ^ List of accounts in the wallet
accountList = liftM (map entityVal) $ selectList [] [Asc DbAccountCreated]

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
accountPrvKey :: (PersistUnique m, PersistMonadBackend m ~ b)
              => AccountName -- ^ Account name
              -> m AccPrvKey -- ^ Account private key
accountPrvKey name = do
    acc <- getAccount name
    w   <- liftM fromJust (get $ dbAccountWallet acc)
    let master = dbWalletMaster w
        deriv  = fromIntegral $ dbAccountIndex acc
    return $ fromJust $ accPrvKey master deriv

