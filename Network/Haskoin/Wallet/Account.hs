module Network.Haskoin.Wallet.Account 
( getAccount
, getAccountEntity
, getWalletEntity
, newAccount
, newAccountMultisig
, newAccountRead
, newAccountReadMultisig
, addAccountKeys
, accountList
, accountPrvKey
, isMSAccount
, isReadAccount
) where

import Control.Monad (liftM, unless, when)
import Control.Monad.Reader (ReaderT)
import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.List (nub)
import Data.Maybe (fromJust, isJust, isNothing, catMaybes)
import Data.Time (getCurrentTime)
import qualified Data.Text as T (unpack)

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , Entity(..)
    , getBy
    , replace
    , insert_
    , update
    , selectList
    , entityVal
    , (=.), (==.)
    , SelectOpt( Asc )
    , Key
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

-- | Get an account by name
getAccount :: (MonadIO m, PersistQuery b, PersistUnique b)
           => WalletName          -- ^ Wallet name
           -> AccountName         -- ^ Account name
           -> ReaderT b m Account -- ^ Account
getAccount wallet name = do
    Entity wk _ <- getWalletEntity wallet
    liftM (dbAccountValue . entityVal) $ getAccountEntity wk name

getAccountEntity :: (MonadIO m, PersistUnique b)
                 => Key (DbWalletGeneric b)
                 -> AccountName 
                 -> ReaderT b m (Entity (DbAccountGeneric b))
getAccountEntity wk name = do
    entM <- getBy $ UniqueAccWalletName wk name
    case entM of
        Just ent -> return ent
        Nothing   -> liftIO $ throwIO $ WalletException $ 
            unwords [ "Account", T.unpack name, "does not exist" ]

-- | Returns a list of all accounts in the wallet.
accountList :: (MonadIO m, PersistQuery b, PersistUnique b)
            => WalletName            -- ^ Wallet name
            -> ReaderT b m [Account] -- ^ List of accounts in the wallet
accountList wallet = do
    Entity wk _ <- getWalletEntity wallet
    res <- selectList [DbAccountWallet ==. wk] [Asc DbAccountCreated]
    return $ map (dbAccountValue . entityVal) res
                  
-- | Create a new account from an account name. Accounts are identified by
-- their name and they must be unique. After creating a new account, you may
-- want to call 'setLookAhead'.
newAccount :: (MonadIO m, PersistUnique b, PersistQuery b) 
           => WalletName          -- ^ Wallet name 
           -> AccountName         -- ^ Account name
           -> ReaderT b m Account -- ^ Returns the new account information
newAccount wallet name = do
    -- Check duplicates
    Entity wk w <- getWalletEntity wallet
    prevAcc <- getBy $ UniqueAccWalletName wk name
    when (isJust prevAcc) $ liftIO $ throwIO $ WalletException $
        unwords [ "Account", T.unpack name
                , "already exists in wallet", T.unpack wallet 
                ]

    -- Insert new account
    time <- liftIO getCurrentTime
    let deriv = maybe 0 (+1) $ dbWalletAccIndex w
        (k,i) = head $ accPubKeys (walletMaster $ dbWalletValue w) deriv
        acc   = AccountRegular wallet name i k
    insert_ $ DbAccount wk name acc 0 time

    -- Update account index in the wallet
    update wk [DbWalletAccIndex =. Just i]
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
newAccountMultisig 
    :: (MonadIO m, PersistUnique b, PersistQuery b)
    => WalletName   -- ^ Wallet name
    -> AccountName  -- ^ Account name
    -> Int          -- ^ Required number of keys (m in m of n)
    -> Int          -- ^ Total number of keys (n in m of n)
    -> [XPubKey]    -- ^ Thirdparty public keys
    -> ReaderT b m Account -- ^ Returns the new account information
newAccountMultisig wallet name m n mskeys = do
    -- Check duplicates
    Entity wk w <- getWalletEntity wallet
    prevAcc <- getBy $ UniqueAccWalletName wk name
    when (isJust prevAcc) $ liftIO $ throwIO $ WalletException $
        unwords [ "Account", T.unpack name
                , "already exists in wallet", T.unpack wallet 
                ]

    -- Insert new account
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        WalletException "Invalid multisig parameters"
    unless (length keys < n) $ liftIO $ throwIO $
        WalletException "Too many keys"
    checkOwnKeys wk keys
    let deriv = maybe 0 (+1) $ dbWalletAccIndex w
        (k,i) = head $ accPubKeys (walletMaster $ dbWalletValue w) deriv
        acc   = AccountMultisig wallet name i m n $ getAccPubKey k:keys
    insert_ $ DbAccount wk name acc 0 time

    -- Update account index in the wallet
    update wk [DbWalletAccIndex =. Just i]
    return acc

-- | Create a new read-only account.
newAccountRead :: (MonadIO m, PersistUnique b, PersistQuery b)
               => WalletName  -- ^ Wallet name
               -> AccountName -- ^ Account name
               -> XPubKey     -- ^ Read-only key
               -> ReaderT b m Account -- ^ New account
newAccountRead wallet name key = do
    -- Check duplicates
    Entity wk _ <- getWalletEntity wallet
    prevAcc <- getBy $ UniqueAccWalletName wk name
    when (isJust prevAcc) $ liftIO $ throwIO $ WalletException $
        unwords [ "Account", T.unpack name
                , "already exists in wallet", T.unpack wallet 
                ]

    -- Insert new account
    checkOwnKeys wk [key]
    let accKeyM = loadPubAcc key
    -- TODO: Should we relax the rules for account keys?
    when (isNothing accKeyM) $ liftIO $ throwIO $ WalletException $
        "Invalid account key provided"
    time <- liftIO getCurrentTime
    let acc = AccountRead wallet name $ fromJust accKeyM
    insert_ $ DbAccount wk name acc 0 time
    return acc

newAccountReadMultisig 
    :: (MonadIO m, PersistUnique b, PersistQuery b)
    => WalletName   -- ^ Wallet name
    -> AccountName  -- ^ Account name
    -> Int          -- ^ Required number of keys (m in m of n)
    -> Int          -- ^ Total number of keys (n in m of n)
    -> [XPubKey]    -- ^ Keys
    -> ReaderT b m Account -- ^ Returns the new account information
newAccountReadMultisig wallet name m n mskeys = do
    -- Check duplicates
    Entity wk _ <- getWalletEntity wallet
    prevAcc <- getBy $ UniqueAccWalletName wk name
    when (isJust prevAcc) $ liftIO $ throwIO $ WalletException $
        unwords [ "Account", T.unpack name, "already exists" ]

    -- Insert new account
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        WalletException "Invalid multisig parameters"
    unless (length keys <= n) $ liftIO $ throwIO $
        WalletException "Too many keys"
    checkOwnKeys wk keys
    let acc   = AccountReadMultisig wallet name m n keys
    insert_ $ DbAccount wk name acc 0 time
    return acc

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. In order
-- to prevent usage mistakes, adding a key from your own wallet will fail.
addAccountKeys :: (MonadIO m, PersistUnique b, PersistQuery b)
               => WalletName  -- ^ Wallet name
               -> AccountName -- ^ Account name
               -> [XPubKey]   -- ^ Thirdparty public keys to add
               -> ReaderT b m Account   -- ^ Returns the account information
addAccountKeys wallet name keys 
    | null keys = liftIO $ throwIO $
         WalletException "Thirdparty key list can not be empty"
    | otherwise = do
        Entity wk _     <- getWalletEntity wallet
        Entity ai dbacc <- getAccountEntity wk name
        unless (isMSAccount $ dbAccountValue dbacc) $ liftIO $ throwIO $ 
            WalletException "Can only add keys to a multisig account"
        checkOwnKeys wk keys
        let acc      = dbAccountValue dbacc
            prevKeys = accountKeys acc
        when (length prevKeys == accountTotal acc) $ 
            liftIO $ throwIO $ WalletException 
                "The account is complete and no further keys can be added"
        let newKeys = nub $ prevKeys ++ keys
            newAcc  = acc { accountKeys = newKeys }
        when (length newKeys > accountTotal acc) $
            liftIO $ throwIO $ WalletException
                "Adding too many keys to the account"
        replace ai dbacc{ dbAccountValue = newAcc }
        return newAcc

checkOwnKeys :: (MonadIO m, PersistQuery b, PersistUnique b) 
             => Key (DbWalletGeneric b)
             -> [XPubKey] 
             -> ReaderT b m ()
checkOwnKeys wk keys = do
    accsE <- selectList [DbAccountWallet ==. wk] []
    let accs = map (dbAccountValue . entityVal) accsE
    let myKeysM = map f accs
        myKeys  = catMaybes myKeysM
        overlap = filter (`elem` myKeys) $ map xPubKey keys
    unless (null overlap) $ liftIO $ throwIO $ WalletException 
        "Can not add your own keys to an account"
  where
    f acc | isReadAccount acc = Nothing
          | isMSAccount acc   = Just $ xPubKey $ head $ accountKeys acc
          | otherwise         = Just $ xPubKey $ getAccPubKey $ accountKey acc

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
accountPrvKey :: (MonadIO m, PersistUnique b, PersistQuery b)
              => WalletName            -- ^ Wallet name
              -> AccountName           -- ^ Account name
              -> ReaderT b m AccPrvKey -- ^ Account private key
accountPrvKey wallet name = do
    Entity wk w <- getWalletEntity wallet
    Entity _ a  <- getAccountEntity wk name
    let master = walletMaster (dbWalletValue w)
        deriv  = accountIndex (dbAccountValue a)
    return $ fromJust $ accPrvKey master deriv

{- Helpers -}

isMSAccount :: Account -> Bool
isMSAccount acc = case acc of
    AccountMultisig _ _ _ _ _ _   -> True
    AccountReadMultisig _ _ _ _ _ -> True
    _                             -> False

isReadAccount :: Account -> Bool
isReadAccount acc = case acc of
    AccountRead _ _ _             -> True
    AccountReadMultisig _ _ _ _ _ -> True
    _                             -> False

