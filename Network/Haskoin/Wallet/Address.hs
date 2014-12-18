{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Address 
( getAddress
, addressList
, addressPage
, unusedAddrs
, unlabeledAddrs
, unusedAddrsGeneric
, newAddrs
, newAddrsGeneric
, setAddrLabel
, addressPrvKey
, addLookAhead
, adjustLookAhead
, addrPubKey
, toPaymentAddr
) where

import Control.Monad (liftM, when)
import Control.Monad.Reader (ReaderT)
import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Maybe (fromJust, isNothing)
import Data.Time (getCurrentTime)

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , Entity(..)
    , getBy
    , get
    , replace
    , count
    , selectList
    , selectFirst
    , insertMany
    , entityVal
    , (==.), (>.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    , Key
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account

toPaymentAddr :: DbAddressGeneric b -> PaymentAddress
toPaymentAddr x = PaymentAddress (dbAddressValue x) 
                                 (dbAddressLabel x) 
                                 (dbAddressIndex x)

-- Get an address by account name and key
getAddress :: (MonadIO m, PersistUnique b, PersistQuery b)
           => WalletName  -- ^ Wallet name
           -> AccountName -- ^ Account name
           -> KeyIndex    -- ^ Derivation index (key)
           -> Bool        -- ^ Internal address
           -> ReaderT b m PaymentAddress  -- ^ Payment address
getAddress wallet name key internal = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    liftM (toPaymentAddr . entityVal) $ getAddressEntity ai key internal

getAddressEntity :: (MonadIO m, PersistUnique b, PersistQuery b)
                 => Key (DbAccountGeneric b)
                 -> KeyIndex 
                 -> Bool 
                 -> ReaderT b m (Entity (DbAddressGeneric b))
getAddressEntity ai key internal = do
    entM <- getBy $ UniqueAddressKey ai key internal
    when (isNothing entM) $ liftIO $ 
        throwIO $ WalletException "The address has not been generated yet"
    return $ fromJust entM

addrPubKey :: (MonadIO m, PersistUnique b, PersistQuery b)
           => DbAddressGeneric b
           -> ReaderT b m (Maybe PubKeyC)
addrPubKey add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    if isMSAccount (dbAccountValue acc) then return Nothing else do
        let deriv    = dbAddressIndex add
            internal = dbAddressInternal add
            accKey   = accountKey $ dbAccountValue acc
            f        = if internal then intPubKey else extPubKey
            pk       = fromJust $ f accKey deriv
        return $ Just $ xPubKey $ getAddrPubKey pk

-- | Returns all addresses for an account.
addressList :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Bool        -- ^ Internal address
            -> ReaderT b m [PaymentAddress] -- ^ Payment addresses
addressList wallet name internal = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    addrs <- selectList [ DbAddressAccount ==. ai 
                        , DbAddressInternal ==. internal
                        ]
                        [ Asc DbAddressId ]
    return $ map (toPaymentAddr . entityVal) addrs

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
addressPage :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Int         -- ^ Requested page number
            -> Int         -- ^ Number of addresses per page
            -> Bool        -- ^ Internal address
            -> ReaderT b m ([PaymentAddress], Int) 
                -- ^ (Requested page, Highest page number)
addressPage wallet name pageNum resPerPage internal
    | pageNum < 0 = liftIO $ throwIO $ WalletException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ WalletException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = do
        Entity wk _ <- getWalletEntity wallet
        Entity ai _ <- getAccountEntity wk name
        addrCount   <- addressCount ai
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise    = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ WalletException $ 
            unwords ["The page number", show pageNum, "is too high"]
        addrs <- selectList 
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. internal
                ] 
                [ Asc DbAddressId
                , LimitTo resPerPage
                , OffsetBy $ (page - 1) * resPerPage
                ]
        return $ ((map (toPaymentAddr . entityVal) addrs), maxPage)
  where
    addressCount ai = count [ DbAddressAccount ==. ai 
                            , DbAddressInternal ==. internal
                            ]

-- | Get list of unused addresses: those in the account gap.
unusedAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Bool        -- ^ Internal
            -> ReaderT b m [PaymentAddress] -- ^ Unused addresses
unusedAddrs wallet name internal = do
    Entity wk _ <- getWalletEntity wallet
    accE        <- getAccountEntity wk name
    liftM (map toPaymentAddr) $ unusedAddrsGeneric accE internal

-- | Get unused and unlabeled addresses.
unlabeledAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
              => WalletName  -- ^ Account name
              -> AccountName -- ^ Account name
              -> ReaderT b m [PaymentAddress] 
                -- ^ Unlabeled and unused addresses
unlabeledAddrs wallet name = 
    liftM f $ unusedAddrs wallet name False
  where
    f = dropWhile (not . null . addressLabel)

-- | Get a list of unused addresses (generic version)
unusedAddrsGeneric :: (MonadIO m, PersistUnique b, PersistQuery b)
                    => Entity (DbAccountGeneric b)      -- ^ Account
                    -> Bool                             -- ^ Internal
                    -> ReaderT b m [DbAddressGeneric b] -- ^ Unused addresses
unusedAddrsGeneric (Entity ai acc) internal = do
    when (dbAccountGap acc <= 0) $ liftIO . throwIO $ WalletException 
        "No addresses available: account gap is zero (or less)"
    addrs <- selectList [ DbAddressAccount ==. ai
                        , DbAddressInternal ==. internal
                        ]
                        [ Desc DbAddressIndex
                        , LimitTo (dbAccountGap acc)
                        ]
    return $ reverse $ map entityVal addrs

-- | Generate new payment addresses for an account
newAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
         => WalletName  -- ^ Wallet name
         -> AccountName -- ^ Account name
         -> Int         -- ^ Count
         -> ReaderT b m [PaymentAddress]  -- ^ Newly generated addresses
newAddrs wallet name cnt = do
    Entity wk _ <- getWalletEntity wallet
    acc <- getAccountEntity wk name
    liftM (map toPaymentAddr) $ newAddrsGeneric acc False cnt

newAddrsGeneric :: (MonadIO m, PersistUnique b, PersistQuery b)
                => Entity (DbAccountGeneric b)
                -> Bool      -- ^ Internal
                -> Int       -- ^ Count
                -> ReaderT b m [DbAddressGeneric b]
newAddrsGeneric (Entity ai acc) internal cnt = do
    time <- liftIO getCurrentTime
    lstEntM <- selectFirst
        [ DbAddressAccount ==. ai
        , DbAddressInternal ==. internal
        ]
        [ Desc DbAddressIndex ]
    let nxtIdx = case lstEntM of Nothing -> 0
                                 Just (Entity _ lst) -> dbAddressIndex lst + 1
    let build (a,i) = DbAddress a "" i ai internal time
        as = map build . take cnt $ f nxtIdx
    _ <- insertMany as
    return as
  where 
    f idx | isMSAccount $ dbAccountValue acc = 
              (if internal then intMulSigAddrs else extMulSigAddrs)
                  (AccPubKey $ head $ accountKeys $ dbAccountValue acc)
                  (tail $ accountKeys $ dbAccountValue acc) 
                  (accountRequired $ dbAccountValue acc)
                  idx
          | otherwise =
              (if internal then intAddrs else extAddrs)
                  (accountKey $ dbAccountValue acc)
                  idx

-- | Add a label to an address.
setAddrLabel :: (MonadIO m, PersistQuery b, PersistUnique b)
             => WalletName  -- ^ Wallet name
             -> AccountName -- ^ Account name
             -> KeyIndex    -- ^ Derivation index of the address
             -> String      -- ^ New label
             -> ReaderT b m PaymentAddress -- ^ New address information
setAddrLabel wallet name key label = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    (Entity i add) <- getAddressEntity ai key False
    let new = add { dbAddressLabel = label }
    replace i new
    return $ toPaymentAddr new

-- | Returns the private key of an address.
addressPrvKey :: (MonadIO m, PersistQuery b, PersistUnique b)
              => WalletName  -- ^ Account name
              -> AccountName -- ^ Account name
              -> KeyIndex    -- ^ Derivation index of the address
              -> ReaderT b m PrvKeyC -- ^ Private key
addressPrvKey wallet name key = do
    accPrv <- accountPrvKey wallet name
    add    <- getAddress wallet name key False
    let addrPrvKey = fromJust $ extPrvKey accPrv $ addressIndex add
    return $ xPrvKey $ getAddrPrvKey addrPrvKey

-- Returns the number of new addresses created
adjustLookAhead :: (MonadIO m, PersistUnique b, PersistQuery b)
                => DbAddressGeneric b -> ReaderT b m Int
adjustLookAhead a = do
    acc <- liftM fromJust $ get ai
    cnt <- count [ DbAddressIndex >. dbAddressIndex a
                 , DbAddressAccount ==. ai
                 , DbAddressInternal ==. dbAddressInternal a
                 ]
    let diff = dbAccountGap acc - cnt
    when (diff > 0) $ do
        _ <- newAddrsGeneric (Entity ai acc) (dbAddressInternal a) diff
        return ()
    return $ max 0 diff
  where
    ai = dbAddressAccount a

-- | Add addresses to an account and increase gap accordingly.  This will add
-- both internal and external addresses.
addLookAhead :: (MonadIO m, PersistUnique b, PersistQuery b)
             => WalletName  -- ^ Wallet name
             -> AccountName -- ^ Account name
             -> Int         -- ^ Count
             -> ReaderT b m ()
addLookAhead wallet name cnt = do
    Entity wk _ <- getWalletEntity wallet
    accE@(Entity ai acc) <- getAccountEntity wk name
    _ <- newAddrsGeneric accE True  cnt
    _ <- newAddrsGeneric accE False cnt
    replace ai acc { dbAccountGap = dbAccountGap acc + cnt }
    return ()

