{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Address 
( getAddress
, addressList
, addressCount
, addressPage
, unusedAddrs
, unusedAddr
, unlabeledAddr
, changeAddr
, newAddrs
, newAddrsGeneric
, setAddrLabel
, addressPrvKey
, addLookAhead
, adjustLookAhead
) where

import Control.Monad (liftM, when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust, isNothing)
import Data.Time (getCurrentTime)

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , getBy
    , get
    , replace
    , count
    , selectList
    , selectFirst
    , insertMany
    , entityVal
    , (==.), (>=.), (>.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
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
getAddress :: PersistUnique m
           => AccountName       -- ^ Account name
           -> KeyIndex          -- ^ Derivation index (key)
           -> m PaymentAddress  -- ^ Payment address
getAddress accName key = 
    liftM (toPaymentAddr . entityVal) $ getAddressEntity accName key False

getAddressEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
                 => AccountName -> KeyIndex -> Bool 
                 -> m (Entity (DbAddressGeneric b))
getAddressEntity accName key internal = do
    (Entity ai _) <- getAccountEntity accName
    entM <- getBy $ UniqueAddressKey ai key internal
    when (isNothing entM) $ liftIO $ 
        throwIO $ WalletException "The address has not been generated yet"
    return $ fromJust entM

-- | Returns all addresses for an account.
addressList :: (PersistUnique m, PersistQuery m)
            => AccountName        -- ^ Account name
            -> m [PaymentAddress] -- ^ Payment addresses
addressList name = do
    (Entity ai _) <- getAccountEntity name
    addrs <- selectList [ DbAddressAccount ==. ai 
                        , DbAddressInternal ==. False
                        ]
                        [ Asc DbAddressId ]
    return $ map (toPaymentAddr . entityVal) addrs

-- | Returns a count of all addresses in an account.
addressCount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
             => AccountName  -- ^ Account name
             -> m Int        -- ^ Address count
addressCount name = do
    (Entity ai _) <- getAccountEntity name
    count [ DbAddressAccount ==. ai 
          , DbAddressInternal ==. False
          ]

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
addressPage :: (PersistUnique m, PersistQuery m)
            => AccountName            -- ^ Account name
            -> Int                    -- ^ Requested page number
            -> Int                    -- ^ Number of addresses per page
            -> m ([PaymentAddress], Int) 
                -- ^ (Requested page, Highest page number)
addressPage name pageNum resPerPage 
    | pageNum < 0 = liftIO $ throwIO $ WalletException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ WalletException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = do
        (Entity ai _) <- getAccountEntity name
        addrCount <- addressCount name
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise    = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ WalletException $ 
            unwords ["The page number", show pageNum, "is too high"]
        addrs <- selectList 
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. False
                ] 
                [ Asc DbAddressId
                , LimitTo resPerPage
                , OffsetBy $ (page - 1) * resPerPage
                ]
        return $ ((map (toPaymentAddr . entityVal) addrs), maxPage)

-- | Get list of unused addresses: those in the account "gap".
unusedAddrs :: (PersistUnique m, PersistQuery m)
            => AccountName        -- ^ Account name
            -> m [PaymentAddress] -- ^ Unused addresses
unusedAddrs name = do
    Entity ai acc <- getAccountEntity name
    addrs <- selectList [ DbAddressAccount ==. ai
                        , DbAddressInternal ==. False
                        ]
                        [ Desc DbAddressIndex
                        , LimitTo (dbAccountGap acc)
                        ]
    return . reverse $ map (toPaymentAddr . entityVal) addrs

-- | Get first unused unlabeled address.
unlabeledAddr :: (PersistUnique m, PersistQuery m)
              => AccountName      -- ^ Account name
              -> m PaymentAddress -- ^ Unlabeled unused address
unlabeledAddr name = do
    frst <- unusedAddrGeneric name False
    Entity ai _ <- getAccountEntity name
    addrM <- selectFirst [ DbAddressAccount ==. ai
                         , DbAddressInternal ==. False
                         , DbAddressIndex >=. dbAddressIndex frst
                         , DbAddressLabel ==. ""
                         ] []
    when (isNothing addrM) $ liftIO . throwIO $
        WalletException "No unlabeled addresses available"
    return . toPaymentAddr . entityVal $ fromJust addrM

-- | Get first unused address.  Faster than previous function to retrieve a
-- single address.
unusedAddr :: (PersistUnique m, PersistQuery m)
         => AccountName      -- ^ Account name
         -> m PaymentAddress -- ^ Unused addresses
unusedAddr name = liftM toPaymentAddr $ unusedAddrGeneric name False

-- | Get first unused change address.
changeAddr :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
           => AccountName               -- ^ Account name
           -> m (DbAddressGeneric b)    -- ^ First unused change address
changeAddr name = unusedAddrGeneric name True

-- | Get first unused payment or change address.
unusedAddrGeneric
    :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
    => AccountName              -- ^ Account name
    -> Bool                     -- ^ Internal
    -> m (DbAddressGeneric b)   -- ^ Address
unusedAddrGeneric name internal = do
    Entity ai acc <- getAccountEntity name
    when (dbAccountGap acc <= 0) $ liftIO . throwIO $
        WalletException "No addresses available: account gap is zero (or less)"
    addrM <- selectFirst [ DbAddressAccount ==. ai
                         , DbAddressInternal ==. internal
                         ]
                         [ Desc DbAddressIndex
                         , OffsetBy (dbAccountGap acc - 1)
                         ]
    when (isNothing addrM) $ liftIO . throwIO $
        WalletException "No addresses available, although there should be"
    return . entityVal $ fromJust addrM

-- | Generate new payment addresses for an account
newAddrs :: (PersistUnique m, PersistQuery m)
         => AccountName         -- ^ Account name
         -> Int                 -- ^ Count
         -> m [PaymentAddress]  -- ^ Newly generated addresses
newAddrs name = liftM (map toPaymentAddr) . newAddrsGeneric name False

newAddrsGeneric :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
                => AccountName
                -> Bool      -- ^ Internal
                -> Int       -- ^ Count
                -> m [DbAddressGeneric b]
newAddrsGeneric name internal cnt = do
    time <- liftIO getCurrentTime
    Entity ai acc <- getAccountEntity name
    lstEntM <- selectFirst
        [ DbAddressAccount ==. ai
        , DbAddressInternal ==. internal
        ]
        [ Desc DbAddressIndex ]
    let nxtIdx = case lstEntM of Nothing -> 0
                                 Just (Entity _ lst) -> dbAddressIndex lst + 1
    let build (a,i) = DbAddress a "" i ai internal time
        as = map build . take cnt $ f acc nxtIdx
    _ <- insertMany as
    return as
  where 
    f acc idx | isMSAccount $ dbAccountValue acc = 
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
setAddrLabel :: PersistUnique m
             => AccountName      -- ^ Account name
             -> KeyIndex         -- ^ Derivation index of the address
             -> String           -- ^ New label
             -> m PaymentAddress -- ^ New address information
setAddrLabel name key label = do
    (Entity i add)   <- getAddressEntity name key False
    let new = add { dbAddressLabel = label }
    replace i new
    return $ toPaymentAddr new

-- | Returns the private key of an address.
addressPrvKey :: PersistUnique m
              => AccountName      -- ^ Account name
              -> KeyIndex         -- ^ Derivation index of the address
              -> m PrvKey         -- ^ Private key
addressPrvKey name key = do
    accPrv <- accountPrvKey name
    add    <- getAddress name key 
    let addrPrvKey = fromJust $ extPrvKey accPrv $ addressIndex add
    return $ xPrvKey $ getAddrPrvKey addrPrvKey

adjustLookAhead :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
                => DbAddressGeneric b -> m ()
adjustLookAhead a = do
    acc <- liftM fromJust (get $ dbAddressAccount a)
    cnt <- count [ DbAddressIndex >. dbAddressIndex a
                 , DbAddressAccount ==. dbAddressAccount a
                 , DbAddressInternal ==. dbAddressInternal a
                 ]
    let diff = dbAccountGap acc - cnt
    when (diff > 0) $ do
        _ <- newAddrsGeneric (dbAccountName acc) (dbAddressInternal a) diff
        return ()

-- | Add addresses to an account and increase gap accordingly.  This will add
-- both internal and external addresses.
addLookAhead :: (PersistUnique m, PersistQuery m)
             => AccountName -- ^ Account name
             -> Int         -- ^ Count
             -> m ()
addLookAhead name cnt = do
    _ <- newAddrsGeneric name True  cnt
    _ <- newAddrsGeneric name False cnt
    Entity ai acc <- getAccountEntity name
    replace ai acc { dbAccountGap = dbAccountGap acc + cnt }
    return ()

