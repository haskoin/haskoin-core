{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Address 
( getAddress
, addressList
, addressCount
, addressPage
, newAddr
, newAddrGeneric
, setAddrLabel
, addressPrvKey
, addLookAhead
, adjustLookAhead
) where

import Control.Monad (liftM, replicateM, when)
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
    , insert_
    , entityVal
    , (==.), (<=.), (>.), (>=.)
    , SelectOpt( Asc, LimitTo, OffsetBy )
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
    (Entity ai acc) <- getAccountEntity accName
    when (isNothing $ dbAccountExtIndex acc) $ liftIO $ throwIO $
        WalletException "You have not generated any addresses in this account"
    entM <- getBy $ UniqueAddressKey ai key internal
    -- Make sure we are not fetching a look-ahead address
    when (isNothing entM || key > fromJust (dbAccountExtIndex acc)) $ liftIO $ 
        throwIO $ WalletException "The address has not been generated yet"
    return $ fromJust entM

-- | Returns all addresses for an account (excluding look-aheads and internal
-- addresses)
addressList :: (PersistUnique m, PersistQuery m)
            => AccountName        -- ^ Account name
            -> m [PaymentAddress] -- ^ Payment addresses
addressList name = do
    (Entity ai acc) <- getAccountEntity name
    addrs <- case dbAccountExtIndex acc of
        Nothing -> return []
        Just x -> selectList 
            [ DbAddressAccount ==. ai 
            , DbAddressInternal ==. False
            , DbAddressIndex <=. x
            ]
            [ Asc DbAddressId ]
    return $ map (toPaymentAddr . entityVal) addrs

-- | Returns a count of all addresses in an account (excluding look-aheads and
-- internal addresses)
addressCount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
             => AccountName  -- ^ Account name
             -> m Int        -- ^ Address count
addressCount name = do
    (Entity ai acc) <- getAccountEntity name
    case dbAccountExtIndex acc of
        Nothing -> return 0
        Just x  -> count 
            [ DbAddressAccount ==. ai 
            , DbAddressInternal ==. False
            , DbAddressIndex <=. x
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
        (Entity ai acc) <- getAccountEntity name
        addrCount <- addressCount name
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise    = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ WalletException $ 
            unwords ["The page number", show pageNum, "is too high"]
        addrs <- case dbAccountExtIndex acc of
            Nothing -> return []
            Just x  -> selectList 
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. False
                , DbAddressIndex <=. x
                ] 
                [ Asc DbAddressId
                , LimitTo resPerPage
                , OffsetBy $ (page - 1) * resPerPage
                ]
        return $ ((map (toPaymentAddr . entityVal) addrs), maxPage)

-- | Generate new payment address for an account
newAddr :: (PersistUnique m, PersistQuery m)
        => AccountName       -- ^ Account name
        -> m PaymentAddress  -- ^ Newly generated addresses
newAddr name = liftM toPaymentAddr $ newAddrGeneric name False False

newAddrGeneric :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
               => AccountName
               -> Bool      -- ^ Internal
               -> Bool      -- ^ Lookahead only
               -> m (DbAddressGeneric b)
newAddrGeneric name internal la = do
    time <- liftIO getCurrentTime
    Entity ai acc <- getAccountEntity name
    let build (a,i) = DbAddress a "" i ai internal time
        g = build $ head $ f acc
    insert_ g
    Just (Entity _ l) <- selectFirst
        [ case fIndex acc of
            Nothing -> DbAddressIndex >=. 0
            Just  x -> DbAddressIndex >.  x
        , DbAddressAccount ==. ai
        , DbAddressInternal ==. internal
        ] []
    let lastLookAhead = Just (dbAddressIndex g)
        lastIndex     = Just (dbAddressIndex l)
        newAcc
            | la && internal = acc { dbAccountIntLookAhead = lastLookAhead }
            | la             = acc { dbAccountExtLookAhead = lastLookAhead }
            | internal       = acc { dbAccountIntLookAhead = lastLookAhead
                                   , dbAccountIntIndex     = lastIndex }
            | otherwise      = acc { dbAccountExtLookAhead = lastLookAhead
                                   , dbAccountExtIndex     = lastIndex }
    replace ai newAcc
    return g
  where 
    f acc | isMSAccount $ dbAccountValue acc = 
              (if internal then intMulSigAddrs else extMulSigAddrs)
                  (AccPubKey $ head $ accountKeys $ dbAccountValue acc)
                  (tail $ accountKeys $ dbAccountValue acc) 
                  (accountRequired $ dbAccountValue acc)
                  (maybe 0 (+1) $ fLookAhead acc)
          | otherwise = (if internal then intAddrs else extAddrs)
              (accountKey $ dbAccountValue acc)
              (maybe 0 (+1) $ fLookAhead acc)
    fLookAhead | internal  = dbAccountIntLookAhead
               | otherwise = dbAccountExtLookAhead
    fIndex     | internal  = dbAccountIntIndex
               | otherwise = dbAccountExtIndex

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
    let fIndex | dbAddressInternal a = dbAccountIntIndex 
               | otherwise           = dbAccountExtIndex
    diff <- count [ case fIndex acc of
                      Nothing -> DbAddressIndex >=. 0
                      Just x  -> DbAddressIndex >. x
                  , DbAddressIndex <=. dbAddressIndex a
                  , DbAddressAccount ==. dbAddressAccount a
                  , DbAddressInternal ==. dbAddressInternal a
                  ]
    when (diff > 0) $ do
        _ <- replicateM diff $
            newAddrGeneric (dbAccountName acc) (dbAddressInternal a) False
        return ()

-- | Add one look-ahead address to an account. This will add one to both
-- internal and external addresses.
addLookAhead :: (PersistUnique m, PersistQuery m)
             => AccountName -- ^ Account name
             -> m ()
addLookAhead name = do
    _ <- addLookAheadGeneric name True
    _ <- addLookAheadGeneric name False
    return ()

addLookAheadGeneric :: (PersistUnique m, PersistQuery m)
                    => AccountName -- Account name
                    -> Bool        -- True for internal addresses
                    -> m ()
addLookAheadGeneric name internal =
    newAddrGeneric name internal True >> return ()
