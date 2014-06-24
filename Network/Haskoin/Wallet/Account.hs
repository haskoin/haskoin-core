{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Account 
( -- Account
  AccountName
, getAccount
, getAccountEntity
, getWalletEntity
, newAccount
, newMSAccount
, addAccountKeys
, accountList
, accountPrvKey
, isMSAccount

  -- Address
, getAddress
, addressList
, addressCount
, addressPage
, newAddrs
, newAddrsGeneric
, addressLabel
, addressPrvKey
, setLookAhead
, adjustLookAhead

, checkInit
, isWalletInit
) where

import Control.Monad (liftM, unless, when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)

import Data.List (nub)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (getCurrentTime)

import Database.Persist
    ( PersistUnique
    , PersistQuery
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
    , insertMany
    , entityVal
    , (=.), (==.), (<=.), (>.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

type AccountName = String

isWalletInit :: PersistUnique m => String -> m Bool
isWalletInit name = do
    entM <- getBy $ UniqueWalletName name
    return $ isJust entM

checkInit :: PersistUnique m => m ()
checkInit = do
    isInit <- isWalletInit "main"
    unless isInit $ liftIO $ throwIO $ 
        InitializationException "Wallet main is not initialized"

getWalletEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> m (Entity (DbWalletGeneric b))
getWalletEntity name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InitializationException $ 
            unwords ["Wallet", name, "is not initialized"]

getAccountEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
             => AccountName -> m (Entity (DbAccountGeneric b))
getAccountEntity name = checkInit >> do
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
-- their name and they must be unique.
newAccount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b) 
             => String                 -- ^ Account name
             -> m (DbAccountGeneric b) -- ^ Returns the new account information
newAccount name = checkInit >> do
    time <- liftIO getCurrentTime
    (Entity wk w) <- getWalletEntity "main"
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
    setLookAhead name 30 False
    setLookAhead name 30 True
    return acc

-- | Create a new multisignature account. The thirdparty keys can be provided
-- now or later using the 'cmdAddKeys' command. The number of thirdparty keys
-- can not exceed n-1 as your own account key will be used as well in the
-- multisignature scheme. If less than n-1 keys are provided, the account will
-- be in a pending state and no addresses can be generated.
--
-- In order to prevent usage mistakes, you can not create a multisignature 
-- account with other keys from your own wallet.
newMSAccount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
             => String    -- ^ Account name
             -> Int       -- ^ Required number of keys (m in m of n)
             -> Int       -- ^ Total number of keys (n in m of n)
             -> [XPubKey] -- ^ Thirdparty public keys
             -> m (DbAccountGeneric b) -- ^ Returns the new account information
newMSAccount name m n mskeys = checkInit >> do
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        AccountSetupException "Invalid multisig parameters"
    unless (length keys < n) $ liftIO $ throwIO $
        AccountSetupException "Too many keys"
    checkOwnKeys keys
    (Entity wk w) <- getWalletEntity "main"
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
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        setLookAhead name 30 False
        setLookAhead name 30 True
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
    | otherwise = checkInit >> do
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
        let n = fromJust $ dbAccountMsTotal newAcc
        when (length (dbAccountMsKeys acc) == n - 1) $ do
            -- Generate gap addresses
            setLookAhead name 30 False
            setLookAhead name 30 True
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
accountList = checkInit >> (liftM (map entityVal) $ selectList [] [])

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
accountPrvKey :: (PersistUnique m, PersistMonadBackend m ~ b)
              => AccountName -- ^ Account name
              -> m AccPrvKey -- ^ Account private key
accountPrvKey name = checkInit >> do
    acc <- getAccount name
    w   <- liftM fromJust (get $ dbAccountWallet acc)
    let master = dbWalletMaster w
        deriv  = fromIntegral $ dbAccountIndex acc
    return $ fromJust $ accPrvKey master deriv

-- Address function --

getAddress :: (PersistUnique m, PersistMonadBackend m ~ b)
           => AccountName
           -> Int
           -> Bool 
           -> m (DbAddressGeneric b)
getAddress accName key internal = 
    liftM entityVal $ getAddressEntity accName key internal

getAddressEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
                 => AccountName -> Int -> Bool 
                 -> m (Entity (DbAddressGeneric b))
getAddressEntity accName key internal = checkInit >> do
    (Entity ai acc) <- getAccountEntity accName
    entM <- getBy $ UniqueAddressKey ai key internal
    -- Make sure we are not fetching a look-ahead address
    when (isNothing entM || key > dbAccountExtIndex acc) $ liftIO $ throwIO $
        InvalidAddressException "Invalid address key"
    return $ fromJust entM

-- | Returns all addresses for an account (excluding look-aheads and internal
-- addresses)
addressList :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
            => AccountName
            -> m [DbAddressGeneric b]
addressList name = checkInit >> do
    (Entity ai acc) <- getAccountEntity name
    addrs <- selectList [ DbAddressAccount ==. ai 
                        , DbAddressInternal ==. False
                        , DbAddressIndex <=. dbAccountExtIndex acc
                        ]
                        [ Asc DbAddressId ]
    return $ map entityVal addrs

-- | Returns a count of all addresses in an account (excluding look-aheads and
-- internal addresses)
addressCount :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
             => AccountName -> m Int
addressCount name = checkInit >> do
    (Entity ai acc) <- getAccountEntity name
    count [ DbAddressAccount ==. ai 
          , DbAddressInternal ==. False
          , DbAddressIndex <=. dbAccountExtIndex acc
          ]

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
addressPage :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
            => AccountName            -- ^ Account name
            -> Int                    -- ^ Requested page number
            -> Int                    -- ^ Number of addresses per page
            -> m ([DbAddressGeneric b], Int) 
                -- ^ (Requested page, Highest page number)
addressPage name pageNum resPerPage 
    | pageNum < 0 = liftIO $ throwIO $ InvalidPageException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ InvalidPageException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = checkInit >> do
        (Entity ai acc) <- getAccountEntity name
        addrCount <- addressCount name
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ InvalidPageException $ 
            unwords ["The page number", show pageNum, "is too high"]
        addrs <- selectList [ DbAddressAccount ==. ai
                            , DbAddressInternal ==. False
                            , DbAddressIndex <=. dbAccountExtIndex acc
                            ] 
                            [ Asc DbAddressId
                            , LimitTo resPerPage
                            , OffsetBy $ (page - 1) * resPerPage
                            ]

        return $ ((map entityVal addrs), maxPage)

newAddrs :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
         => AccountName 
         -> Int 
         -> m [DbAddressGeneric b]
newAddrs name cnt = newAddrsGeneric name cnt False

newAddrsGeneric :: ( PersistUnique m
                   , PersistQuery m
                   , PersistMonadBackend m ~ b
                   )
                => AccountName -> Int -> Bool -> m [DbAddressGeneric b]
newAddrsGeneric name cnt internal
    | cnt <= 0 = liftIO $ throwIO $
        AddressGenerationException "Can not generate less than 1 address"
    | otherwise = checkInit >> do
        time <- liftIO getCurrentTime
        (Entity ai acc) <- getAccountEntity name
        let tree | internal  = "1/"
                 | otherwise = "0/"
            build (a,i) = DbAddress 
                             a "" (fromIntegral i)
                             (concat [dbAccountTree acc,tree,show i,"/"])
                             ai internal time
        let gapAddr = map build $ take cnt $ f acc
        _ <- insertMany gapAddr
        resAddr <- liftM (map entityVal) $ selectList 
            [ DbAddressIndex >. fIndex acc
            , DbAddressAccount ==. ai
            , DbAddressInternal ==. internal
            ]
            [ Asc DbAddressIndex
            , LimitTo cnt
            ]
        let lastLookAhead = dbAddressIndex $ last gapAddr
            lastIndex = dbAddressIndex $ last resAddr
            newAcc | internal  = acc{ dbAccountIntLookAhead = lastLookAhead
                                    , dbAccountIntIndex     = lastIndex
                                    }
                   | otherwise = acc{ dbAccountExtLookAhead = lastLookAhead
                                    , dbAccountExtIndex     = lastIndex
                                    }
        replace ai newAcc
        return resAddr
  where 
    f acc | isMSAccount acc = 
              (if internal then intMulSigAddrs else extMulSigAddrs)
                  (dbAccountKey acc)
                  (dbAccountMsKeys acc) 
                  (fromJust $ dbAccountMsRequired acc)
                  (fromIntegral $ fLookAhead acc + 1)
          | otherwise = (if internal then intAddrs else extAddrs)
              (dbAccountKey acc)
              (fromIntegral $ fLookAhead acc + 1)
    fLookAhead | internal  = dbAccountIntLookAhead
               | otherwise = dbAccountExtLookAhead
    fIndex | internal  = dbAccountIntIndex
           | otherwise = dbAccountExtIndex

-- | Add a label to an address.
addressLabel :: (PersistUnique m, PersistMonadBackend m ~ b)
             => AccountName   -- ^ Account name
             -> Int           -- ^ Derivation index of the address
             -> String        -- ^ New label
             -> m (DbAddressGeneric b) -- ^ New address information
addressLabel name key label = checkInit >> do
    acc <- getAccount name
    when (key > dbAccountExtIndex acc) $ liftIO $ throwIO $
        InvalidAddressException "The address key does not exist"
    (Entity i add) <- getAddressEntity name key False
    let newAddr = add { dbAddressLabel = label }
    replace i newAddr
    return newAddr

-- | Returns the private key of an address.
addressPrvKey :: (PersistUnique m, PersistMonadBackend m ~ b)
              => AccountName      -- ^ Account name
              -> Int              -- ^ Derivation index of the address
              -> m PrvKey         -- ^ Private key
addressPrvKey name key = checkInit >> do
    acc <- getAccount name
    w   <- liftM fromJust (get $ dbAccountWallet acc)
    add <- getAddress name key False
    let master     = dbWalletMaster w
        deriv      = fromIntegral $ dbAccountIndex acc
        accKey     = fromJust $ accPrvKey master deriv
        index      = fromIntegral $ dbAddressIndex add
        addrPrvKey = fromJust $ extPrvKey accKey index
    return $ xPrvKey $ getAddrPrvKey addrPrvKey

adjustLookAhead :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
                => DbAddressGeneric b -> m ()
adjustLookAhead a = do
    acc <- liftM fromJust (get $ dbAddressAccount a)
    let fIndex | dbAddressInternal a = dbAccountIntIndex 
               | otherwise           = dbAccountExtIndex
    diff <- count [ DbAddressIndex >. fIndex acc
                  , DbAddressIndex <=. dbAddressIndex a
                  , DbAddressAccount ==. dbAddressAccount a
                  , DbAddressInternal ==. dbAddressInternal a
                  ]
    when (diff > 0) $ do
        _ <- newAddrsGeneric (dbAccountName acc) diff (dbAddressInternal a)
        return ()

-- | Set how many look ahead addresses to generate for an account
setLookAhead :: (PersistUnique m, PersistQuery m)
             => AccountName -- ^ Account name
             -> Int         -- ^ Number of look-ahead addresses 
             -> Bool        -- ^ True for internal addresses
             -> m ()
setLookAhead name lookAhead internal = checkInit >> do
    (Entity ai acc) <- getAccountEntity name 
    diff <- count [ DbAddressIndex >. fIndex acc
                  , DbAddressIndex <=. fLookAhead acc
                  , DbAddressAccount ==. ai
                  , DbAddressInternal ==. internal
                  ]
    when (diff < lookAhead) $ do
        _ <- newAddrsGeneric name (lookAhead - diff) internal
        return ()
    res <- liftM (map entityVal) $ selectList  
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. internal
                ]
                [ Desc DbAddressIndex
                , LimitTo (lookAhead + 1)
                ]
    let lastIndex | length res <= lookAhead = (-1)
                  | otherwise = dbAddressIndex $ last res
        lastLookAhead = dbAddressIndex $ head res
        newAcc | internal  = acc{ dbAccountIntIndex     = lastIndex 
                                , dbAccountIntLookAhead = lastLookAhead
                                }
               | otherwise = acc{ dbAccountExtIndex     = lastIndex 
                                , dbAccountExtLookAhead = lastLookAhead
                                }
    replace ai newAcc
  where 
    fIndex | internal  = dbAccountIntIndex 
           | otherwise = dbAccountExtIndex
    fLookAhead | internal  = dbAccountIntLookAhead 
               | otherwise = dbAccountExtLookAhead

