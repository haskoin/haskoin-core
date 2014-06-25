{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Address 
( getAddress
, addressList
, addressCount
, addressPage
, newAddrs
, newAddrsGeneric
, addressLabel
, addressPrvKey
, setLookAhead
, adjustLookAhead
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
import Network.Haskoin.Wallet.Account

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
                    -> m ()
setLookAhead name lookAhead = do
    setLookAheadGeneric name lookAhead True
    setLookAheadGeneric name lookAhead False

setLookAheadGeneric :: (PersistUnique m, PersistQuery m)
                    => AccountName -- Account name
                    -> Int         -- Number of look-ahead addresses 
                    -> Bool        -- True for internal addresses
                    -> m ()
setLookAheadGeneric name lookAhead internal = checkInit >> do
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

