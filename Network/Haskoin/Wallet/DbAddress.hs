{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.DbAddress 
( dbGenIntAddrs
, dbGenAddrs
, dbAdjustGap
, dbSetGap
, dbGetAddr
, dbGetAddressByIndex
, yamlAddr
, yamlAddrList
) where

import Control.Monad (when, forM, liftM)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Data.Yaml (Value, object, (.=), toJSON)

import Database.Persist
    ( PersistQuery
    , PersistUnique
    , PersistStore
    , PersistMonadBackend
    , Entity(..)
    , get
    , getBy
    , selectList
    , insertMany
    , count
    , replace
    , (==.), (>.), (<=.)
    , SelectOpt( Asc, Desc, LimitTo )
    )
import Database.Persist.Sql (SqlBackend)

import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Crypto

yamlAddr :: DbAddressGeneric b -> Value
yamlAddr a
    | null $ dbAddressLabel a = object base
    | otherwise = object $ label:base
  where 
    base  = [ "Addr" .= dbAddressBase58 a
            , "Key"  .= dbAddressIndex a
            , "Tree" .= dbAddressTree a
            ]
    label = "Label" .= dbAddressLabel a

yamlAddrList :: [DbAddressGeneric b] -> Int -> Int -> Int -> Value
yamlAddrList addrs pageNum resPerPage addrCount = object
    [ "Addresses" .= (toJSON $ map yamlAddr addrs)
    , "Page results" .= object
        [ "Current page"     .= pageNum
        , "Results per page" .= resPerPage
        , "Total pages"      .= totPages
        , "Total addresses"  .= addrCount
        ]
    ]
  where totPages = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage

dbGetAddr :: (PersistUnique m, PersistMonadBackend m ~ b)
          => String 
          -> m (Entity (DbAddressGeneric b))
dbGetAddr addrStr = do
    entM <- getBy $ UniqueAddress addrStr
    case entM of
        Just ent -> return ent
        Nothing -> liftIO $ throwIO $
            InvalidAddressException $ unwords ["Invalid address", addrStr]

dbGetAddressByIndex :: ( PersistUnique m
                       , PersistMonadBackend m ~ SqlBackend
                       )
                    => DbAccountId
                    -> Int
                    -> Bool 
                    -> m (Entity (DbAddressGeneric SqlBackend))
dbGetAddressByIndex accKey index internal = do
    entM <- getBy $ UniqueAddressKey accKey index internal
    case entM of
        Just ent -> return ent 
        Nothing  -> liftIO $ throwIO $
            InvalidAddressException "Invalid address key"

dbAdjustGap :: ( PersistUnique m
               , PersistQuery m
               , PersistMonadBackend m ~ b
               )
            => DbAddressGeneric b -> m ()
dbAdjustGap a = do
    acc <- liftM fromJust (get $ dbAddressAccount a)
    let fIndex | dbAddressInternal a = dbAccountIntIndex 
               | otherwise           = dbAccountExtIndex
    diff <- count [ DbAddressIndex >. fIndex acc
                  , DbAddressIndex <=. dbAddressIndex a
                  , DbAddressAccount ==. dbAddressAccount a
                  , DbAddressInternal ==. dbAddressInternal a
                  ]
    when (diff > 0) $ do
        _ <- dbGenAddrs (dbAccountName acc) 
                        (replicate diff "") 
                        (dbAddressInternal a)
        return ()

dbSetGap :: ( PersistUnique m
            , PersistQuery m
            )
         => AccountName -> Int -> Bool -> m ()
dbSetGap name gap internal = do
    (Entity ai acc) <- dbGetAccount name 
    diff <- count [ DbAddressIndex >. fIndex acc
                  , DbAddressIndex <=. fGap acc
                  , DbAddressAccount ==. ai
                  , DbAddressInternal ==. internal
                  ]
    when (diff < gap) $ do
        _ <- dbGenAddrs name (replicate (gap - diff) "") internal
        return ()
    res <- liftM (map entityVal) $ selectList  
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. internal
                ]
                [ Desc DbAddressIndex
                , LimitTo (gap + 1)
                ]
    let lastIndex | length res <= gap = (-1)
                  | otherwise         = dbAddressIndex $ last res
        lastGap = dbAddressIndex $ head res
        newAcc | internal  = acc{ dbAccountIntIndex = lastIndex 
                                , dbAccountIntGap   = lastGap
                                }
               | otherwise = acc{ dbAccountExtIndex = lastIndex 
                                , dbAccountExtGap   = lastGap
                                }
    replace ai newAcc
  where 
    fIndex | internal  = dbAccountIntIndex 
           | otherwise = dbAccountExtIndex
    fGap   | internal  = dbAccountIntGap 
           | otherwise = dbAccountExtGap

dbGenIntAddrs :: ( PersistUnique m
                 , PersistQuery m
                 , PersistMonadBackend m ~ b
                 )
              => AccountName -> Int 
              -> m [DbAddressGeneric b]
dbGenIntAddrs name c 
    | c <= 0    = liftIO $ throwIO $ AddressGenerationException 
        "dbGenIntAddrs: Count argument must be greater than 0"
    | otherwise = dbGenAddrs name (replicate c "") True

dbGenAddrs :: ( PersistUnique m
              , PersistQuery m
              , PersistMonadBackend m ~ b
              )
           => AccountName -> [String] -> Bool 
           -> m [DbAddressGeneric b]
dbGenAddrs name labels internal
    | null labels = liftIO $ throwIO $
        AddressGenerationException "Labels can not be empty"
    | otherwise = do
        time <- liftIO getCurrentTime
        (Entity ai acc) <- dbGetAccount name
        let tree | internal  = "1/"
                 | otherwise = "0/"
            build (s,i) = DbAddress 
                             s "" (fromIntegral i)
                             (concat [dbAccountTree acc,tree,show i,"/"])
                             ai internal time
        let gapAddr = map build $ take (length labels) $ f acc
        _ <- insertMany gapAddr
        resAddr <- selectList 
            [ DbAddressIndex >. fIndex acc
            , DbAddressAccount ==. ai
            , DbAddressInternal ==. internal
            ]
            [ Asc DbAddressIndex
            , LimitTo $ length labels
            ]
        let lastGap   = dbAddressIndex $ last gapAddr
            lastIndex = dbAddressIndex $ entityVal $ last resAddr
            newAcc | internal  = acc{ dbAccountIntGap   = lastGap
                                    , dbAccountIntIndex = lastIndex
                                    }
                   | otherwise = acc{ dbAccountExtGap   = lastGap
                                    , dbAccountExtIndex = lastIndex
                                    }
        replace ai newAcc
        forM (zip resAddr labels) $ \(Entity idx a,l) -> do
            let newAddr = a{ dbAddressLabel = l }
            replace idx newAddr 
            return newAddr
  where 
    f acc | isMSAcc acc = (if internal then intMulSigAddrs else extMulSigAddrs)
              (dbAccountKey acc)
              (dbAccountMsKeys acc) 
              (fromJust $ dbAccountMsRequired acc)
              (fromIntegral $ fGap acc + 1)
          | otherwise = (if internal then intAddrs else extAddrs)
              (dbAccountKey acc)
              (fromIntegral $ fGap acc + 1)
    fGap   | internal  = dbAccountIntGap
           | otherwise = dbAccountExtGap
    fIndex | internal  = dbAccountIntIndex
           | otherwise = dbAccountExtIndex


