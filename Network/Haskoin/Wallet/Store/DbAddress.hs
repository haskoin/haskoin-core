{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Store.DbAddress 
( dbGenIntAddrs
, dbGenAddrs
, dbAdjustGap
, dbSetGap
, dbGetAddr
, yamlAddr
, yamlAddrList
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (when, forM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT, left)

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

import Network.Haskoin.Wallet.Keys
import Network.Haskoin.Wallet.Manager
import Network.Haskoin.Wallet.Store.DbAccount
import Network.Haskoin.Wallet.Store.Util
import Network.Haskoin.Util

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
          -> EitherT String m (Entity (DbAddressGeneric b))
dbGetAddr addrStr = 
    liftMaybe addrErr =<< (getBy $ UniqueAddress addrStr)
  where 
    addrErr = unwords ["dbGetAddr: Invalid address", addrStr]

dbGenIntAddrs :: ( PersistUnique m
                 , PersistQuery m
                 , PersistMonadBackend m ~ b
                 )
              => AccountName -> Int 
              -> EitherT String m [DbAddressGeneric b]
dbGenIntAddrs name c 
    | c <= 0    = left "dbGenIntAddrs: Count argument must be greater than 0"
    | otherwise = dbGenAddrs name (replicate c "") True

dbAdjustGap :: ( PersistUnique m
               , PersistQuery m
               , PersistMonadBackend m ~ b
               )
            => DbAddressGeneric b -> EitherT String m ()
dbAdjustGap a = do
    acc <- liftMaybe accErr =<< (get $ dbAddressAccount a)
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
  where
    accErr = "dbAdjustGap: Could not load address account"

dbSetGap :: ( PersistUnique m
            , PersistQuery m
            )
         => AccountName -> Int -> Bool -> EitherT String m ()
dbSetGap name gap internal = do
    (Entity ai acc) <- dbGetAcc name 
    diff <- count [ DbAddressIndex >. fIndex acc
                  , DbAddressIndex <=. fGap acc
                  , DbAddressAccount ==. ai
                  , DbAddressInternal ==. internal
                  ]
    when (diff < gap) $ do
        _ <- dbGenAddrs name (replicate (gap - diff) "") internal
        return ()
    res <- (map entityVal) <$> selectList  
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

dbGenAddrs :: ( PersistUnique m
              , PersistQuery m
              , PersistMonadBackend m ~ b
              )
           => AccountName -> [String] -> Bool 
           -> EitherT String m [DbAddressGeneric b]
dbGenAddrs name labels internal
    | null labels = left "dbGenAddr: Labels can not be empty"
    | otherwise = do
        time <- liftIO getCurrentTime
        (Entity ai acc) <- dbGetAcc name
        let tree | internal  = "1/"
                 | otherwise = "0/"
            build (s,i) = DbAddress 
                             s "" (fromIntegral i)
                             (concat [dbAccountTree acc,tree,show i,"/"])
                             ai internal time
        ls <- liftMaybe keyErr $ f acc
        let gapAddr = map build $ take (length labels) ls
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
    keyErr = "dbGenAddr: Error decoding account keys"
    f acc | isMSAcc acc = (if internal then intMulSigAddrs else extMulSigAddrs)
              <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
              <*> (mapM xPubImport $ dbAccountMsKeys acc) 
              <*> (dbAccountMsRequired acc)
              <*> (return $ fromIntegral $ fGap acc + 1)
          | otherwise = (if internal then intAddrs else extAddrs)
              <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
              <*> (return $ fromIntegral $ fGap acc + 1)
    fGap   | internal  = dbAccountIntGap
           | otherwise = dbAccountExtGap
    fIndex | internal  = dbAccountIntIndex
           | otherwise = dbAccountExtIndex


