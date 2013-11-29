{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.DbAddress 
( cmdList
, cmdGenAddrs
, cmdGenWithLabel
, dbGenIntAddrs
, dbGenAddrs
, cmdLabel
, dbGetAddr
, yamlAddr
, yamlAddrList
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Time
import Data.Yaml
import Data.Maybe
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Conduit as C

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Store.DbAccount
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

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

dbGetAddr :: ( PersistUnique m 
             , PersistMonadBackend m ~ b
             )
          => String 
          -> EitherT String m (Entity (DbAddressGeneric b))
dbGetAddr addr = 
    liftMaybe addrErr =<< (getBy $ UniqueAddress addr)
  where 
    addrErr = unwords ["dbGetAddr: Invalid address", addr]

-- |Return a page of addresses. pageNum = 0 computes the last page
cmdList :: (PersistStore m, PersistUnique m, PersistQuery m) 
        => AccountName -> Int -> Int -> EitherT String m Value
cmdList name pageNum resPerPage 
    | pageNum < 0    = left $ 
        unwords ["cmdList: Invalid page number", show pageNum]
    | resPerPage < 1 = left $ 
        unwords ["cmdList: Invalid results per page",show resPerPage]
    | otherwise = do
        (Entity ai acc) <- dbGetAcc name
        addrCount <- count [ DbAddressAccount ==. ai
                           , DbAddressInternal ==. False
                           ] 
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise = pageNum
        when (page > maxPage) $ left "cmdList: Page number too high"
        addrs <- selectList [ DbAddressAccount ==. ai
                            , DbAddressInternal ==. False
                            ] 
                            [ Asc DbAddressId
                            , LimitTo resPerPage
                            , OffsetBy $ (page - 1) * resPerPage
                            ]
        return $ yamlAddrList (map entityVal addrs) page resPerPage addrCount

cmdGenAddrs :: ( PersistStore m
               , PersistUnique m
               , PersistQuery m
               )
            => AccountName -> Int -> EitherT String m Value
cmdGenAddrs name c = cmdGenWithLabel name (replicate c "")

cmdGenWithLabel :: ( PersistStore m
                   , PersistUnique m
                   , PersistQuery m
                   )
                => AccountName -> [String] -> EitherT String m Value
cmdGenWithLabel name labels = do
    addrs <- dbGenAddrs name labels False
    return $ toJSON $ map yamlAddr addrs

dbGenIntAddrs :: ( PersistStore m
                 , PersistUnique m
                 , PersistQuery m
                 , PersistMonadBackend m ~ b
                 )
              => AccountName -> Int 
              -> EitherT String m [DbAddressGeneric b]
dbGenIntAddrs name c = dbGenAddrs name (replicate c "") True


dbGenAddrs :: ( PersistStore m
              , PersistUnique m
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
            build ((s,i),l) = DbAddress 
                                s l (fromIntegral i) 
                                (concat [dbAccountTree acc,tree,show i,"/"])
                                ai internal time
        ls <- liftMaybe keyErr $ f acc
        let addrs     = map build $ zip ls labels
            lastDeriv = dbAddressIndex $ last addrs
            newAcc | internal  = acc{ dbAccountIntIndex = lastDeriv }
                   | otherwise = acc{ dbAccountExtIndex = lastDeriv }
        replace ai newAcc
        insertMany addrs
        return addrs
  where 
    keyErr = "cmdGenAddr: Error decoding account keys"
    f acc | isMSAcc acc = (if internal then intMulSigAddrs else extMulSigAddrs)
              <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
              <*> (mapM xPubImport $ dbAccountMsKeys acc) 
              <*> (dbAccountMsRequired acc)
              <*> (return $ fromIntegral $ lastIndex acc + 1)
          | otherwise = (if internal then intAddrs else extAddrs)
              <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
              <*> (return $ fromIntegral $ lastIndex acc + 1)
    lastIndex | internal  = dbAccountIntIndex
              | otherwise = dbAccountExtIndex

cmdLabel :: (PersistStore m, PersistUnique m) 
         => AccountName -> Int -> String -> EitherT String m Value
cmdLabel name key label = do
    (Entity ai acc) <- dbGetAcc name
    (Entity i addr) <- liftMaybe keyErr =<< (getBy $ UniqueAddressKey ai key)
    let newAddr = addr{dbAddressLabel = label}
    replace i newAddr
    return $ yamlAddr newAddr
  where 
    keyErr = unwords ["cmdLabel: Key",show key,"does not exist"]

