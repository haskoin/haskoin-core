{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Wallet.Store.DbAddress 
( cmdList
, cmdGenAddr
, cmdGenWithLabel
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
    where base  = [ "Addr" .= dbAddressBase58 a
                  , "Key"  .= dbAddressDerivation a
                  , "Tree" .= dbAddressTree a
                  ]
          label = "Label" .= dbAddressLabel a

yamlAddrList :: [DbAddressGeneric b] -> Int -> Int -> Int -> Value
yamlAddrList addrs pageNum resPerPage addrCount = object
    [ "Addresses" .= (toJSON $ map yamlAddr addrs)
    , "Page results" .= object
        [ "Current Page" .= pageNum
        , "Results per page"  .= resPerPage
        , "Total Pages" .= ((addrCount + resPerPage - 1) `div` resPerPage)
        , "Total Addresses" .= addrCount
        ]
    ]

dbGetAddr :: PersistUnique m 
          => String 
          -> EitherT String m 
              (Entity (DbAddressGeneric (PersistMonadBackend m)))
dbGetAddr addr = liftMaybe addrErr =<< (getBy $ UniqueAddress addr)
    where addrErr = unwords ["dbGetAddr: Invalid address", addr]

-- |Return a page of addresses. pageNum = 0 computes the last page
cmdList :: (PersistStore m, PersistUnique m, PersistQuery m) 
        => AccountName -> Int -> Int -> EitherT String m Value
cmdList name pageNum resPerPage = do
    (Entity ai acc) <- liftMaybe accErr =<< (getBy $ UniqueAccName name)
    addrCount <- count [ DbAddressAccount ==. ai
                       , DbAddressInternal ==. False
                       ] 
    let maxPage = (addrCount + resPerPage - 1) `div` resPerPage
        page = if pageNum == 0 then maxPage else pageNum
    addrs <- selectList [ DbAddressAccount ==. ai
                        , DbAddressInternal ==. False
                        ] 
                        [ Asc DbAddressId
                        , LimitTo resPerPage
                        , OffsetBy $ (page - 1) * resPerPage
                        ]
    return $ yamlAddrList (map entityVal addrs) page resPerPage addrCount
    where accErr = unwords ["cmdList: Account", name, "does not exist"]

cmdGenAddr :: (PersistStore m, PersistUnique m, PersistQuery m)
           => AccountName -> Int -> EitherT String m Value
cmdGenAddr name c = cmdGenWithLabel name (replicate c "")

cmdGenWithLabel :: (PersistStore m, PersistUnique m, PersistQuery m)
                => AccountName -> [String] -> EitherT String m Value
cmdGenWithLabel name labels = do
    time <- liftIO getCurrentTime
    (Entity ai acc) <- liftMaybe accErr =<< (getBy $ UniqueAccName name)
    let build ((s,i),l) = DbAddress 
                            s l (fromIntegral i) 
                            (concat [dbAccountTree acc,"0/",show i,"/"])
                            ai False time
    ls <- liftMaybe keyErr $ f acc
    let addrs     = map build $ zip ls labels
        lastDeriv = dbAddressDerivation $ last addrs
        newAcc    = acc{ dbAccountExtDerivation = lastDeriv }
    replace ai newAcc
    insertMany addrs
    return $ toJSON $ map yamlAddr addrs
    where accErr = unwords ["cmdGenAddr: Account", name, "does not exist"]
          keyErr = "cmdGenAddr: Error decoding account keys"
          f acc | isMSAcc acc = extMulSigAddrs 
                    <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
                    <*> (mapM xPubImport $ dbAccountMsKeys acc) 
                    <*> (dbAccountMsRequired acc)
                    <*> (return $ fromIntegral $ dbAccountExtDerivation acc + 1)
                | otherwise   = extAddrs 
                    <$> (loadPubAcc =<< (xPubImport $ dbAccountKey acc))
                    <*> (return $ fromIntegral $ dbAccountExtDerivation acc + 1)

cmdLabel :: (PersistStore m, PersistUnique m) 
         => AccountName -> Int -> String -> EitherT String m Value
cmdLabel name key label = do
    (Entity ai acc) <- liftMaybe accErr =<< (getBy $ UniqueAccName name)
    (Entity i addr) <- liftMaybe keyErr =<< (getBy $ UniqueAddressKey ai key)
    let newAddr = addr{dbAddressLabel = label}
    replace i newAddr
    return $ yamlAddr newAddr
    where accErr = unwords ["cmdLabel: Account", name, "does not exist"]
          keyErr = unwords ["cmdLabel: Key",show key,"does not exist"]

