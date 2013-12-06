{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store
-- Util functions
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, DbTxGeneric(..)
, DbTxBlobGeneric(..)
, DbWalletId
, DbAccountId
, DbAddressId
, DbCoinId
, DbTxId
, DbTxBlobId
, Unique(..)
, EntityField(..)
, AccountName
, CoinStatus(..)
, catStatus
, dbGetWallet
, dbGetTxBlob
, liftEither
, liftMaybe
, migrateAll

-- Account functions
, dbGetAcc
, dbNewAcc
, dbNewMS
, dbAddKeys
, cmdAccInfo
, cmdListAcc
, cmdDumpKeys
, yamlAcc
, isMSAcc

-- Address functions
, cmdList
, cmdGenAddrs
, cmdGenWithLabel
, dbGenIntAddrs
, dbGenAddrs
, dbAdjustGap
, dbSetGap
, cmdLabel
, cmdWIF
, dbGetAddr
, yamlAddr
, yamlAddrList

-- Coin functions
, cmdBalance
, cmdBalances
, dbCoins
, cmdCoins
, cmdAllCoins
, yamlCoin
, toCoin

-- Tx functions
, cmdImportTx 
, cmdRemoveTx
, dbImportTx
, cmdListTx
, cmdSend
, cmdSendMany
, dbSendTx
, dbSendSolution
, dbSendCoins
, cmdSignTx
, dbSignTx
, cmdDecodeTx
, cmdBuildRawTx
, cmdSignRawTx

-- Store functions
, cmdInit
, cmdNewAcc
, cmdNewMS
, cmdAddKeys
) where

import Control.Applicative
import Control.Monad
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
import Haskoin.Wallet.Store.DbAddress
import Haskoin.Wallet.Store.DbCoin
import Haskoin.Wallet.Store.DbTx
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

cmdInit :: ( PersistUnique m 
           , PersistQuery m 
           , PersistStore m
           )
        => String -> EitherT String m Value
cmdInit seed 
    | null seed = left "cmdInit: seed can not be empty"
    | otherwise = do
        time   <- liftIO getCurrentTime
        master <- liftMaybe err $ makeMasterKey $ stringToBS seed
        let str = xPrvExport $ runMasterKey master
        prev <- getBy $ UniqueWalletName "main"
        when (isJust prev) $ left
            "cmdInit: Wallet is already initialized"
        insert_ $ DbWallet "main" "full" str (-1) time
        return Null
  where 
    err = "cmdInit: Invalid master key generated from seed"

cmdNewAcc :: (PersistUnique m, PersistQuery m) 
         => String -> EitherT String m Value
cmdNewAcc name = do
    acc <- dbNewAcc name
    -- Generate gap addresses
    dbSetGap name 30 False
    dbSetGap name 30 True
    return $ yamlAcc acc

cmdNewMS :: (PersistUnique m, PersistQuery m)
         => String -> Int -> Int -> [XPubKey]
         -> EitherT String m Value
cmdNewMS name m n mskeys = do
    acc <- dbNewMS name m n mskeys
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        dbSetGap name 30 False
        dbSetGap name 30 True
    return $ yamlAcc acc

cmdAddKeys :: (PersistStore m, PersistUnique m, PersistQuery m)
           => AccountName -> [XPubKey] -> EitherT String m Value
cmdAddKeys name keys = do
    acc <- dbAddKeys name keys
    let n = fromJust $ dbAccountMsTotal acc
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        dbSetGap name 30 False
        dbSetGap name 30 True
    return $ yamlAcc acc

