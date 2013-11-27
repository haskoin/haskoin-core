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
, dbGetWallet
, liftEither
, liftMaybe
, migrateAll

-- Account functions
, cmdNewAcc
, cmdNewMS
, cmdAddKeys
, cmdAccInfo
, cmdListAcc
, cmdDumpKeys
, yamlAcc
, isMSAcc

-- Address functions
, cmdList
, cmdGenAddr
, cmdGenWithLabel
, cmdLabel
, dbGetAddr
, yamlAddr
, yamlAddrList

-- Coin functions
, cmdBalance
, cmdBalances
, cmdCoins
, cmdAllCoins
, yamlCoin

-- Store functions
, cmdInit
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
import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Store.DbAccount
import Haskoin.Wallet.Store.DbAddress
import Haskoin.Wallet.Store.DbCoin
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

cmdInit :: PersistUnique m => String -> EitherT String m Value
cmdInit seed 
    | null seed = left "cmdInit: seed can not be empty"
    | otherwise = do
        time   <- liftIO getCurrentTime
        master <- liftMaybe err $ makeMasterKey $ stringToBS seed
        let str = xPrvExport $ runMasterKey master
        insert_ $ DbWallet "main" "full" str (-1) time
        return Null
  where 
    err = "dbInit: Invalid master key generated from seed"

{-

dbGetSigData :: MonadResource m => Script -> OutPoint -> SigHash 
             -> WalletDB m (SigInput,PrvKey)
dbGetSigData out op sh = do
    master <- dbGetConfig cfgMaster
    a      <- liftEither $ scriptRecipient out
    addr   <- dbGetAddr $ AddrBase58 $ addrToBase58 a
    acc    <- dbGetAcc $ AccPos $ addrAccPos addr
    sigInp <- liftEither $ buildSigInput acc addr out op sh
    sigKey <- liftEither $ buildSigKey master addr
    return (sigInp,sigKey)

buildSigInput :: DBAccount -> DBAddress -> Script -> OutPoint -> SigHash
              -> Either String SigInput
buildSigInput acc addr out op sh
    | isMSAcc acc = do
        aks <- maybeToEither msg $ f key (msKeys acc) (addrIndex addr)
        let pks = map (xPubKey . runAddrPubKey) aks
            rdm = sortMulSig $ PayMulSig pks (msReq acc)
        return $ SigInputSH out op (encodeOutput rdm) sh
    | otherwise   = return $ SigInput out op sh
    where f   = if addrInt addr then intMulSigKey else extMulSigKey
          msg = "buildSigInput: Invalid derivation index"
          key = accKey $ runAccData acc
    
buildSigKey :: MasterKey -> DBAddress -> Either String PrvKey
buildSigKey master addr = do
    aKey   <- maybeToEither prvMsg $ accPrvKey master $ addrAccIndex addr
    sigKey <- maybeToEither keyMsg $ g aKey $ addrIndex addr
    return $ xPrvKey $ runAddrPrvKey sigKey
    where prvMsg = "buildPrvKey: Invalid account derivation index"
          keyMsg = "buildPrvKey: Invalid address derivation index"
          g      = if addrInt addr then intPrvKey else extPrvKey
-}

