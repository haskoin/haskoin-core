module Haskoin.Wallet.Store 
-- Account
( DBAccount(..)
, AccountData(..)
, AccKey(..)
, dbGetAcc
, dbPutAcc
, dbNewAcc
, dbNewMSAcc
, dbAccList
, isMSAcc
, dbAccTree

-- Address
, DBAddress(..)
, AddressKey(..)
, dbGetAddr
, dbPutAddr
, dbGenAddr
, dbAddrList
, dbAddrTree

-- Coin
, DBCoin(..)
, CoinKey(..)
, dbGetCoin
, dbPutCoin
, dbImportTx
, dbCoinList
, dbCoinListAll

-- Config
, DBConfig(..)
, dbInitConfig
, dbGetConfig
, dbPutConfig

-- Util
, WalletDB
, runWalletDB

-- Store
, dbInit
, dbGetSigData
, dbExists
, liftEither
, liftMaybe
) where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Wallet.Store.DBConfig
import Haskoin.Wallet.Store.DBAccount
import Haskoin.Wallet.Store.DBAddress
import Haskoin.Wallet.Store.DBCoin
import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.TxBuilder
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

dbInit :: MonadResource m => String -> WalletDB m DBAccount
dbInit seed = do
    master <- liftMaybe msg $ makeMasterKey $ stringToBS seed
    dbInitConfig $ DBConfig { cfgMaster    = master
                            , cfgVersion   = 1
                            , cfgAccIndex  = maxBound
                            , cfgAccCount  = 0
                            , cfgFocus     = ""
                            , cfgCoinCount = 0
                            }
    dbNewAcc "default"
    where msg = "dbInit: Invalid master key generation from seed"

{- Signing functions -}

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

