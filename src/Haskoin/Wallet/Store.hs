module Haskoin.Wallet.Store 
-- Account
( DBAccount(..)
, AccountData(..)
, AccKey(..)
, getAcc
, putAcc
, newAcc
, newMSAcc
, listAccs
, isMSAcc

-- Address
, DBAddress(..)
, AddressKey(..)
, getAddr
, putAddr
, genAddr
, listAddr

-- Coin
, DBCoin(..)
, CoinKey(..)
, getCoin
, putCoin
, importTx
, listCoins
, listAllCoins

-- Config
, DBConfig(..)
, initConfig
, getConfig
, putConfig

-- Util
, WalletDB
, runWalletDB
, dbGet
, dbPut
, dbIter
, encodeInt
, decodeInt

-- Store
, dbInit
, signData
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

dbInit :: MonadResource m => String -> WalletDB m ()
dbInit seed = do
    let masterKey = fromJust $ makeMasterKey $ stringToBS seed
    initConfig $ DBConfig { cfgMaster    = masterKey
                          , cfgVersion   = 1
                          , cfgAccIndex  = maxBound
                          , cfgAccCount  = 0
                          , cfgFocus     = 0
                          , cfgCoinCount = 0
                          }
    newAcc "default" >> return ()

{- Signing functions -}

signData :: MonadResource m => Script -> OutPoint -> SigHash 
         -> WalletDB m (Maybe (SigInput,PrvKey))
signData out op sh = do
    masterM <- getConfig cfgMaster
    addrM   <- case decodeOutput out of
        Right (PayPKHash a)     -> getAddr $ AddrBase58 $ addrToBase58 a
        Right (PayScriptHash a) -> getAddr $ AddrBase58 $ addrToBase58 a
        _                       -> return Nothing
    case addrM of
        Just addr -> getAcc (AccPos $ addrAccPos addr) >>= \accM -> return $ do
            acc <- accM
            mst <- masterM
            buildSignData out op sh mst addr acc
        _ -> return Nothing

buildSignData :: Script -> OutPoint -> SigHash -> MasterKey -> DBAddress 
              -> DBAccount -> Maybe (SigInput,PrvKey)
buildSignData out op sh master addr acc = do
    sgi <- if isMSAcc acc 
        then do
            aks <- fms (accKey dat) (msKeys acc) (addrIndex addr)
            let pks = map (xPubKey . runAddrPubKey) aks
                rdm = sortMulSig $ PayMulSig pks (msReq acc)
            return $ SigInputSH out op (encodeOutput rdm) sh
        else return $ SigInput out op sh
    accPrvKey <- accPrvKey master $ addrAccIndex addr
    sigKey    <- g accPrvKey $ addrIndex addr
    return (sgi, xPrvKey $ runAddrPrvKey $ sigKey)
    where fms = if addrInt addr then intMulSigKey else extMulSigKey
          g   = if addrInt addr then intPrvKey else extPrvKey
          dat = runAccData acc
    

