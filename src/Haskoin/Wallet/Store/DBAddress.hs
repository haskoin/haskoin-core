-- |This module provides an address type for use in the wallet database
module Haskoin.Wallet.Store.DBAddress
( DBAddress(..)
, AddressKey(..)
, dbGetAddr
, dbPutAddr
, dbGenAddr
, dbAddrList
, dbAddrTree
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.DBAccount
import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Keys
import Haskoin.Protocol
import Haskoin.Util

-- |Address type used in the wallet database
data DBAddress = DBAddress 
    { addrBase58   :: String
    , addrLabel    :: String
    , addrIndex    :: Word32
    , addrPos      :: Int
    , addrAccIndex :: Word32
    , addrAccPos   :: Int
    , addrInt      :: Bool
    } deriving (Eq, Show)

-- |Keys used for querying addresses with dbGetAddr
-- Addresses can be queried by base58 address or position
data AddressKey = AddrBase58 { addrKeyBase58 :: String }
                | AddrExt { addrKeyAccPos :: Int 
                          , addrKeyPos    :: Int
                          }
                | AddrInt { addrKeyAccPos :: Int 
                          , addrKeyPos    :: Int
                          }
                deriving (Eq, Show)

dbAddrTree :: DBAddress -> String
dbAddrTree addr = concat 
    [ "m/"
    , show $ addrAccIndex addr
    , "'/"
    , if addrInt addr then "1/" else "0/"
    , show $ addrIndex addr
    ]

-- |Query an address from the database using an AddressKey query type
dbGetAddr :: MonadResource m => AddressKey -> WalletDB m DBAddress
dbGetAddr key = case key of
    AddrBase58 addr -> do
        f =<< dbGet . bsToString =<< (dbGet $ "addrbase58_" ++ addr)
    _ -> do
        acPos <- liftEither $ dbEncodeInt $ addrKeyAccPos key
        adPos <- liftEither $ dbEncodeInt $ addrKeyPos key
        f =<< (dbGet $ prefix ++ acPos ++ "_" ++ adPos)
    where f       = liftEither . decodeToEither
          prefix  = case key of AddrExt _ _ -> "addrext_"
                                AddrInt _ _ -> "addrint_"

-- |Save an address in the wallet database
dbPutAddr :: MonadResource m => DBAddress -> WalletDB m ()
dbPutAddr addr = do
    acPos <- liftEither $ dbEncodeInt $ addrAccPos addr
    adPos <- liftEither $ dbEncodeInt $ addrPos addr
    let key = prefix ++ acPos ++ "_" ++ adPos
    dbPut key $ encode' addr
    dbPut ("addrbase58_" ++ addrBase58 addr) $ stringToBS key
    where prefix  = if addrInt addr then "addrint_" else "addrext_"

-- |Generate new addresses and save them in the wallet database
dbGenAddr :: MonadResource m => Int -> Int -> Bool -> WalletDB m [DBAddress]
dbGenAddr pos count int = do
    acc <- dbGetAcc $ AccPos pos
    let addrs  = map (buildAddr acc int) $ zip (take count $ f acc) [1..]
    dbPutAcc acc{ runAccData = newData addrs $ runAccData acc }
    forM_ addrs dbPutAddr
    return addrs
    where newData addrs aData
              | int       = aData{ accIntIndex = addrIndex $ last addrs
                                 , accIntCount = accIntCount aData + count
                                 }
              | otherwise = aData{ accExtIndex = addrIndex $ last addrs
                                 , accExtCount = accExtCount aData + count
                                 }
          f (DBAccount d)
              | int       = intAddrs (accKey d) (accIntIndex d + 1)
              | otherwise = extAddrs (accKey d) (accExtIndex d + 1)
          f (DBAccountMS d mk r _ _)
              | int       = intMulSigAddrs (accKey d) mk r (accIntIndex d + 1)
              | otherwise = extMulSigAddrs (accKey d) mk r (accExtIndex d + 1)
    
buildAddr :: DBAccount -> Bool -> ((String,Word32),Int) -> DBAddress
buildAddr acc int ((s,i),n) = 
    DBAddress { addrBase58   = s
              , addrLabel    = ""
              , addrIndex    = i
              , addrPos      = f (runAccData acc) + n
              , addrAccIndex = accIndex $ runAccData acc
              , addrAccPos   = accPos $ runAccData acc
              , addrInt      = int
              }
    where f = if int then accIntCount else accExtCount

-- |List addresses in the wallet database
dbAddrList :: MonadResource m => Int -> Int -> Int -> Bool 
           -> WalletDB m [DBAddress]
dbAddrList pos from count int = do
    prefix <- (start ++) . (++ "_")  <$> (liftEither $ dbEncodeInt pos)
    key    <- (prefix ++) <$> (liftEither $ dbEncodeInt from)
    vals   <- dbIter key prefix count
    liftEither $ forM vals decodeToEither
    where start  = if int then "addrint_" else "addrext_" 

-- |Binary instance for Address type
instance Binary DBAddress where

    get = DBAddress <$> (bsToString . getVarString <$> get)
                    <*> (bsToString . getVarString <$> get)
                    <*> getWord32le
                    <*> (fromIntegral . getVarInt <$> get)
                    <*> getWord32le
                    <*> (fromIntegral . getVarInt <$> get)
                    <*> get

    put (DBAddress a l i p ai ap int) = do
        put $ VarString $ stringToBS a
        put $ VarString $ stringToBS l
        putWord32le i
        put $ VarInt $ fromIntegral p
        putWord32le ai
        put $ VarInt $ fromIntegral ap
        put int

