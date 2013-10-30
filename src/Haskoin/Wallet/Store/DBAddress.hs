module Haskoin.Wallet.Store.DBAddress
( DBAddress(..)
, AddressKey(..)
, getAddr
, putAddr
, genAddr
, listAddr
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

data DBAddress = DBAddress 
    { addrBase58   :: String
    , addrLabel    :: String
    , addrIndex    :: Word32
    , addrPos      :: Int
    , addrAccIndex :: Word32
    , addrAccPos   :: Int
    , addrInt      :: Bool
    } deriving (Eq, Show)

data AddressKey = AddrBase58 { addrKeyBase58 :: String }
                | AddrExt { addrKeyAccPos :: Int 
                          , addrKeyPos    :: Int
                          }
                | AddrInt { addrKeyAccPos :: Int 
                          , addrKeyPos    :: Int
                          }
                deriving (Eq, Show)

getAddr :: MonadResource m => AddressKey -> WalletDB m (Maybe DBAddress)
getAddr key = case key of
    AddrBase58 addr -> do
        bsM <- dbGet $ "addrbase58_" ++ addr
        maybe (return Nothing) ((f <$>) . dbGet . bsToString) bsM
    _ -> f <$> (dbGet $ prefix ++ accPos ++ "_" ++ addrPos)
    where prefix  = case key of AddrExt _ _ -> "addrext_"
                                AddrInt _ _ -> "addrint_"
          accPos  = encodeInt $ addrKeyAccPos key
          addrPos = encodeInt $ addrKeyPos key
          f       = (decodeToMaybe =<<)

putAddr :: MonadResource m => DBAddress -> WalletDB m ()
putAddr addr = do
    dbPut key $ encode' addr
    dbPut ("addrbase58_" ++ addrBase58 addr) $ stringToBS key
    where prefix  = if addrInt addr then "addrint_" else "addrext_"
          accp  = encodeInt $ addrAccPos addr
          addrp = encodeInt $ addrPos addr
          key   = prefix ++ accp ++ "_" ++ addrp

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

genAddr :: MonadResource m => Int -> Int -> Bool -> WalletDB m [DBAddress]
genAddr pos count int = getAcc (AccPos pos) >>= \accM -> case accM of
    Nothing  -> error $ "Invalid account index: " ++ (show pos)
    Just acc -> do
        let addrs  = map (buildAddr acc int) $ zip (take count $ f acc) [1..]
            accDat = runAccData acc
            intDat = accDat{ accIntIndex = addrIndex $ last addrs
                           , accIntCount = accIntCount accDat + count
                           }
            extDat = accDat{ accExtIndex = addrIndex $ last addrs
                           , accExtCount = accExtCount accDat + count
                           }
        putAcc acc{ runAccData = if int then intDat else extDat }
        forM_ addrs putAddr
        return addrs
    where f (DBAccount d)
              | int       = intAddrs (accKey d) (accIntIndex d + 1)
              | otherwise = extAddrs (accKey d) (accExtIndex d + 1)
          f (DBAccountMS d mk r _)
              | int       = intMulSigAddrs (accKey d) mk r (accIntIndex d + 1)
              | otherwise = extMulSigAddrs (accKey d) mk r (accExtIndex d + 1)

listAddr :: MonadResource m => Int -> Int -> Int -> Bool 
         -> WalletDB m [DBAddress]
listAddr pos from count int
    | from  < 0 = error $ "Invalid from: " ++ (show from)
    | count < 0 = error $ "Invalid count: " ++ (show count)
    | otherwise = getAcc (AccPos pos) >>= \accM -> case accM of
        Nothing -> error $ "Invalid account index: " ++ (show pos)
        Just acc -> do
            vals <- dbIter key prefix count
            return $ catMaybes $ map decodeToMaybe vals
    where start  = if int then "addrint_" else "addrext_" 
          prefix = start ++ (encodeInt pos) ++ "_"
          key    = prefix ++ (encodeInt from)

{- Binary Instance -}

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

