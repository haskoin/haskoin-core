module Haskoin.Wallet.Store.DBAccount
( DBAccount(..)
, AccountData(..)
, AccKey(..)
, getAcc
, putAcc
, newAcc
, newMSAcc
, listAccs
, isMSAcc
)
where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.DBConfig
import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Keys
import Haskoin.Protocol
import Haskoin.Util

data AccountData = 
    AccountData
        { accName      :: String
        , accIndex     :: Word32
        , accPos       :: Int
        , accKey       :: AccPubKey
        , accExtIndex  :: Word32
        , accExtCount  :: Int
        , accIntIndex  :: Word32
        , accIntCount  :: Int
        , accCoinCount :: Int
        } 
    deriving (Eq, Show)

data DBAccount = 
    DBAccountMS
        { runAccData :: AccountData
        , msKeys     :: [XPubKey]
        , msReq      :: Int
        , msUrl      :: String
        } 
  | DBAccount
        { runAccData :: AccountData }
    deriving (Eq, Show)

isMSAcc :: DBAccount -> Bool
isMSAcc (DBAccount _) = False
isMSAcc _           = True

data AccKey = AccName String | AccPos Int 
    deriving (Eq, Show)

getAcc :: MonadResource m => AccKey -> WalletDB m (Maybe DBAccount)
getAcc key = case key of
    AccPos i     -> f <$> (dbGet $ "acc_" ++ encodeInt i)
    AccName name -> (dbGet $ "accname_" ++ name) >>= \bsM ->
        maybe (return Nothing) ((f <$>) . dbGet . bsToString) bsM
    where f = (decodeToMaybe =<<)

putAcc :: MonadResource m => DBAccount -> WalletDB m ()
putAcc acc = do
    dbPut key $ encode' acc
    dbPut ("accname_" ++ name) $ stringToBS key
    where pos  = encodeInt $ accPos $ runAccData acc
          key  = "acc_" ++ pos
          name = accName $ runAccData acc

buildAccData :: String -> Word32 -> Int -> AccPubKey -> AccountData
buildAccData name index pos key = 
    AccountData 
        { accName      = name
        , accIndex     = index 
        , accPos       = pos
        , accKey       = key
        , accExtIndex  = maxBound
        , accExtCount  = 0
        , accIntIndex  = maxBound
        , accIntCount  = 0
        , accCoinCount = 0
        }

newAcc :: MonadResource m => String -> WalletDB m DBAccount
newAcc name = getAcc (AccName name) >>= \prev -> case prev of
    Just _ -> error $ "Account already exists: " ++ name
    Nothing -> do
        master <- (fromMaybe (error "No seed")) <$> getConfig cfgMaster
        index  <- (fromMaybe 0) . ((+1) <$>)    <$> getConfig cfgAccIndex
        count  <- (fromMaybe 0)                 <$> getConfig cfgAccCount
        let (k,i) = head $ accPubKeys master index
            acc   = DBAccount $ buildAccData name i (count+1) k
        putAcc acc
        putConfig $ \cfg -> cfg{ cfgAccIndex = i 
                               , cfgAccCount = count+1
                               , cfgFocus    = count+1
                               }
        return acc

newMSAcc :: MonadResource m => String -> Int -> [XPubKey] 
         -> WalletDB m DBAccount
newMSAcc name r mskeys = getAcc (AccName name) >>= \prev -> case prev of
    Just _  -> error $ "Account already exists: " ++ name
    Nothing -> do
        master <- (fromMaybe (error "No seed")) <$> getConfig cfgMaster
        index  <- (fromMaybe 0) . ((+1) <$>)    <$> getConfig cfgAccIndex
        count  <- (fromMaybe 0)                 <$> getConfig cfgAccCount
        let (k,i)   = head $ accPubKeys master index
            accData = buildAccData name i (count+1) k
            acc     = DBAccountMS accData mskeys r ""
        putAcc acc
        putConfig $ \cfg -> cfg{ cfgAccIndex = i 
                               , cfgAccCount = count+1
                               , cfgFocus    = count+1
                               }
        return acc

listAccs :: MonadResource m => WalletDB m [DBAccount]
listAccs = do
    count <- (fromMaybe 0) <$> getConfig cfgAccCount
    vals  <- dbIter ("acc_" ++ encodeInt 1) "acc_" count
    return $ catMaybes $ map decodeToMaybe vals

{- Binary Instances -}

instance Binary AccountData where

    get = AccountData <$> (bsToString . getVarString <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (AccPubKey <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (fromIntegral . getVarInt <$> get)
                      <*> (fromIntegral . getVarInt <$> get)

    put (AccountData n i p k ei ec ii ic cc) = do
        put $ VarString $ stringToBS n
        put $ VarInt $ fromIntegral i
        put $ VarInt $ fromIntegral p
        put $ runAccPubKey k
        put $ VarInt $ fromIntegral ei
        put $ VarInt $ fromIntegral ec
        put $ VarInt $ fromIntegral ii
        put $ VarInt $ fromIntegral ic
        put $ VarInt $ fromIntegral cc

instance Binary DBAccount where

    get = getWord8 >>= \t -> case t of
        0 -> DBAccount <$> get
        1 -> DBAccountMS <$> get
                         <*> (go =<< get) 
                         <*> (fromIntegral . getVarInt <$> get)
                         <*> (bsToString . getVarString <$> get)
        _ -> fail $ "Invalid account type: " ++ (show t)
        where go (VarInt len) = replicateM (fromIntegral len) get

    put acc = case acc of
        DBAccount d              -> putWord8 0 >> put d
        DBAccountMS d keys r u -> putWord8 1 >> put d >> do
            put $ VarInt $ fromIntegral $ length keys
            forM_ keys put
            put $ VarInt $ fromIntegral r
            put $ VarString $ stringToBS u
        

