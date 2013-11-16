-- |This module provides an account type for the wallet database
module Haskoin.Wallet.Store.DBAccount
( DBAccount(..)
, AccountData(..)
, AccKey(..)
, dbGetAcc
, dbPutAcc
, dbNewAcc
, dbNewMSAcc
, dbAddMSKeys
, dbAccList
, isMSAcc
, dbAccTree
)
where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.DBConfig
import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Keys
import Haskoin.Protocol
import Haskoin.Util

-- |Account information shared by regular and multisignature accounts
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
        , accTxCount   :: Int
        } 
    deriving (Eq, Show)

-- |Account type used in the wallet database
data DBAccount = 
    DBAccountMS
        { runAccData :: AccountData
        , msKeys     :: [XPubKey]
        , msReq      :: Int
        , msTot      :: Int
        , msUrl      :: String
        } 
  | DBAccount
        { runAccData :: AccountData }
    deriving (Eq, Show)

-- |Test if the argument is a multisignature account
isMSAcc :: DBAccount -> Bool
isMSAcc (DBAccount _) = False
isMSAcc _           = True

-- |Key used for querying accounts with dbGetAcc. 
-- Accounts can be queried by name or by position
data AccKey = AccName String | AccPos Int 
    deriving (Eq, Show)

dbAccTree :: DBAccount -> String
dbAccTree acc = concat [ "m/", show $ accIndex $ runAccData acc, "'" ]

-- |Query an account from the database using an AccKey query type
dbGetAcc :: MonadResource m => AccKey -> WalletDB m DBAccount
dbGetAcc key = case key of
    AccPos i     -> f =<< dbGet . ("acc_" ++) =<< (liftEither $ dbEncodeInt i)
    AccName name -> f =<< dbGet . bsToString =<< (dbGet $ "accname_" ++ name)
    where f = liftEither . decodeToEither

-- |Store an account in the wallet database
dbPutAcc :: MonadResource m => DBAccount -> WalletDB m ()
dbPutAcc acc = do
    key <- ("acc_" ++) <$> (liftEither $ dbEncodeInt $ accPos aData)
    dbPut key $ encode' acc
    dbPut ("accname_" ++ accName aData) $ stringToBS key
    where aData = runAccData acc

-- |Create a new regular account from a name
dbNewAcc :: MonadResource m => String -> WalletDB m DBAccount
dbNewAcc name = dbExists nameKey >>= \exists -> if exists
    then liftEither $ Left $ 
        unwords ["newAcc: Account", name, "already exists"]
    else dbGetConfig cfgMaster >>= \master -> do
        index  <- (+1) <$> dbGetConfig cfgAccIndex
        count  <- (+1) <$> dbGetConfig cfgAccCount
        let (k,i) = head $ accPubKeys master index
            acc   = DBAccount $ buildAccData name i count k
        dbPutConfig $ \cfg -> cfg{ cfgAccIndex = i 
                                 , cfgAccCount = count
                                 , cfgFocus    = name
                                 }
        dbPutAcc acc >> return acc
    where nameKey = concat ["accname_", name]

-- |Create a new multisignature account from a name and public keys
dbNewMSAcc :: MonadResource m => String -> Int -> Int -> [XPubKey] 
           -> WalletDB m DBAccount
dbNewMSAcc name m n mskeys = dbExists nameKey >>= \exists -> if exists
    then liftEither $ Left $ 
        unwords ["newMSAcc: Account", name, "already exists"]
    else dbGetConfig cfgMaster >>= \master -> do
        let keys = nub mskeys
        unless (length keys < n) $ left "dbNewMSAcc: Too many keys"
        unless (n <= 16 && n >= 1 && m <= n && m >= 1) $ left
            "dbNewMSAcc: Invalid multisig parameters"
        index  <- (+1) <$> dbGetConfig cfgAccIndex
        count  <- (+1) <$> dbGetConfig cfgAccCount
        let (k,i)   = head $ accPubKeys master index
            acc     = DBAccountMS (buildAccData name i count k) keys m n ""
        dbPutConfig $ \cfg -> cfg{ cfgAccIndex = i 
                                 , cfgAccCount = count
                                 , cfgFocus    = name
                                 }
        dbPutAcc acc >> return acc
    where nameKey = concat ["accname_", name]

dbAddMSKeys :: MonadResource m => Int -> [XPubKey] -> WalletDB m DBAccount
dbAddMSKeys pos mskeys = dbGetAcc (AccPos pos) >>= \acc -> do
    unless (isMSAcc acc) $ left "dbAddMSKeys: Not a multisig account"
    accs <- dbAccList
    let myKeys = map (runAccPubKey . accKey . runAccData) accs
    unless (null $ mskeys `intersect` myKeys) $ left 
        "dbAddMSKeys: Can not build a multisignature account with your own keys"
    let newKeys = nub $ mskeys ++ (msKeys acc)
        newAcc  = acc{ msKeys = newKeys }
    unless (length newKeys < msTot acc) $ left "dbAddMSKeys: Too many keys"
    dbPutAcc newAcc >> return newAcc

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
        , accTxCount   = 0
        }

-- |List all the accounts in the wallet database
dbAccList :: MonadResource m => WalletDB m [DBAccount]
dbAccList = do
    count <- dbGetConfig cfgAccCount
    key   <- ("acc_" ++) <$> (liftEither $ dbEncodeInt 1)
    vals  <- dbIter key "acc_" count
    liftEither $ forM vals decodeToEither

-- |Binary instance for AccountData
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
                      <*> (fromIntegral . getVarInt <$> get)

    put (AccountData n i p k ei ec ii ic cc tc) = do
        put $ VarString $ stringToBS n
        put $ VarInt $ fromIntegral i
        put $ VarInt $ fromIntegral p
        put $ runAccPubKey k
        put $ VarInt $ fromIntegral ei
        put $ VarInt $ fromIntegral ec
        put $ VarInt $ fromIntegral ii
        put $ VarInt $ fromIntegral ic
        put $ VarInt $ fromIntegral cc
        put $ VarInt $ fromIntegral tc

-- |Binary instance for Account
instance Binary DBAccount where

    get = getWord8 >>= \t -> case t of
        0 -> DBAccount <$> get
        1 -> DBAccountMS <$> get
                         <*> (go =<< get) 
                         <*> (fromIntegral . getVarInt <$> get)
                         <*> (fromIntegral . getVarInt <$> get)
                         <*> (bsToString . getVarString <$> get)
        _ -> fail $ "Invalid account type: " ++ (show t)
        where go (VarInt len) = replicateM (fromIntegral len) get

    put acc = case acc of
        DBAccount d              -> putWord8 0 >> put d
        DBAccountMS d keys m n u -> putWord8 1 >> put d >> do
            put $ VarInt $ fromIntegral $ length keys
            forM_ keys put
            put $ VarInt $ fromIntegral m
            put $ VarInt $ fromIntegral n
            put $ VarString $ stringToBS u
        

