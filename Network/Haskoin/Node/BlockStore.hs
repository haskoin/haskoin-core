{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Node.BlockStore
( DbBlockHeaderGeneric(..)
, DbBlockConfigGeneric(..)
, DbOrphanHeaderGeneric(..)
, DbChainHeightGeneric(..)
, DbBlockHeaderId
, DbBlockConfigId
, DbOrphanHeaderId
, Unique(..)
, EntityField(..)
, dbInit
, dbPutHeader
, dbGetHeader
, dbGetOrphan
, dbPutOrphan
, dbSetChainHead
, dbGetChainHead
, dbPutConfig
, dbGetConfig
, migrateBlockStore
) where

import Control.Applicative ((<$>))
import Control.Monad (when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, isNothing, fromJust)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , EntityField
    , Unique
    , getBy
    , insert_
    , selectFirst
    , updateWhere
    , replace
    , (=.)
    )
import Database.Persist.TH 
    ( mkPersist
    , sqlSettings
    , mkMigrate
    , persistLowerCase
    , share
    )

import Network.Haskoin.Node.Util
import Network.Haskoin.Wallet ()
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

share [mkPersist sqlSettings, mkMigrate "migrateBlockStore"] [persistLowerCase|
DbBlockHeader 
    hash Hash256
    parent Hash256 
    height Int
    chainWork Integer
    txCount Int
    value BlockHeader
    created UTCTime default=CURRENT_TIME
    BlockHash hash
    deriving Show
    deriving Eq

DbChainHeight
    hash Hash256
    created UTCTime default=CURRENT_TIME
    deriving Show
    deriving Eq

DbOrphanHeader 
    hash Hash256
    parent Hash256 
    value BlockHeader
    created UTCTime default=CURRENT_TIME
    OrphanHash hash
    deriving Show
    deriving Eq

DbBlockConfig 
    key String
    value String
    created UTCTime default=CURRENT_TIME
    UniqueKey key
    deriving Show
    deriving Eq
|]

-- | Initialise the BlockStore. Returns True if the database was already
-- initialized, False otherwise.
dbInit :: PersistUnique m => m Bool
dbInit = do
    isInit <- dbConfigExists "version"
    if isInit
        then do
            magicM <- dbGetConfig "networkmagic"
            verM   <- dbGetConfig "version"
            let m = fromJust magicM
                v = fromJust verM
            when (isNothing magicM) $ liftIO $ throwIO $ 
                BlockStoreException 
                    "The networkmagic field is not in the database"
            when (isNothing verM) $ liftIO $ throwIO $ 
                BlockStoreException 
                    "The database version number is not set"
            when (m /= magic) $ liftIO $ throwIO $
                BlockStoreException 
                    "The database was initialized for another network"
            when (v /= "0.0.1") $ liftIO $ throwIO $
                BlockStoreException "The database version is incompatible"
        else do
            dbPutConfig "networkmagic" magic
            dbPutConfig "version" "0.0.1"
    return isInit
  where
    magic = bsToHex $ encode' networkMagic

dbPutHeader :: (PersistUnique m, b ~ PersistMonadBackend m)
            => DbBlockHeaderGeneric b
            -> m (DbBlockHeaderGeneric b)
dbPutHeader bhg = do
    time <- liftIO getCurrentTime
    let res = bhg { dbBlockHeaderCreated = time }
    insert_ res >> return res

dbGetHeader :: (PersistUnique m, b ~ PersistMonadBackend m)
            => Hash256 -> m (Maybe (DbBlockHeaderGeneric b))
dbGetHeader hash = do
    headM <- getBy $ BlockHash hash
    return $ entityVal <$> headM

dbSetChainHead :: PersistQuery m => Hash256 -> m ()
dbSetChainHead hash = updateWhere [] [DbChainHeightHash =. hash]

dbGetChainHead :: (PersistQuery m, PersistUnique m, b ~ PersistMonadBackend m) 
               => m (DbBlockHeaderGeneric b)
dbGetChainHead = do
    hashM <- selectFirst [] []
    when (isNothing hashM) $ liftIO $ throwIO $
        BlockStoreException "There is no chain head in the database"
    headM <- dbGetHeader $ dbChainHeightHash $ entityVal $ fromJust hashM
    when (isNothing headM) $ liftIO $ throwIO $
        BlockStoreException "There is no chain head in the database"
    return $ fromJust headM

dbPutConfig :: PersistUnique m => String -> String -> m ()
dbPutConfig key val = do
    time   <- liftIO $ getCurrentTime
    cnfEnt <- getBy $ UniqueKey key
    let record = DbBlockConfig key val time
    case cnfEnt of
        Just (Entity k _) -> replace k record
        Nothing           -> insert_ record

dbGetConfig :: PersistUnique m => String -> m (Maybe String)
dbGetConfig key = do
    resM <- getBy $ UniqueKey key
    return $ (dbBlockConfigValue . entityVal) <$> resM

dbConfigExists :: PersistUnique m
               => String -> m Bool
dbConfigExists key = liftM isJust $ getBy (UniqueKey key)

dbPutOrphan :: (PersistUnique m, b ~ PersistMonadBackend m)
            => BlockHeader
            -> m (DbOrphanHeaderGeneric b)
dbPutOrphan block = do
    time <- liftIO getCurrentTime
    let res = DbOrphanHeader (blockid block) (prevBlock block) block time
    insert_ res >> return res

dbGetOrphan :: (PersistUnique m, b ~ PersistMonadBackend m)
            => Hash256
            -> m (Maybe (DbOrphanHeaderGeneric b))
dbGetOrphan hash = do
    orphM <- getBy $ OrphanHash hash
    return $ entityVal <$> orphM

