-- |Configuration module for wallet database
module Haskoin.Wallet.Store.DBConfig
( DBConfig(..)
, dbInitConfig
, dbGetConfig
, dbPutConfig
) where

import Control.Applicative
import Control.Monad.Trans.Resource

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Manager
import Haskoin.Wallet.Store.Util
import Haskoin.Protocol
import Haskoin.Util

-- |Data type for holding wallet configuration options
data DBConfig = DBConfig { cfgMaster    :: MasterKey
                         , cfgVersion   :: Int
                         , cfgAccIndex  :: Word32
                         , cfgAccCount  :: Int
                         , cfgFocus     :: Int
                         , cfgCoinCount :: Int
                         } deriving (Eq, Show)

-- |Initialize the database with an initial configuration
dbInitConfig :: MonadResource m => DBConfig -> WalletDB m ()
dbInitConfig cfg = go =<< dbExists "config"
    where go True  = liftEither $ Left "Configuration already initialized"
          go False = dbPut "config" $ encode' cfg

-- |Retrieve one value of the configuration type
dbGetConfig :: MonadResource m => (DBConfig -> a) -> WalletDB m a
dbGetConfig f = f <$> (liftEither . decodeToEither =<< (dbGet "config"))

-- |Update one value of the configuration type
dbPutConfig :: MonadResource m => (DBConfig -> DBConfig) -> WalletDB m ()
dbPutConfig f = go =<< (liftEither . decodeToEither =<< (dbGet "config"))
    where go cfg = dbPut "config" $ encode' $ f cfg

-- |Binary instance for the configuration type
instance Binary DBConfig where

    get = DBConfig <$> (get >>= \key -> f $ loadMasterKey key)
                   <*> (fromIntegral . getVarInt <$> get)
                   <*> getWord32le
                   <*> (fromIntegral . getVarInt <$> get)
                   <*> (fromIntegral . getVarInt <$> get)
                   <*> (fromIntegral . getVarInt <$> get)
          where f = maybe (fail "DBConfig get: Invalid master key") return

    put (DBConfig m v i c f cc) = do
        put $ runMasterKey m
        put $ VarInt $ fromIntegral v
        putWord32le i
        put $ VarInt $ fromIntegral c
        put $ VarInt $ fromIntegral f
        put $ VarInt $ fromIntegral cc

