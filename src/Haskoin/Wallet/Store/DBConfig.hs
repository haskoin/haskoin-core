module Haskoin.Wallet.Store.DBConfig
( DBConfig(..)
, initConfig
, getConfig
, putConfig
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

data DBConfig = DBConfig { cfgMaster   :: MasterKey
                         , cfgVersion  :: Int
                         , cfgAccIndex :: Word32
                         , cfgAccCount :: Int
                         , cfgFocus    :: Int
                         } deriving (Eq, Show)

initConfig :: MonadResource m => DBConfig -> WalletDB m ()
initConfig cfg = dbGet "config" >>= \cfgM -> case cfgM of
    Just _  -> error "Database already initialized"
    Nothing -> dbPut "config" $ encode' cfg

getConfig :: MonadResource m => (DBConfig -> a) -> WalletDB m (Maybe a)
getConfig f = dbGet "config" >>= \cfg -> return $
    f <$> (decodeToMaybe =<< cfg)

putConfig :: MonadResource m => (DBConfig -> DBConfig) -> WalletDB m ()
putConfig f = dbGet "config" >>= \cfgM -> case decodeToMaybe =<< cfgM of
    Just cfg -> dbPut "config" $ encode' $ f cfg
    _ -> return ()

instance Binary DBConfig where

    get = DBConfig <$> (get >>= \key -> f $ loadMasterKey key)
                   <*> (fromIntegral . getVarInt <$> get)
                   <*> getWord32le
                   <*> (fromIntegral . getVarInt <$> get)
                   <*> (fromIntegral . getVarInt <$> get)
          where f = maybe (fail "DBConfig get: Invalid master key") return

    put (DBConfig m v i c f) = do
        put $ runMasterKey m
        put $ VarInt $ fromIntegral v
        putWord32le i
        put $ VarInt $ fromIntegral c
        put $ VarInt $ fromIntegral f

