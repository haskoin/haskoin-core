module Haskoin.Wallet.Store where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Applicative

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Util

dbOptions = DB.defaultOptions
    { DB.createIfMissing = True
    , DB.cacheSize = 2048
    }

-- Track the database handle in a ReaderT monad
type WalletDB m a = ReaderT DB.DB m a

runWalletDB :: MonadResource m => FilePath -> WalletDB m a -> m a
runWalletDB fp wm = DB.open fp dbOptions >>= (runReaderT wm)

--format = {
--    version => 1
--    gap => 5
--    masterxprv => enc(xprv...)
--    addressbook => { name => address },
--    accounts => {
--        accname => {
--            index => 0
--            pubkey => xpub...
--            type => '2of3 protected wallet'
--            service => 'Haskoin.org'
--            multisig => [xpub1,xpub2...]
--            required => 2
--            address => [
--                addr1 => {
--                    label => 'name'
--                    index => 3
--                }
--            ]
--        }
--    }
--}

dbGet :: MonadResource m => String -> WalletDB m BS.ByteString
dbGet key = ask >>= \h -> do
    value <- lift $ DB.get h DB.defaultReadOptions $ stringToBS key
    return $ fromMaybe (error $ "Key not found in database: " ++ key) value

dbPut :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPut key value = ask >>= \h -> do
    lift $ DB.put h DB.defaultWriteOptions (stringToBS key) value

dbGetVersion :: MonadResource m => WalletDB m Int
dbGetVersion = fromIntegral . (runGet' getWord32le) <$> dbGet "version"

dbGetGap :: MonadResource m => WalletDB m Int
dbGetGap = fromIntegral . (runGet' getWord32le) <$> dbGet "gap"

dbPutGap :: MonadResource m => Int -> WalletDB m ()
dbPutGap g | g < 1 = error "Address gab must be >= 1"
           | otherwise = dbPut "gap" $ runPut' $ putWord32le $ fromIntegral g

dbGetMaster :: MonadResource m => WalletDB m MasterKey
dbGetMaster = fromJust . makeMasterKey <$> dbGet "seed"

-- Todo: encrypt this
dbPutSeed :: MonadResource m => String -> WalletDB m ()
dbPutSeed seed = dbPut "seed" $ runPut' $ putByteString $ stringToBS seed

