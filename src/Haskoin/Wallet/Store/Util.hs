module Haskoin.Wallet.Store.Util
( WalletDB
, runWalletDB
, dbGet
, dbPut
, dbIter
, encodeInt
, decodeInt
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Resource

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Util

-- Track the database handle in a ReaderT monad
type WalletDB m a = ReaderT DB.DB m a

runWalletDB :: MonadResource m => FilePath -> WalletDB m a -> m a
runWalletDB fp wm = do
    db <- DB.open (fp ++ "/dbwallet") dbOptions 
    runReaderT wm db
    where dbOptions = DB.defaultOptions { DB.createIfMissing = True
                                        , DB.cacheSize = 2048
                                        }

dbGet :: MonadResource m => String -> WalletDB m (Maybe BS.ByteString)
dbGet key = ask >>= \db -> lift $ 
    DB.get db DB.defaultReadOptions $ stringToBS key

dbPut :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPut key value = ask >>= \db -> lift $
    DB.put db DB.defaultWriteOptions (stringToBS key) value

dbIter :: MonadResource m => String -> String -> Int 
       -> WalletDB m [BS.ByteString]
dbIter start prefix count = ask >>= \db -> lift $ 
    DB.withIterator db DB.defaultReadOptions $ \iter -> do
        DB.iterSeek iter $ stringToBS start
        go iter count
    where go iter c 
            | c > 0 = DB.iterKey iter >>= \keyM -> case keyM of
                Just key | isPrefixOf prefix $ bsToString key -> do
                            val <- fromJust <$> DB.iterValue iter
                            DB.iterNext iter
                            liftM2 (:) (return val) (go iter $ c - 1)
                         | otherwise -> return []
                _ -> return []
            | otherwise = return []

encodeInt :: Int -> String
encodeInt i | i < 0 = error "encodeInt: negative Int are invalid"
            | otherwise = bsToString $ runPut' $ putWord32le $ fromIntegral i

decodeInt :: BS.ByteString -> Maybe Int
decodeInt bs = fromRunGet getWord32le bs Nothing (Just . fromIntegral)

