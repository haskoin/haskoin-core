-- |Provides utilities for wallet-related database computations
module Haskoin.Wallet.Store.Util
( WalletDB
, runWalletDB
, dbExists
, dbGet
, dbPut
, dbIter
, liftDB
, liftEither
, liftMaybe
, dbEncodeInt
, dbDecodeInt
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Util

-- |Monad for performing wallet-related database computations
type WalletDB m a = EitherT String (ReaderT DB.DB m) a

-- |Lift a database computation to the WalletDB monad
liftDB :: MonadResource m => m a -> WalletDB m a
liftDB = lift . lift

-- |Lift an (Either String) monad to the WalletDB monad
liftEither :: MonadResource m => Either String a -> WalletDB m a
liftEither = hoistEither

-- |Lift a Maybe monad to the WalletDB monad
liftMaybe :: MonadResource m => String -> Maybe a -> WalletDB m a
liftMaybe err = liftEither . (maybeToEither err)

-- |Run a WalletDB monad by providing the database filepath
runWalletDB :: MonadResource m => FilePath -> WalletDB m a 
            -> m (Either String a)
runWalletDB fp wm = do
    db <- DB.open fp dbOptions 
    runReaderT (runEitherT wm) db
    where dbOptions = DB.defaultOptions { DB.createIfMissing = True
                                        , DB.cacheSize = 2048
                                        }

dbExists :: MonadResource m => String -> WalletDB m Bool
dbExists key = (lift ask) >>= \db -> 
    isJust <$> (liftDB $ DB.get db DB.defaultReadOptions $ stringToBS key)

-- |Get a database value from a key
dbGet :: MonadResource m => String -> WalletDB m BS.ByteString
dbGet key = (lift ask) >>= \db -> do
    resM <- liftDB $ DB.get db DB.defaultReadOptions $ stringToBS key
    liftMaybe ("dbGet: no value for key " ++ key) resM

-- |Put a key/value pair into the database
dbPut :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPut key value = (lift ask) >>= \db -> 
    liftDB $ DB.put db DB.defaultWriteOptions (stringToBS key) value

-- |Iterate through a number of keys from the database
-- in lexicographical order
dbIter :: MonadResource m => String -> String -> Int 
       -> WalletDB m [BS.ByteString]
dbIter start prefix count 
    | count < 0 = liftEither $ Left $ "dbIter: 'count' can not be negative"
    | otherwise = (lift ask) >>= \db -> do
        res <- liftDB $ DB.withIterator db DB.defaultReadOptions $ \iter -> do
            DB.iterSeek iter $ stringToBS start
            dbIterLoop iter prefix count
        -- Sequence the list of Either values and lift to WalletDB
        liftEither $ sequence res

dbIterLoop :: MonadResource m => DB.Iterator -> String -> Int 
           -> m [Either String BS.ByteString]
dbIterLoop iter prefix c
    | c > 0 = DB.iterKey iter >>= \keyM -> case keyM of
        Just key -> if isPrefixOf prefix $ bsToString key
            then DB.iterValue iter >>= \valM -> case valM of
                Just val -> do
                    DB.iterNext iter
                    liftM2 (:) (return $ Right val) 
                               (dbIterLoop iter prefix $ c - 1)
                Nothing -> return [Left valErr]
            else return []
        Nothing  -> return [Left keyErr]
    | otherwise = return []
    where keyErr = "dbIterLoop: Invalid iterator key"
          valErr = "dbIterLoop: Invalid iterator value"

-- |Encode an Int into a fixed-length 4 byte representation
-- for preserving lexicographical ordering in database keys
dbEncodeInt :: Int -> Either String String
dbEncodeInt i | i < 0 = Left "encodeInt: Invalid negative Int"
              | fromIntegral i > (maxBound :: Word32) =
                  Left "encodeInt: Int too big"
              | otherwise = return $ bsToString bs
              where bs = runPut' $ putWord32le $ fromIntegral i

-- |Decode a fixed-length 4 byte bytestring into an Int
dbDecodeInt :: BS.ByteString -> Either String Int
dbDecodeInt bs 
    | BS.length bs /= 4 = Left "decodeInt: invalid bytestring size"
    | otherwise = fromRunGet getWord32le bs err (Right . fromIntegral)
    where err = Left "decodeInt: could not decode bytestring as Word32le"

