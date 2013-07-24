{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Bitcoin.Store.LevelDB 
( LevelDB(..)
, AppDB
) where

import Data.Default

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex
import Bitcoin.Store
import Bitcoin.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Applicative

import qualified Data.Conduit as C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Database.LevelDB as DB

type AppDB = LevelDB

data LevelDB a = 
    LevelDB { runLevelDB :: ReaderT DB.DB (ResourceT IO) a }

instance Monad LevelDB where

    a >>= f = LevelDB $ do
        val <- runLevelDB a
        runLevelDB $ f val

    return = LevelDB . return

instance Functor LevelDB where
    fmap f m = liftM f m

instance MonadIO LevelDB where
    liftIO = LevelDB . liftIO

liftDB :: ResourceT IO a -> LevelDB a
liftDB = LevelDB . lift

liftDB' :: ResourceT IO a -> C.ConduitM () BlockIndex LevelDB a
liftDB' = lift . liftDB

getHandle :: LevelDB DB.DB
getHandle = LevelDB ask

dbOptions = DB.defaultOptions
    { DB.createIfMissing = True
    , DB.cacheSize = 2048
    }

instance Store BlockIndex LevelDB where

    dbKey = return . biHash

    dbGet w = do
        db <- getHandle
        let key = bsToBSC . toStrictBS . runPut . putWord256be $ w
        val <- liftDB $ DB.get db def key
        return $ val >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

    dbPut bi = do
        db <- getHandle
        let key = bsToBSC . toStrictBS . runPut . putWord256be $ biHash bi
        let payload = toStrictBS . runPut . bitcoinPut $ bi
        liftDB $ DB.put db def key (bsToBSC payload)

    -- Todo: Save the best block index in LevelDB
    dbBest = return (Nothing :: Maybe BlockIndex)

    dbPrev bi = do
        db <- getHandle
        let pKey = bsToBSC . toStrictBS . runPut . putWord256be $ (biPrev bi)
        prev <- liftDB $ DB.get db def pKey
        return $ prev >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

    dbPrevKey = return . biPrev

    dbStream = do
        db <- lift getHandle 
        (releaseSnap, snap) <- liftDB' $ DB.createSnapshot' db
        let readOptions = def{DB.useSnapshot = Just snap}
        (releaseIter, iter) <- liftDB' $ DB.iterOpen' db  readOptions
        liftDB' $ DB.iterFirst iter
        streamItems iter
        liftDB' $ do
            release releaseIter
            release releaseSnap
        return ()

streamItems :: DB.Iterator -> C.Source LevelDB BlockIndex
streamItems iter = do
    val <- liftDB' $ DB.iterValue iter
    case val of
        (Just bs) -> do
            let bi = runGet bitcoinGet (toLazyBS $ bscToBS $ bs)
            C.yield bi
            liftDB' $ DB.iterNext iter
            streamItems iter
        Nothing -> return ()

instance IndexStore LevelDB where
    delIndex w = do
        db <- getHandle
        let key = bsToBSC . toStrictBS . runPut . putWord256be $ w
        liftDB $ DB.delete db def key


instance AppStore LevelDB where
    runAppDB m = runResourceT $ do
        handle <- DB.open "blockindex" dbOptions
        runReaderT (runLevelDB m) handle

