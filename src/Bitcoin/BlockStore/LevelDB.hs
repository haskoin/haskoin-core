module Bitcoin.BlockStore.LevelDB
( LevelDB
, DefaultDB
) where

import Data.Default

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex
import Bitcoin.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Database.LevelDB as DB

import Bitcoin.BlockStore

type DefaultDB = LevelDB

newtype LevelDB a = 
    LevelDB { runLevelDB :: (ReaderT DB.DB (ResourceT IO)) a }

instance Monad LevelDB where
    a >>= f = LevelDB $ do
        val <- runLevelDB a
        runLevelDB $ f val
    return = LevelDB . return

instance MonadIO LevelDB where
    liftIO = LevelDB . liftIO

liftDB :: ResourceT IO a -> LevelDB a
liftDB = LevelDB . lift

liftAsk :: LevelDB DB.DB
liftAsk = LevelDB ask

dbOptions = DB.defaultOptions
    { DB.createIfMissing = True
    , DB.cacheSize = 2048
    }

instance BlockStore LevelDB where

    blockStoreGet w = do
        db <- liftAsk
        let key = toStrictBS . runPut . putWord256be $ w
        val <- liftDB $ DB.get db def (bsToBSC key)
        return $ 
            val >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

    blockStorePut bi = do
        db <- liftAsk
        let key = toStrictBS . runPut . putWord256be $ biHash bi
        let payload = toStrictBS . runPut . bitcoinPut $ bi
        liftDB $ DB.put db def (bsToBSC key) (bsToBSC payload)

    runDB m = runResourceT $ do
        handle <- DB.open "blockindex" dbOptions
        runReaderT (runLevelDB m) handle

{-
initBitcoinDB :: BitcoinDB ()
initBitcoinDB = do
    val <- lookupBlockIndexDB testGenesisBlockHash
    case val of
        (Just _) -> tell "LevelDB already initialized"
        Nothing  -> do
            tell "Initializing LevelDB with genesis block"
            putBlockIndexDB $ buildBlockIndex testGenesisBlock Nothing

lookupBlockIndexDB :: Word256 -> BitcoinDB (Maybe BlockIndex)
lookupBlockIndexDB w = do
    db <- getDBHandle
    let key = toStrictBS . runPut . putWord256be $ w
    val <- liftDB $ DB.get db def (bsToBSC key)
    return $ val >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

putBlockIndexDB :: BlockIndex -> BitcoinDB ()
putBlockIndexDB bi = do
    db <- getDBHandle
    let key = toStrictBS . runPut . putWord256be $ biHash bi
    let payload = toStrictBS . runPut . bitcoinPut $ bi
    liftDB $ DB.put db def (bsToBSC key) (bsToBSC payload)

getAllBlockIndices :: BitcoinDB [BlockIndex]
getAllBlockIndices = do
    db <- getDBHandle
    (releaseSnap, snap) <- DB.createSnapshot' db
    pairs <- DB.withIterator db def{DB.useSnapshot = Just snap} $ \iter -> do
        DB.iterFirst iter
        DB.iterItems iter
    release releaseSnap
    return $ map (runGet bitcoinGet . toLazyBS . bscToBS . snd) pairs
-}

