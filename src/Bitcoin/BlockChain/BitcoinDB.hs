module Bitcoin.BlockChain.BitcoinDB
( BitcoinDB
, DBState(..)
, openDBHandle
, runResourceT
, initBitcoinDB
, lookupBlockIndexDB
, putBlockIndexDB
, getAllBlockIndexes
) where

import Data.Default

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex
import Bitcoin.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Database.LevelDB as DB

type BitcoinDB = StateT DBState (WriterT String (ResourceT IO))

data DBState = DBState { dbHandle :: DB.DB }

liftDB :: ResourceT IO a -> BitcoinDB a
liftDB = lift . lift

dbOptions = DB.defaultOptions
    { DB.createIfMissing = True
    , DB.cacheSize = 2048
    }

openDBHandle :: ResourceT IO DB.DB
openDBHandle = DB.open "blockindex" dbOptions

getDBHandle :: BitcoinDB DB.DB
getDBHandle = get >>= return . dbHandle

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

getAllBlockIndexes :: BitcoinDB [BlockIndex]
getAllBlockIndexes = do
    db <- getDBHandle
    (releaseSnap, snap) <- DB.createSnapshot' db
    pairs <- DB.withIterator db def{DB.useSnapshot = Just snap} $ \iter -> do
        DB.iterFirst iter
        DB.iterItems iter
    release releaseSnap
    return $ map (runGet bitcoinGet . toLazyBS . bscToBS . snd) pairs

