module Bitcoin.BlockChain.BitcoinDB
( BitcoinDB
, openDBHandle
, runResourceT
, initBitcoinDB
, lookupBlockIndex
, putBlockIndex
, putBlock
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

openDBHandle :: ResourceT IO DBState
openDBHandle = DBState <$> (DB.open "blockindex" dbOptions)

getDBHandle :: BitcoinDB DB.DB
getDBHandle = get >>= return . dbHandle

initBitcoinDB :: BitcoinDB ()
initBitcoinDB = do
    val <- lookupBlockIndex testGenesisBlockHash
    case val of
        (Just _) -> do
            liftIO $ print "LevelDB already initialized" 
            return ()
        Nothing  -> do
            liftIO $ print $ "Initializing LevelDB. Writing genesis block hash "
                ++ (show testGenesisBlockHash)
            putBlockIndex $ buildBlockIndex testGenesisBlock Nothing

lookupBlockIndex :: Word256 -> BitcoinDB (Maybe BlockIndex)
lookupBlockIndex w = do
    db <- getDBHandle
    let key = toStrictBS . runPut . putWord256be $ w
    val <- liftDB $ DB.get db def (bsToBSC key)
    return $ val >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

putBlockIndex :: BlockIndex -> BitcoinDB ()
putBlockIndex bi = do
    db <- getDBHandle
    let key = toStrictBS . runPut . putWord256be $ biHash bi
    let payload = toStrictBS . runPut . bitcoinPut $ bi
    liftDB $ DB.put db def (bsToBSC key) (bsToBSC payload)

putBlock :: Block -> BitcoinDB ()
putBlock block = do
    let hash = blockHash block
    current <- lookupBlockIndex hash
    case current of
        -- we already have the block, ignore it
        (Just _) -> do
            liftIO $ print $ "Block " ++ (show hash) ++ " already indexed"
            return ()
        Nothing  -> do
            prev <- lookupBlockIndex $ prevBlock (blockHeader block)
            liftIO $ print $ "Indexing block " ++ (show hash)
            putBlockIndex $ buildBlockIndex block prev

