module Bitcoin.BlockChain.LevelDB
( DB.DB
, DB.runResourceT
, openHandle
, initBlockIndex
, writeBlockIndex
, writeBlock
, readBlockIndex
) where

import Data.Default

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex
import Bitcoin.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Database.LevelDB as DB

openHandle :: ResourceT IO DB.DB
openHandle = do
    db <- DB.open "blockindex"
        DB.defaultOptions
            { DB.createIfMissing = True
            , DB.cacheSize = 2048
            }
    return db

initBlockIndex :: DB.DB -> ResourceT IO ()
initBlockIndex db = do
    val <- readBlockIndex db testGenesisBlockHash
    case val of
        (Just _) -> do
            liftIO $ print "LevelDB already initialized" 
            return ()
        Nothing  -> do
            liftIO $ print $ "Initializing LevelDB. Writing genesis block hash "
                ++ (show testGenesisBlockHash)
            writeBlockIndex db (buildBlockIndex testGenesisBlock Nothing)

readBlockIndex :: MonadResource m => DB.DB -> Word256 -> m (Maybe BlockIndex)
readBlockIndex db w = do
    let key = toStrictBS . runPut . putWord256be $ w
    val <- DB.get db def (bsToBSC key)
    return $ val >>= return . (runGet bitcoinGet) . toLazyBS . bscToBS

writeBlockIndex :: MonadResource m => DB.DB -> BlockIndex -> m ()
writeBlockIndex db bi = do
    let key = toStrictBS . runPut . putWord256be $ biHash bi
    let payload = toStrictBS . runPut . bitcoinPut $ bi
    DB.put db def (bsToBSC key) (bsToBSC payload)
    return ()

writeBlock :: MonadResource m => DB.DB -> Block -> m ()
writeBlock db b = do
    let hash = blockHash b
    let prevHash = prevBlock $ blockHeader b
    current <- readBlockIndex db hash
    case current of
        -- we already have the block, ignore it
        (Just _) -> do
            liftIO $ print $ "Block " ++ (show hash) ++ " already indexed"
            return ()
        Nothing  -> do
            prev <- readBlockIndex db prevHash
            liftIO $ print $ "Indexing block " ++ (show hash)
            writeBlockIndex db (buildBlockIndex b prev)

