module Bitcoin.App
( BitcoinApp
, runBitcoinApp
, runBitcoinMem
, initBitcoinApp
, processMessage
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Control.Concurrent.STM

import qualified Data.Map.Strict as Map

import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Message
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Ping
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.Inv
import Bitcoin.Protocol.InvVector
import Bitcoin.Protocol.GetData
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.GetBlocks

import Bitcoin.BlockChain.BitcoinMem
import Bitcoin.BlockChain.BlockIndex

import Bitcoin.BlockStore

import qualified Bitcoin.Constants as Const

newtype BitcoinApp m a = BitcoinApp { runApp :: ReaderT AppConfig m a }

instance BlockStore m => Monad (BitcoinApp m) where
    a >>= f = BitcoinApp $ do
        val <- runApp a
        runApp $ f val
    return = BitcoinApp . return

instance BlockStore m => MonadIO (BitcoinApp m) where
    liftIO = BitcoinApp . liftIO

dbGet :: BlockStore m => Word256 -> BitcoinApp m (Maybe BlockIndex)
dbGet = BitcoinApp . lift . blockStoreGet

dbPut :: BlockStore m => BlockIndex -> BitcoinApp m ()
dbPut = BitcoinApp . lift . blockStorePut

data AppConfig = AppConfig 
    { memState :: MemState
    } 

getMemState :: BlockStore m => BitcoinApp m MemState
getMemState = do
    val <- BitcoinApp ask
    return $ memState val

runBitcoinApp :: BlockStore m => BitcoinApp m () -> IO ()
runBitcoinApp m = do
    mapBlockIndex   <- newTVarIO (Map.empty :: MapBlockIndex)
    mapOrphanBlocks <- newTVarIO (Map.empty :: MapOrphanBlocks)
    bestBlock       <- newTVarIO (buildBlockIndex testGenesisBlock Nothing)
    let s = AppConfig
                (MemState mapBlockIndex mapOrphanBlocks bestBlock)
    runDB $ runReaderT (runApp $ initBitcoinApp >> m) s

runBitcoinMem :: BlockStore m => BitcoinMem a -> BitcoinApp m a
runBitcoinMem m = do
    s <- getMemState
    (res, log) <- liftIO . atomically . runWriterT $ evalStateT m s
    when (not (null log)) (liftIO $ print log)
    return res
     
initBitcoinApp :: BlockStore m => BitcoinApp m ()
initBitcoinApp = do

    bis <- do
        liftIO $ print "Initializing LevelDB ... "
        val <- dbGet testGenesisBlockHash
        case val of
            (Just _) -> liftIO $ print "LevelDB already initialized"
            Nothing  -> do
                liftIO $ print "Initializing LevelDB with genesis block"
                dbPut $ buildBlockIndex testGenesisBlock Nothing
        --getAllBlockIndices
        return []

    liftIO $ print "Loading Block Indices ... "

    runBitcoinMem $ do
        initBitcoinMem >> (forM_ bis putBlockIndexMem)
        bb <- getBestBlockIndex
        tell $ "Best Block Height: " ++ (show $ biHeight bb) ++ " "
        tell $ "Initialization complete "

processMessage :: BlockStore m => Message -> BitcoinApp m [Message]
processMessage msg = case msg of
    MVersion _     -> return [MVerAck]
    MVerAck        -> processVerAck
    MPing (Ping n) -> return [MPong (Pong n)]
    MInv (Inv vs)  -> processInvVector vs
    MBlock b       -> processBlock b
    _              -> return []

processVerAck :: BlockStore m => BitcoinApp m [Message]
processVerAck = runBitcoinMem $ do
    bestBlockIndex <- getBestBlockIndex
    locator        <- buildBlockLocator bestBlockIndex
    return [buildGetBlocks locator]

processBlock :: BlockStore m => Block -> BitcoinApp m [Message]
processBlock block = do
    newBIs <- runBitcoinMem $ do
        exists <- alreadyHave (blockHash block) 
        if exists then return []
        else putBlock block
    case newBIs of
        [] -> runBitcoinMem $ do
            bestBlockIndex <- getBestBlockIndex
            orphanRoot     <- getOrphanRoot block
            locator        <- buildBlockLocator bestBlockIndex
            return [buildStopGetBlocks locator (blockHash orphanRoot)]
        _ -> do
            forM_ newBIs dbPut
            return []

processInvVector :: BlockStore m => [InvVector] -> BitcoinApp m [Message]
processInvVector vs = runBitcoinMem $ do
    let blockVectors = filter ((== InvBlock) . invType) vs
    (have,notHave)  <- partitionM haveInvVector blockVectors
    orphans         <- mapM lookupOrphanBlock (map invHash have)
    orphanGetBlocks <- buildOrphanGetBlocks orphans
    lastMBI         <- lookupBlockIndexMem (invHash $ last blockVectors)
    lastGetBlocks   <- case lastMBI of
                        (Just lastBI) -> do
                            lastLocator <- buildBlockLocator lastBI
                            return $ [buildGetBlocks lastLocator]
                        Nothing -> return []
    tell $ "Got Inv of size " ++ (show $ length vs) ++ " "
    tell $ "I have this many: " ++ (show $ length have) ++ " "
    tell $ "Orphan block locator " ++ (show orphanGetBlocks) ++ " "
    tell $ "LastBlock block locator " ++ (show lastGetBlocks) ++ " "
    return $ MGetData (GetData notHave) : orphanGetBlocks ++ lastGetBlocks

haveInvVector :: InvVector -> BitcoinMem Bool
haveInvVector v 
    | (invType v) == InvBlock = alreadyHave (invHash v)
    | otherwise = return True -- ignore for now

buildOrphanGetBlocks :: [Maybe Block] -> BitcoinMem [Message]
buildOrphanGetBlocks ((Just b):xs) = do
    orphanRoot      <- getOrphanRoot b
    bestBlockIndex  <- getBestBlockIndex
    rest            <- buildOrphanGetBlocks xs
    locator         <- buildBlockLocator bestBlockIndex
    return $ buildStopGetBlocks locator (blockHash orphanRoot) : rest
buildOrphanGetBlocks (Nothing:xs) = buildOrphanGetBlocks xs
buildOrphanGetBlocks _ = return []

checkTransaction :: Tx -> Bool
checkTransaction tx = case tx of
    (Tx _ [] _ _) -> False --vin False
    (Tx _ _ [] _) -> False --vout False
    _ -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize

