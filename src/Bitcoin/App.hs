module Bitcoin.App
( BitcoinApp
, AppState
, runBitcoinApp
, runBitcoinDB
, runBitcoinMem
, initBitcoinApp
, processMessage
) where

import Control.Monad
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
import qualified Bitcoin.BlockChain.BitcoinDB as DB
import Bitcoin.BlockChain.BlockIndex

import qualified Bitcoin.Constants as Const

type BitcoinApp = StateT AppState IO

data AppState = AppState { memState :: MemState } 

getMemState :: BitcoinApp MemState
getMemState = get >>= return . memState

runBitcoinApp :: AppState -> BitcoinApp a -> IO a
runBitcoinApp s m = evalStateT m s

runBitcoinDB :: DB.BitcoinDB a -> BitcoinApp (a, String)
runBitcoinDB m = 
    liftIO . runResourceT $ DB.openDBHandle >>= runWriterT . (evalStateT m)

runBitcoinMem :: BitcoinMem a -> BitcoinApp (a, String)
runBitcoinMem m = 
    getMemState >>= liftIO . atomically . runWriterT . (evalStateT m)

initBitcoinApp :: IO AppState
initBitcoinApp = do
    mapBlockIndex   <- newTVarIO (Map.empty :: MapBlockIndex)
    mapOrphanBlocks <- newTVarIO (Map.empty :: MapOrphanBlocks)
    bestBlock       <- newTVarIO (buildBlockIndex testGenesisBlock Nothing)
    let s = AppState $ MemState mapBlockIndex mapOrphanBlocks bestBlock
    runBitcoinApp s $ do
        runBitcoinMem initBitcoinMem 
        runBitcoinDB DB.initBitcoinDB
    return s

processMessage :: Message -> BitcoinApp [Message]
processMessage msg = do
    (res, log) <- runBitcoinMem $ dispatchMessage msg
    when (not $ null log) $ liftIO $ putStrLn log
    return res

dispatchMessage :: Message -> BitcoinMem [Message]
dispatchMessage msg = case msg of
    MVersion _ -> return [MVerAck]
    MVerAck -> do
        bestBlockIndex <- getBestBlockIndex
        locator        <- buildBlockLocator bestBlockIndex
        return [buildGetBlocks locator]
    MPing (Ping n) -> return [MPong (Pong n)]
    MInv (Inv l) -> do
        let blockList = filter ((== InvBlock) . invType) l
        (have,notHave)  <- partitionM haveInvVector blockList
        orphans         <- mapM lookupOrphanBlock (map invHash have)
        orphanGetBlocks <- buildOrphanGetBlocks orphans
        lastMBI         <- lookupBlockIndex (invHash $ last blockList)
        lastGetBlocks   <- case lastMBI of
                            (Just lastBI) -> do
                                lastLocator <- buildBlockLocator lastBI
                                return $ [buildGetBlocks lastLocator]
                            Nothing -> return []
        logString $ "Got Inv of size " ++ (show $ length l)
        logString $ "I have this many: " ++ (show $ length have)
        logString $ "Orphan block locator " ++ (show orphanGetBlocks)
        logString $ "LastBlock block locator " ++ (show lastGetBlocks)
        return $ MGetData (GetData notHave) : orphanGetBlocks ++ lastGetBlocks
    MBlock b -> do
        conditionM (alreadyHave $ blockHash b) (return []) $ 
            conditionM (addBlock b) (return []) $ do
                bestBlockIndex <- getBestBlockIndex
                orphanRoot     <- getOrphanRoot b
                locator        <- buildBlockLocator bestBlockIndex
                return [buildStopGetBlocks locator (blockHash orphanRoot)]
        -- DB.writeBlock db b
    _ -> return []

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

