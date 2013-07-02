import Network
import System.IO
import Data.Char --for ord
import System.Random -- for randon nonce
import Data.Time.Clock.POSIX -- unix time
import Data.Default

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Control.Concurrent.STM

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Bitcoin.BlockChain.LevelDB as DB

import Bitcoin.BlockChain.MemoryMaps
import Bitcoin.BlockChain.BlockIndex
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

import qualified Bitcoin.Constants as Const
import Bitcoin.Util

import qualified Data.Map.Strict as Map

import qualified Text.Show.Pretty as Pr

main :: IO ()
main = withSocketsDo $ do

    -- Open network handle
    h <- connectTo Const.bitcoinHost Const.bitcoinPort
    hSetBuffering h LineBuffering

    -- Greet our host with the Version message
    version <- buildVersion
    (CL.sourceList [[MVersion version]])
        C.$$ fromMessages
        C.=$ (CB.sinkHandle h)

    -- Initialise shared memory
    mapBlockIndex   <- newTVarIO (Map.empty :: MapBlockIndex)
    mapOrphanBlocks <- newTVarIO (Map.empty :: MapOrphanBlocks)
    bestBlock       <- newTVarIO (buildBlockIndex testGenesisBlock Nothing)
    let mem = MemoryMap mapBlockIndex mapOrphanBlocks bestBlock

    (_, log) <- atomically $ runWriterT (evalStateT initMemoryMaps mem)
    when (not $ null log) (putStrLn log)

    -- Execute main program loop
    DB.runResourceT $ do
        db <- DB.openHandle
        (CB.sourceHandle h) 
            C.$= toMessage 
            C.$= (runApp db mem)
            C.$$ fromMessages 
            C.=$ (CB.sinkHandle h)

runApp :: DB.DB -> MemoryMap -> C.Conduit Message (ResourceT IO) [Message]
runApp db mem = C.awaitForever $ \msg -> do
    (res, log) <- lift $ evalStateT (runStateSTM $ dispatchMessage msg) mem
    when (not $ null log) $ liftIO $ putStrLn log
    C.yield res

dispatchMessage :: Message -> StateSTM [Message]
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

haveInvVector :: InvVector -> StateSTM Bool
haveInvVector v 
    | (invType v) == InvBlock = alreadyHave (invHash v)
    | otherwise = return True -- ignore for now

buildOrphanGetBlocks :: [Maybe Block] -> StateSTM [Message]
buildOrphanGetBlocks ((Just b):xs) = do
    orphanRoot      <- getOrphanRoot b
    bestBlockIndex  <- getBestBlockIndex
    rest            <- buildOrphanGetBlocks xs
    locator         <- buildBlockLocator bestBlockIndex
    return $ buildStopGetBlocks locator (blockHash orphanRoot) : rest
buildOrphanGetBlocks (Nothing:xs) = buildOrphanGetBlocks xs
buildOrphanGetBlocks _ = return []

buildVersion :: IO Version
buildVersion = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) addr addr rdmn ua 0 False

checkTransaction :: Tx -> Bool
checkTransaction tx = case tx of
    (Tx _ [] _ _) -> False --vin False
    (Tx _ _ [] _) -> False --vout False
    _ -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize
    
