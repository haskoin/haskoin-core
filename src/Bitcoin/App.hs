{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Bitcoin.App
( BitcoinApp
, runBitcoinApp
, processMessage
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Control.Concurrent.STM

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.Map.Strict as Map

import Data.Maybe

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

import Bitcoin.Store
import Bitcoin.Store.STM

import qualified Bitcoin.Constants as Const

type BitcoinApp m = ReaderT AppState m

data AppState = AppState { memState :: MemState } 

getMemState :: AppStore m => BitcoinApp m MemState
getMemState = ask >>= return . memState

withDB :: AppStore m => m a -> BitcoinApp m a
withDB = lift

withMem :: AppStore m => BitcoinMem a -> BitcoinApp m a
withMem m = do
    state <- getMemState
    liftIO $ atomically $ runReaderT m state

runBitcoinApp :: AppStore m => BitcoinApp m () -> IO ()
runBitcoinApp m = do
    memState <- newMemState
    runAppDB $ runReaderT (initBitcoinApp >> m) (AppState memState)

initBitcoinApp :: AppStore m => BitcoinApp m ()
initBitcoinApp = do
    liftIO $ print "Initializing Haskoin"
    val <- withDB $ dbGetIndex testGenesisBlockHash 
    case val of
        (Just _) -> liftIO $ print "Found existing LevelDB database"
        Nothing  -> do
            liftIO $ print "Initializing LevelDB with genesis block"
            withDB $ dbPut $ buildBlockIndex testGenesisBlock Nothing
    liftIO $ print "Loading block index"
    blockIndices <- withDB $ dbStreamIndices C.$$ CL.consume
    withMem $ withIndexMap $ forM_ blockIndices dbPut
    liftIO $ print "Haskoin initialized"

processMessage :: AppStore m => Message -> BitcoinApp m [Message]
processMessage msg = case msg of
    MVersion _     -> return [MVerAck]
    MVerAck        -> processVerAck
    MPing (Ping n) -> return [MPong (Pong n)]
    MInv (Inv vs)  -> processInvVector vs
    MBlock b       -> processBlock b
    _              -> return []

processVerAck :: AppStore m => BitcoinApp m [Message]
processVerAck = withMem $ do
    bestIndex <- fromJust <$> getBestIndex
    locator <- withIndexMap $ getBlockLocator bestIndex
    return [buildGetBlocks locator]

processBlock :: AppStore m => Block -> BitcoinApp m [Message]
processBlock block = do
    insertedBIs <- withMem $ saveBlock block
    case insertedBIs of
        [] -> withMem $ do
            bestIndex  <- fromJust <$> getBestIndex
            orphanRoot <- withOrphanMap $ getRootOf block
            locator    <- withIndexMap $ getBlockLocator bestIndex
            return [buildStopGetBlocks locator (blockHash orphanRoot)]
        _ -> withDB $ do
            forM_ insertedBIs dbPut
            return []

processInvVector :: AppStore m => [InvVector] -> BitcoinApp m [Message]
processInvVector vs = withMem $ do
    let blockVectors = filter ((== InvBlock) . invType) vs
    (have,notHave)  <- partitionM haveInvVector blockVectors
    orphans         <- withOrphanMap $ mapM dbGet (map invHash have)
    orphanGetBlocks <- buildOrphanGetBlocks orphans
    lastMBI         <- withIndexMap $ dbGetIndex (invHash $ last blockVectors)
    lastGetBlocks   <- case lastMBI of
                        (Just lastBI) -> do
                            lastLocator <- withIndexMap $ getBlockLocator lastBI
                            return $ [buildGetBlocks lastLocator]
                        Nothing -> return []
    return $ 
        MGetData (GetData notHave) : 
        orphanGetBlocks ++ lastGetBlocks

haveInvVector :: InvVector -> BitcoinMem Bool
haveInvVector v 
    | (invType v) == InvBlock = alreadyHave (invHash v)
    | otherwise = return True -- ignore for now

buildOrphanGetBlocks :: [Maybe Block] -> BitcoinMem [Message]
buildOrphanGetBlocks ((Just b):xs) = do
    orphanRoot <- withOrphanMap $ getRootOf b
    bestIndex  <- fromJust <$> getBestIndex
    rest       <- buildOrphanGetBlocks xs
    locator    <- withIndexMap $ getBlockLocator bestIndex
    return $ buildStopGetBlocks locator (blockHash orphanRoot) : rest
buildOrphanGetBlocks (Nothing:xs) = buildOrphanGetBlocks xs
buildOrphanGetBlocks _ = return []

checkTransaction :: Tx -> Bool
checkTransaction tx = case tx of
    (Tx _ [] _ _) -> False --vin False
    (Tx _ _ [] _) -> False --vout False
    _ -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize

