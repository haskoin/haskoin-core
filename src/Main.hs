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
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Database.LevelDB as DB

import Bitcoin.MemMap
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

import qualified Bitcoin.LevelDB as DB
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
    bestBlock       <- newTVarIO DB.genesisBlockIndex

    -- Execute main program loop
    DB.runResourceT $ do
        db <- DB.openHandle
        DB.initBlockIndex db
        (CB.sourceHandle h) 
            C.$= toMessage 
            C.$= (C.awaitForever $ \msg -> do
                res <- lift $ evalStateT 
                                (runApp db msg)
                                (MemMap 
                                    mapBlockIndex 
                                    mapOrphanBlocks
                                    bestBlock)
                C.yield res)
            C.$$ fromMessages 
            C.=$ (CB.sinkHandle h)

runApp :: DB.DB -> Message -> App [Message]
runApp db msg = case msg of
    MVersion _ -> return [MVerAck]
    MVerAck -> do
        loc <- buildBlockLocator
        return [ MGetBlocks $ 
                   GetBlocks 
                      (fromIntegral 1) 
                      loc 
                      (fromIntegral 0)
               ]
    MPing (Ping n) -> return [MPong (Pong n)]
    MInv (Inv l) -> do
        (ls,rs)   <- partitionM haveInvVector l
        orphans   <- runStateSTM $ mapM lookupOrphanBlock (map invHash ls)
        getBlocks <- buildOrphanGetBlocks orphans
        return $ (MGetData (GetData rs)) : getBlocks
    MBlock b -> do
        DB.writeBlock db b
        return []
    _ -> return []

haveInvVector :: InvVector -> App Bool
haveInvVector v 
    | (invType v) == InvBlock = runStateSTM $ alreadyHave (invHash v)
    | otherwise = return True -- ignore for now

buildOrphanGetBlocks :: [Maybe Block] -> App [Message]
buildOrphanGetBlocks ((Just b):xs) = do
    orphanRoot      <- runStateSTM $ getOrphanRoot b
    bestBlockIndex  <- runStateSTM $ getBestBlockIndex
    rest            <- buildOrphanGetBlocks xs
    return $ (MGetBlocks $ GetBlocks
                            (fromIntegral 1)
                            [DB.biHash bestBlockIndex]
                            (blockHash orphanRoot)
             ) : rest
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

buildBlockLocator :: App [Word256]
buildBlockLocator = return [testGenesisBlockHash]
    
