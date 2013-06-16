import Network
import System.IO
import Data.Char --for ord
import System.Random -- for randon nonce
import Data.Time.Clock.POSIX -- unix time
import Data.Default

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Database.LevelDB as DB

import Bitcoin.Message
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Ping
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.Inv
import Bitcoin.Protocol.GetData
import Bitcoin.Protocol.Block
import Bitcoin.LevelDB
import qualified Bitcoin.Constants as Const

import qualified Text.Show.Pretty as Pr

mainContext :: ResourceT IO a -> IO a 
mainContext = withSocketsDo . DB.runResourceT

main :: IO ()
main = mainContext $ do
    db <- DB.open "blockindex" 
        DB.defaultOptions { DB.createIfMissing = True
                          , DB.cacheSize = 2048
                          }
    h <- liftIO $ do 
        h <- connectTo "127.0.0.1" (PortNumber 18333)
        hSetBuffering h LineBuffering
        sendVersion h
        return h
    E.run_ $ (EB.enumHandle 1024 h) E.$$ (loopIter h db)

loopIter :: MonadIO m => Handle -> DB.DB -> E.Iteratee BS.ByteString m b
loopIter h db = do
    req <- iterMessage
    E.run_ $ (processMessage req) E.$$ (EB.iterHandle h)
    case req of
        MBlock block -> writeBlock db block
        _            -> ldbContext $ return ()
    loopIter h db

writeBlock :: MonadIO m => DB.DB -> Block -> m ()
writeBlock db block = ldbContext $ do
    DB.put db def (BSC.pack "block") (buildBlockIndex block)
    val <- DB.get db def (BSC.pack "block") 
    liftIO $ print val

sendVersion :: Handle -> IO ()
sendVersion h = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    let vers = Version 70001 1 (floor time) addr addr rdmn ua 0 False
    E.run_ $ (enumMessage $ MVersion vers) E.$$ (EB.iterHandle h)

processMessage :: MonadIO m => Message -> E.Enumerator BS.ByteString m b
processMessage msg step = do
    liftIO $ putStrLn $ Pr.ppShow msg 
    case msg of
        MVersion _ -> (enumMessage MVerAck) step
        --MVerAck -> (enumMessage MGetAddr) step
        MPing (Ping n) -> (enumMessage $ MPong (Pong n)) step
        MTx t -> (processTx t) step
        MInv (Inv l) -> (enumMessage $ MGetData (GetData l)) step
        _ -> E.returnI step

processTx :: MonadIO m => Tx -> E.Enumerator BS.ByteString m b
processTx tx step = do
    liftIO $ if (checkTransaction tx)
                 then putStr "checkTransaction true"
                 else putStr "checkTransaction false"
    E.enumEOF step

checkTransaction :: Tx -> Bool
checkTransaction tx = case tx of
    (Tx _ [] _ _) -> False --vin False
    (Tx _ _ [] _) -> False --vout False
    _ -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize
    
