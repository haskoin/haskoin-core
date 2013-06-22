import Network
import System.IO
import Data.Char --for ord
import System.Random -- for randon nonce
import Data.Time.Clock.POSIX -- unix time
import Data.Default

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Database.LevelDB as DB

import Bitcoin.Protocol
import Bitcoin.Message
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Ping
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.Inv
import Bitcoin.Protocol.GetData
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.GetBlocks


import qualified Bitcoin.LevelDB as DB
import qualified Bitcoin.Constants as Const

import qualified Text.Show.Pretty as Pr

main :: IO ()
main = withSocketsDo $ do

    -- Open network handle
    h <- connectTo Const.bitcoinHost Const.bitcoinPort
    hSetBuffering h LineBuffering

    -- Greet our host with the Version message
    version <- buildVersion
    (CL.sourceList [Just $ MVersion version])
        C.$$ fromMessage
        C.=$ (CB.sinkHandle h)

    -- Execute main program loop
    DB.runResourceT $ do
        db <- DB.openHandle
        (CB.sourceHandle h) 
            C.$= toMessage 
            C.$= (runApp db)
            C.$$ fromMessage 
            C.=$ (CB.sinkHandle h)

buildVersion :: IO Version
buildVersion = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) addr addr rdmn ua 0 False

runApp :: MonadResource m => DB.DB -> C.Conduit Message m (Maybe Message)
runApp db = C.awaitForever $ \msg -> do
    liftIO $ putStrLn $ Pr.ppShow msg
    res <- lift $ do
        case msg of
            MVersion _ -> return $ Just MVerAck
            MVerAck -> do
                loc <- buildBlockLocator
                return . Just . MGetBlocks $ 
                    GetBlocks (fromIntegral 1) loc (fromIntegral 0)
            MPing (Ping n) -> return $ Just $ MPong (Pong n)
            MInv (Inv l) -> return $ Just $ MGetData (GetData l)
            _ -> return $ Nothing
    C.yield res

checkTransaction :: Tx -> Bool
checkTransaction tx = case tx of
    (Tx _ [] _ _) -> False --vin False
    (Tx _ _ [] _) -> False --vout False
    _ -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize

buildBlockLocator :: MonadResource m => m [Word256]
buildBlockLocator = do
    return $ [genesisBlockHash]
    
