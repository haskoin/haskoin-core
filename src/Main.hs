import Network
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Random 
import Data.Char 
import Data.Time.Clock.POSIX 
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

import Bitcoin.App
import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Message
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.RunConfig

import Bitcoin.Store
import Bitcoin.Store.LevelDB (AppDB)


import qualified Data.Map.Strict as Map

import qualified Text.Show.Pretty as Pr

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder flagOptions args of
        (flags,[],[])  -> runBitcoinApp flags $ haskoin
        (_,nonOpts,[]) -> 
            error $ "Unrecognized arguments: " ++ unwords nonOpts
        (_,_,msgs)     -> 
            error $ concat msgs ++ usageInfo usageHeader flagOptions

haskoin :: BitcoinApp AppDB ()
haskoin = do 

    host <- withConf getHost
    port <- withConf getPort

    -- Open network handle
    h <- lift $ liftIO $ connectTo host port
    lift $ liftIO $ hSetBuffering h LineBuffering

    -- Greet our host with the Version message
    version <- buildVersion
    (CL.sourceList [[MVersion version]])
        C.$$ fromMessages
        C.=$ (CB.sinkHandle h)

    -- Execute main program loop
    (CB.sourceHandle h) 
        C.$= toMessage 
        C.$= (C.awaitForever mainLoop)
        C.$$ fromMessages 
        C.=$ (CB.sinkHandle h)

mainLoop :: AppStore m => Message -> C.Conduit Message (BitcoinApp m) [Message]
mainLoop msg = C.yield =<< (lift $ processMessage msg)

buildVersion :: AppStore m => BitcoinApp m Version
buildVersion = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) haskoinUserAgent
    time <- liftIO $ getPOSIXTime
    rdmn <- liftIO $ randomIO -- nonce
    return $ Version protocolVersion 1 (floor time) addr addr rdmn ua 0 False
    
