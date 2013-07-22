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

import Bitcoin.App
import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Message
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress

import Bitcoin.Store.LevelDB (AppDB)

import qualified Bitcoin.Constants as Const

import qualified Data.Map.Strict as Map

import qualified Text.Show.Pretty as Pr

main :: IO ()
main = do

    -- Open network handle
    h <- connectTo Const.bitcoinHost Const.bitcoinPort
    hSetBuffering h LineBuffering

    -- Greet our host with the Version message
    version <- buildVersion
    (CL.sourceList [[MVersion version]])
        C.$$ fromMessages
        C.=$ (CB.sinkHandle h)

    -- Execute main program loop
    runBitcoinApp $ 
        (CB.sourceHandle h) 
            C.$= toMessage 
            C.$= (C.awaitForever mainLoop)
            C.$$ fromMessages 
            C.=$ (CB.sinkHandle h)

mainLoop :: Message -> C.Conduit Message (BitcoinApp AppDB) [Message]
mainLoop msg = C.yield =<< (lift $ processMessage msg)

buildVersion :: IO Version
buildVersion = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    rdmn <- randomIO -- nonce
    return $ Version 70001 1 (floor time) addr addr rdmn ua 0 False
    
