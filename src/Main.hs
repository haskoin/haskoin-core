import Network
import System.IO
import Data.Char --for ord
import System.Random -- for randon nonce
import Data.Time.Clock.POSIX -- unix time

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import Data.Enumerator ( ($$) )

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString as BS

import Bitcoin.Message
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Ping
import Bitcoin.Util
import Bitcoin.Protocol.Tx
import qualified Bitcoin.Constants as Const

main = withSocketsDo $ do
    h <- connectTo "127.0.0.1" (PortNumber 18333)
    hSetBuffering h LineBuffering
    sendVersion h
    E.run_ $ (EB.enumHandle 1024 h) $$ (loopIter h)

loopIter h = do
    req <- iterMessage
    E.run_ $ (processMessage req) $$ (EB.iterHandle h)
    loopIter h

sendVersion :: Handle -> IO ()
sendVersion h = do
    let zeroAddr = 0xffff00000000
        addr = NetworkAddress 1 zeroAddr 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    nonce <- randomIO
    let version = Version 70001 1 (floor time) addr addr nonce ua 0 False
    E.run_ $ (enumMessage $ MVersion version) $$ (EB.iterHandle h)

processMessage :: MonadIO m => Message -> E.Enumerator BS.ByteString m b
processMessage msg step = do
    liftIO $ print msg 
    case msg of
        MVersion _ -> (enumMessage MVerAck) step
        --MVerAck -> (enumMessage MGetAddr) step
        MPing (Ping n) -> (enumMessage $ MPong (Pong n)) step
        MTx t -> (processTx t) step
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
    (Tx ver vin vout nlock) 
        -> not $ getSerializeSize (MTx tx) > Const.maxBlockSize
    
