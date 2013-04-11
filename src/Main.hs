import Network
import System.IO
import Data.Binary
import Data.Char --for ord
import Numeric -- for showHex
import System.Random -- for randon nonce

import Control.Applicative
import Control.Monad.IO.Class

import Data.Time.Clock.POSIX -- unix time

import Data.Binary.Get
import Data.Binary.Put

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import Data.Enumerator ( ($$) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Message
import Bitcoin.Type.VarString
import Bitcoin.Type.NetworkAddress
import Bitcoin.Util

import qualified Bitcoin.Type as Bitcoin

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
    let zeroAddr = [0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0]
        addr = NetworkAddress 1 (BS.pack zeroAddr) 0
        ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"
    time <- getPOSIXTime
    nonce <- randomIO
    let version = Version 70001 1 (floor time) addr addr nonce ua 0 False
    E.run_ $ (enumMessage $ MVersion version) $$ (EB.iterHandle h)
    print version

processMessage :: MonadIO m => Message -> E.Enumerator BS.ByteString m b
processMessage msg (E.Continue k) = do
    liftIO $ print msg
    E.continue k
processMessage m step = E.returnI step


