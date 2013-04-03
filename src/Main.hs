import Network
import System.IO
import Data.Binary
import Data.Char --for ord
import Numeric -- for showHex
import System.Random -- for randon nonce
import Control.Applicative

import Data.Time.Clock.POSIX -- unix time

import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Message
import Bitcoin.Type.VarString
import Bitcoin.Type.NetworkAddress
import Bitcoin.Util

import qualified Bitcoin.Type as Bitcoin

zeroAddr = [0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0]
addr = NetworkAddress 1 (BS.pack zeroAddr) 0
ua = VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"

main = withSocketsDo $ do
    h <- connectTo "127.0.0.1" (PortNumber 18333)
    hSetBuffering h LineBuffering
    time <- getPOSIXTime
    nonce <- randomIO
    let version = Version 70001 1 (floor time) addr addr nonce ua 0 False
    print version
    BS.hPutStr h (toStrict . runPut . Bitcoin.put $ MVersion version)
    msg <- feed (runGetIncremental (Bitcoin.get :: Get Message)) h
    print msg
    hClose h

feed :: Decoder Message -> Handle -> IO Message
feed (Done bs _ msg ) h = return msg
feed (Fail _ _ str) _ = error $ "readMessageLoop: " ++ str
feed (Partial cont) h = do
    chunk <- BS.hGet h 1
    case BS.length chunk of
        0 -> feed (cont Nothing) h
        _ -> feed (cont (Just chunk)) h


