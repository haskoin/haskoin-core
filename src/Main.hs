import Network
import System.IO
import Data.Binary
import Data.Char --for ord
import Numeric -- for showHex
import System.Random -- for randon nonce

import Data.Time.Clock.POSIX -- unix time

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Bitcoin.Types as B

zeroAddr = [0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0]
addr = B.NetworkAddress 1 (BS.pack zeroAddr) 0
userAgent = B.VarString $ BS.pack $ map (fromIntegral . ord) "/haskoin:0.0.1/"

main = withSocketsDo $ do
    h <- connectTo "127.0.0.1" (PortNumber 18333)
    hSetBuffering h LineBuffering
    time <- getPOSIXTime
    nonce <- randomIO
    let version = B.Version
                    70001 1 (floor time) addr addr nonce userAgent 0 False
        message = B.encodeMessage $ B.MVersion version
    print version
    BS.hPutStr h message
    contents <- BL.hGetContents h
    let response = B.decodeMessage contents
    print response
    hClose h

