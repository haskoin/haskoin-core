import Network
import System.IO
import Data.Binary
import Data.Char --for ord
import Numeric -- for showHex
import System.Random -- for randon nonce

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
    bl <- BL.hGetContents h
    print $ runGet (Bitcoin.get :: Get Message) bl
    hClose h

