module Bitcoin.Constants where

import Data.Word
import Network

maxBlockSize :: Int
maxBlockSize = 1000000 

testnetMagic :: Word32
testnetMagic = 0x0b110907

bitcoinHost :: String
bitcoinHost = "127.0.0.1"

bitcoinPort :: PortID
bitcoinPort = (PortNumber 18333)


