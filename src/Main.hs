import Data.Binary
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Types

zeroAddr = [0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0]
addr = NetworkAddress 1 (BL.pack zeroAddr) 0
version = Version 1 1 0 addr addr 0 0 False

