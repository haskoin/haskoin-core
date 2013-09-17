module Haskoin.Wallet.Manager
( MasterKey
, AccPrvKey
, AccPubKey
, AddrPrvKey
, AddrPubKey
, makeMasterKey
, loadMasterKey
, loadPrvAcc
, loadPubAcc
, addr
, accPrvKeys
, accPubKeys
, extPrvKeys
, extPubKeys
, intPrvKeys
, intPubKeys
, extPubKeys2
, extPubKeys3
, intPubKeys2
, intPubKeys3
) where

import Control.Applicative

import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet.Keys
import Haskoin.Crypto
import Haskoin.Protocol

type KeyIndex = Word32

-- m/
newtype MasterKey = MasterKey { runMasterKey :: XPrvKey }
    deriving (Eq, Show)
-- m/i'/ 
newtype AccPrvKey = AccPrvKey { runAccPrvKey :: XPrvKey }
    deriving (Eq, Show)
-- M/i'/
newtype AccPubKey = AccPubKey { runAccPubKey :: XPubKey }
    deriving (Eq, Show)
-- m/i'/0|1/j
newtype AddrPrvKey = AddrPrvKey { runAddrPrvKey :: XPrvKey }
    deriving (Eq, Show)
-- M/i'/0|1/j
newtype AddrPubKey = AddrPubKey { runAddrPubKey :: XPubKey }
    deriving (Eq, Show)

makeMasterKey :: BS.ByteString -> Maybe MasterKey
makeMasterKey bs = MasterKey <$> makeXPrvKey bs

loadMasterKey :: XPrvKey -> MasterKey
loadMasterKey = MasterKey

loadPrvAcc :: XPrvKey -> AccPrvKey
loadPrvAcc = AccPrvKey

loadPubAcc :: XPubKey -> AccPubKey
loadPubAcc = AccPubKey

addr :: AddrPubKey -> Address
addr = xPubAddr . runAddrPubKey

-- List of all valid accounts derived from the master private key
-- Filters accounts for which dubkeys 0 and 1 are invalid
accPrvKeys :: MasterKey -> KeyIndex -> [AccPrvKey]
accPrvKeys (MasterKey par) i = map AccPrvKey $ filter f $ primeSubKeys par i
    where f k = isJust (prvSubKey k 0) && isJust (prvSubKey k 1)

accPubKeys :: MasterKey -> KeyIndex -> [AccPubKey]
accPubKeys k i = map f $ accPrvKeys k i
    where f = AccPubKey . deriveXPubKey . runAccPrvKey

extPrvKeys :: AccPrvKey -> KeyIndex -> [AddrPrvKey]
extPrvKeys (AccPrvKey par) i = map AddrPrvKey $ prvSubKeys extKey i
    where extKey = fromJust $ prvSubKey par 0

extPubKeys :: AccPubKey -> KeyIndex -> [AddrPubKey]
extPubKeys (AccPubKey par) i = map AddrPubKey $ pubSubKeys extKey i
    where extKey = fromJust $ pubSubKey par 0

intPrvKeys :: AccPrvKey -> KeyIndex -> [AddrPrvKey]
intPrvKeys (AccPrvKey par) i = map AddrPrvKey $ prvSubKeys intKey i
    where intKey = fromJust $ prvSubKey par 1

intPubKeys :: AccPubKey -> KeyIndex -> [AddrPubKey]
intPubKeys (AccPubKey par) i = map AddrPubKey $ pubSubKeys intKey i
    where intKey = fromJust $ pubSubKey par 1

{- MultiSig -}

extPubKeys2 :: AccPubKey -> AccPubKey -> KeyIndex -> [Script]
extPubKeys2 p1 p2 i = pubSubKeys2 extKey1 extKey2 i
    where extKey1 = fromJust $ pubSubKey (runAccPubKey p1) 0
          extKey2 = fromJust $ pubSubKey (runAccPubKey p2) 0

extPubKeys3 :: AccPubKey -> AccPubKey -> AccPubKey -> KeyIndex -> [Script]
extPubKeys3 p1 p2 p3 i = pubSubKeys3 extKey1 extKey2 extKey3 i
    where extKey1 = fromJust $ pubSubKey (runAccPubKey p1) 0
          extKey2 = fromJust $ pubSubKey (runAccPubKey p2) 0
          extKey3 = fromJust $ pubSubKey (runAccPubKey p3) 0

intPubKeys2 :: AccPubKey -> AccPubKey -> KeyIndex -> [Script]
intPubKeys2 p1 p2 i = pubSubKeys2 intKey1 intKey2 i
    where intKey1 = fromJust $ pubSubKey (runAccPubKey p1) 1
          intKey2 = fromJust $ pubSubKey (runAccPubKey p2) 1

intPubKeys3 :: AccPubKey -> AccPubKey -> AccPubKey -> KeyIndex -> [Script]
intPubKeys3 p1 p2 p3 i = pubSubKeys3 intKey1 intKey2 intKey3 i
    where intKey1 = fromJust $ pubSubKey (runAccPubKey p1) 1
          intKey2 = fromJust $ pubSubKey (runAccPubKey p2) 1
          intKey3 = fromJust $ pubSubKey (runAccPubKey p3) 1


