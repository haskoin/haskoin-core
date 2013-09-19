module Haskoin.Wallet.Manager
( MasterKey(..)
, AccPrvKey(..)
, AccPubKey(..)
, AddrPrvKey(..)
, AddrPubKey(..)
, makeMasterKey
, loadMasterKey
, loadPrvAcc
, loadPubAcc
, addr
, accPrvKey
, accPubKey
, extPrvKey
, extPubKey
, intPrvKey
, intPubKey
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

-- Create a new MasterKey from a random seed
makeMasterKey :: BS.ByteString -> Maybe MasterKey
makeMasterKey bs = MasterKey <$> makeXPrvKey bs

-- Load a MasterKey from an existing XPrvKey (usually saved on disk)
loadMasterKey :: XPrvKey -> Maybe MasterKey
loadMasterKey k
    | xPrvDepth  k == 0 && 
      xPrvParent k == 0 && 
      xPrvIndex  k == 0 = Just $ MasterKey k
    | otherwise         = Nothing

-- Load an account from an existing XPrvKey (usually saved on disk) 
loadPrvAcc :: XPrvKey -> Maybe AccPrvKey
loadPrvAcc k
    | xPrvDepth k == 1 &&
      xPrvIsPrime k    = Just $ AccPrvKey k
    | otherwise        = Nothing

-- Load a read-only account from an existing XPubKey (usually saved on disk)
loadPubAcc :: XPubKey -> Maybe AccPubKey
loadPubAcc k
    | xPubDepth k == 1 &&
      xPubIsPrime k    = Just $ AccPubKey k
    | otherwise        = Nothing

addr :: AddrPubKey -> Address
addr = xPubAddr . runAddrPubKey

accPrvKey :: MasterKey -> KeyIndex -> Maybe AccPrvKey
accPrvKey (MasterKey par) i = AccPrvKey <$> primeSubKey par i

accPubKey :: MasterKey -> KeyIndex -> Maybe AccPubKey
accPubKey (MasterKey par) i = f <$> primeSubKey par i
    where f = AccPubKey . deriveXPubKey

extPrvKey :: AccPrvKey -> KeyIndex -> Maybe AddrPrvKey
extPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey extKey i
    where extKey = fromJust $ prvSubKey par 0

extPubKey :: AccPubKey -> KeyIndex -> Maybe AddrPubKey
extPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey extKey i
    where extKey = fromJust $ pubSubKey par 0

intPrvKey :: AccPrvKey -> KeyIndex -> Maybe AddrPrvKey
intPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey intKey i
    where intKey = fromJust $ prvSubKey par 1

intPubKey :: AccPubKey -> KeyIndex -> Maybe AddrPubKey
intPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey intKey i
    where intKey = fromJust $ pubSubKey par 1

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

-- 2 of 2 multisig (external chain)
extPubKeys2 :: AccPubKey -> XPubKey -> KeyIndex -> [Script]
extPubKeys2 a p1 i = pubSubKeys2 extKey1 extKey2 i
    where extKey1 = fromJust $ pubSubKey (runAccPubKey a) 0
          extKey2 = fromJust $ pubSubKey p1 0

-- 2 of 3 multisig (external chain)
extPubKeys3 :: AccPubKey -> XPubKey -> XPubKey -> KeyIndex -> [Script]
extPubKeys3 a p1 p2 i = pubSubKeys3 extKey1 extKey2 extKey3 i
    where extKey1 = fromJust $ pubSubKey (runAccPubKey a) 0
          extKey2 = fromJust $ pubSubKey p1 0
          extKey3 = fromJust $ pubSubKey p2 0

-- 2 of 2 multisig (internal chain)
intPubKeys2 :: AccPubKey -> XPubKey -> KeyIndex -> [Script]
intPubKeys2 a p1 i = pubSubKeys2 intKey1 intKey2 i
    where intKey1 = fromJust $ pubSubKey (runAccPubKey a) 1
          intKey2 = fromJust $ pubSubKey p1 1

-- 2 of 3 multisig (internal chain)
intPubKeys3 :: AccPubKey -> XPubKey -> XPubKey -> KeyIndex -> [Script]
intPubKeys3 a p1 p2 i = pubSubKeys3 intKey1 intKey2 intKey3 i
    where intKey1 = fromJust $ pubSubKey (runAccPubKey a) 1
          intKey2 = fromJust $ pubSubKey p1 1
          intKey3 = fromJust $ pubSubKey p2 1


