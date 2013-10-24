module Haskoin.Wallet.Manager
( MasterKey(..)
, AccPrvKey(..)
, AccPubKey(..)
, AddrPrvKey(..)
, AddrPubKey(..)
, KeyIndex
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
, extAddr
, intAddr
, extAddrs
, intAddrs
, extAddrs'
, intAddrs'
, extMulSigKey
, intMulSigKey
, extMulSigKeys
, intMulSigKeys
, extMulSigAddr
, intMulSigAddr
, extMulSigAddrs
, intMulSigAddrs
) where

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet.Keys
import Haskoin.Script
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

-- Filters accounts for which dubkeys 0 and 1 are invalid
accPrvKey :: MasterKey -> KeyIndex -> Maybe AccPrvKey
accPrvKey (MasterKey par) i = AccPrvKey <$> (f =<< primeSubKey par i)
    where f k = guard (isJust $ prvSubKey k 0) >>
                guard (isJust $ prvSubKey k 1) >>
                return k

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
accPrvKeys :: MasterKey -> KeyIndex -> [(AccPrvKey,KeyIndex)]
accPrvKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPrvKey m j) (return j)

accPubKeys :: MasterKey -> KeyIndex -> [(AccPubKey,KeyIndex)]
accPubKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPubKey m j) (return j)

extPrvKeys :: AccPrvKey -> KeyIndex -> [(AddrPrvKey,KeyIndex)]
extPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPrvKey a j) (return j)

extPubKeys :: AccPubKey -> KeyIndex -> [(AddrPubKey,KeyIndex)]
extPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPubKey a j) (return j)

intPrvKeys :: AccPrvKey -> KeyIndex -> [(AddrPrvKey,KeyIndex)]
intPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPrvKey a j) (return j)

intPubKeys :: AccPubKey -> KeyIndex -> [(AddrPubKey,KeyIndex)]
intPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPubKey a j) (return j)

{- Generate addresses -}

addr :: AddrPubKey -> Address
addr = xPubAddr . runAddrPubKey

extAddr :: AccPubKey -> KeyIndex -> Maybe String
extAddr a i = addrToBase58 . addr <$> extPubKey a i

intAddr :: AccPubKey -> KeyIndex -> Maybe String
intAddr a i = addrToBase58 . addr <$> intPubKey a i

extAddrs :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
extAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extAddr a j) (return j)

intAddrs :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
intAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intAddr a j) (return j)

-- Generate addresses in reverse (useful for displaying history)
extAddrs' :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
extAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (extAddr a j) (return j)

intAddrs' :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
intAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (intAddr a j) (return j)

{- MultiSig -}

extMulSigKey :: AccPubKey -> [XPubKey] -> KeyIndex -> Maybe [AddrPubKey]
extMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 0)) $ (runAccPubKey a) : ps

intMulSigKey :: AccPubKey -> [XPubKey] -> KeyIndex -> Maybe [AddrPubKey]
intMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 1)) $ (runAccPubKey a) : ps

-- Multisig addresses on external chain
extMulSigKeys :: AccPubKey -> [XPubKey] -> KeyIndex -> [([AddrPubKey],KeyIndex)]
extMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigKey a ps j) (return j)

intMulSigKeys :: AccPubKey -> [XPubKey] -> KeyIndex -> [([AddrPubKey],KeyIndex)]
intMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigKey a ps j) (return j)

extMulSigAddr :: AccPubKey -> [XPubKey] -> Int -> KeyIndex -> Maybe String
extMulSigAddr a ps r i = do
    ps <- (map (xPubKey . runAddrPubKey)) <$> extMulSigKey a ps i
    return $ addrToBase58 $ scriptAddr $ sortMulSig $ PayMulSig ps r

intMulSigAddr :: AccPubKey -> [XPubKey] -> Int -> KeyIndex -> Maybe String
intMulSigAddr a ps r i = do
    ps <- (map (xPubKey . runAddrPubKey)) <$> intMulSigKey a ps i
    return $ addrToBase58 $ scriptAddr $ sortMulSig $ PayMulSig ps r

extMulSigAddrs :: AccPubKey -> [XPubKey] -> Int -> KeyIndex 
              -> [(String,KeyIndex)]
extMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigAddr a ps r j) (return j)

intMulSigAddrs :: AccPubKey -> [XPubKey] -> Int -> KeyIndex 
              -> [(String,KeyIndex)]
intMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigAddr a ps r j) (return j)

