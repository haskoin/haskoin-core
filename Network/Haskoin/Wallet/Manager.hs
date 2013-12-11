module Network.Haskoin.Wallet.Manager
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

import Network.Haskoin.Wallet.Keys
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol

type KeyIndex = Word32

-- | Data type representing an extended private key at the root of the
-- derivation tree. Master keys have depth 0 and no parents. They are
-- represented as m\/ in BIP32 notation.
newtype MasterKey = MasterKey { runMasterKey :: XPrvKey }
    deriving (Eq, Show)

-- | Data type representing a private account key. Account keys are generated
-- from a 'MasterKey' through prime derivation. This guarantees that the
-- 'MasterKey' will not be compromised if the account key is compromised. 
-- 'AccPrvKey' is represented as m\/i'\/ in BIP32 notation.
newtype AccPrvKey = AccPrvKey { runAccPrvKey :: XPrvKey }
    deriving (Eq, Show)

-- | Data type representing a public account key. It is computed through
-- derivation from an 'AccPrvKey'. It can not be derived from the 'MasterKey'
-- directly (property of prime derivation). It is represented as M\/i'\/ in
-- BIP32 notation. 'AccPubKey' is used for generating receiving payment
-- addresses without the knowledge of the 'AccPrvKey'.
newtype AccPubKey = AccPubKey { runAccPubKey :: XPubKey }
    deriving (Eq, Show)

-- | Data type representing a private address key. Private address keys are
-- generated through a non-prime derivation from an 'AccPrvKey'. Non-prime
-- derivation is used so that the public account key can generate the receiving
-- payment addresses without knowledge of the private account key. 'AccPrvKey'
-- is represented as m\/i'\/0\/j\/ in BIP32 notation if it is a regular
-- receiving address. Internal (change) addresses are represented as
-- m\/i'\/1\/j\/. Non-prime subtree 0 is used for regular receiving addresses
-- and non-prime subtree 1 for internal (change) addresses.
newtype AddrPrvKey = AddrPrvKey { runAddrPrvKey :: XPrvKey }
    deriving (Eq, Show)

-- | Data type representing a public address key. They are generated through
-- non-prime derivation from an 'AccPubKey'. This is a useful feature for
-- read-only wallets. They are represented as M\/i'\/0\/j in BIP32 notation
-- for regular receiving addresses and by M\/i'\/1\/j for internal (change)
-- addresses.
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

