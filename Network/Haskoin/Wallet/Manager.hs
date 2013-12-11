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

-- | Create a 'MasterKey' from a seed.
makeMasterKey :: BS.ByteString -> Maybe MasterKey
makeMasterKey bs = MasterKey <$> makeXPrvKey bs

-- | Load a 'MasterKey' from an 'XPrvKey'. This function will fail if the
-- extended private key does not have the properties of a 'MasterKey'.
loadMasterKey :: XPrvKey -> Maybe MasterKey
loadMasterKey k
    | xPrvDepth  k == 0 && 
      xPrvParent k == 0 && 
      xPrvIndex  k == 0 = Just $ MasterKey k
    | otherwise         = Nothing

-- | Load a private account key from an 'XPrvKey'. This function will fail if
-- the extended private key does not have the properties of a 'AccPrvKey'.
loadPrvAcc :: XPrvKey -> Maybe AccPrvKey
loadPrvAcc k
    | xPrvDepth k == 1 &&
      xPrvIsPrime k    = Just $ AccPrvKey k
    | otherwise        = Nothing

-- | Load a public account key from an 'XPubKey'. This function will fail if
-- the extended public key does not have the properties of a 'AccPubKey'.
loadPubAcc :: XPubKey -> Maybe AccPubKey
loadPubAcc k
    | xPubDepth k == 1 &&
      xPubIsPrime k    = Just $ AccPubKey k
    | otherwise        = Nothing

-- | Computes an 'AccPrvKey' from a 'MasterKey' and a derivation index.
accPrvKey :: MasterKey -> KeyIndex -> Maybe AccPrvKey
accPrvKey (MasterKey par) i = AccPrvKey <$> (f =<< primeSubKey par i)
    where f k = guard (isJust $ prvSubKey k 0) >>
                guard (isJust $ prvSubKey k 1) >>
                return k

-- | Computes an 'AccPubKey' from a 'MasterKey' and a derivation index.
accPubKey :: MasterKey -> KeyIndex -> Maybe AccPubKey
accPubKey (MasterKey par) i = f <$> primeSubKey par i
    where f = AccPubKey . deriveXPubKey

-- | Computes an external 'AddrPrvKey' from an 'AccPrvKey' and a derivation
-- index.
extPrvKey :: AccPrvKey -> KeyIndex -> Maybe AddrPrvKey
extPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey extKey i
    where extKey = fromJust $ prvSubKey par 0

-- | Computes an external 'AddrPubKey' from an 'AccPubKey' and a derivation
-- index.
extPubKey :: AccPubKey -> KeyIndex -> Maybe AddrPubKey
extPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey extKey i
    where extKey = fromJust $ pubSubKey par 0

-- | Computes an internal 'AddrPrvKey' from an 'AccPrvKey' and a derivation
-- index.
intPrvKey :: AccPrvKey -> KeyIndex -> Maybe AddrPrvKey
intPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey intKey i
    where intKey = fromJust $ prvSubKey par 1

-- | Computes an internal 'AddrPubKey' from an 'AccPubKey' and a derivation
-- index.
intPubKey :: AccPubKey -> KeyIndex -> Maybe AddrPubKey
intPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey intKey i
    where intKey = fromJust $ pubSubKey par 1

-- | Cyclic list of all valid 'AccPrvKey' derived from a 'MasterKey' and
-- starting from an offset index.
accPrvKeys :: MasterKey -> KeyIndex -> [(AccPrvKey,KeyIndex)]
accPrvKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPrvKey m j) (return j)

-- | Cyclic list of all valid 'AccPubKey' derived from a 'MasterKey' and
-- starting from an offset index.
accPubKeys :: MasterKey -> KeyIndex -> [(AccPubKey,KeyIndex)]
accPubKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPubKey m j) (return j)

-- | Cyclic list of all valid external 'AddrPrvKey' derived from a 'AccPrvKey'
-- and starting from an offset index.
extPrvKeys :: AccPrvKey -> KeyIndex -> [(AddrPrvKey,KeyIndex)]
extPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPrvKey a j) (return j)

-- | Cyclic list of all valid external 'AddrPubKey' derived from a 'AccPubKey'
-- and starting from an offset index.
extPubKeys :: AccPubKey -> KeyIndex -> [(AddrPubKey,KeyIndex)]
extPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPubKey a j) (return j)

-- | Cyclic list of all internal 'AddrPrvKey' derived from a 'AccPrvKey' and
-- starting from an offset index.
intPrvKeys :: AccPrvKey -> KeyIndex -> [(AddrPrvKey,KeyIndex)]
intPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPrvKey a j) (return j)

-- | Cyclic list of all internal 'AddrPubKey' derived from a 'AccPubKey' and
-- starting from an offset index.
intPubKeys :: AccPubKey -> KeyIndex -> [(AddrPubKey,KeyIndex)]
intPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPubKey a j) (return j)

{- Generate addresses -}

-- | Computes an 'Address' from an 'AddrPubKey'.
addr :: AddrPubKey -> Address
addr = xPubAddr . runAddrPubKey

-- | Computes an external base58 address from an 'AccPubKey' and a 
-- derivation index.
extAddr :: AccPubKey -> KeyIndex -> Maybe String
extAddr a i = addrToBase58 . addr <$> extPubKey a i

-- | Computes an internal base58 addres from an 'AccPubKey' and a 
-- derivation index.
intAddr :: AccPubKey -> KeyIndex -> Maybe String
intAddr a i = addrToBase58 . addr <$> intPubKey a i

-- | Cyclic list of all external base58 addresses derived from a 'AccPubKey'
-- and starting from an offset index.
extAddrs :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
extAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extAddr a j) (return j)

-- | Cyclic list of all internal base58 addresses derived from a 'AccPubKey'
-- and starting from an offset index.
intAddrs :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
intAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intAddr a j) (return j)

-- | Same as 'extAddrs' with the list reversed.
extAddrs' :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
extAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (extAddr a j) (return j)

-- | Same as 'intAddrs' with the list reversed.
intAddrs' :: AccPubKey -> KeyIndex -> [(String,KeyIndex)]
intAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (intAddr a j) (return j)

{- MultiSig -}

-- | Computes a list of external 'AddrPubKey' from an 'AccPubKey', a list
-- of thirdparty multisig keys and a derivation index. This is useful for 
-- computing the public keys associated with a derivation index for
-- multisig accounts.
extMulSigKey :: AccPubKey -> [XPubKey] -> KeyIndex -> Maybe [AddrPubKey]
extMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 0)) $ (runAccPubKey a) : ps

-- | Computes a list of internal 'AddrPubKey' from an 'AccPubKey', a list
-- of thirdparty multisig keys and a derivation index. This is useful for 
-- computing the public keys associated with a derivation index for
-- multisig accounts.
intMulSigKey :: AccPubKey -> [XPubKey] -> KeyIndex -> Maybe [AddrPubKey]
intMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 1)) $ (runAccPubKey a) : ps

-- | Cyclic list of all external multisignature 'AddrPubKey' derivations 
-- starting from an offset index.
extMulSigKeys :: AccPubKey -> [XPubKey] -> KeyIndex -> [([AddrPubKey],KeyIndex)]
extMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigKey a ps j) (return j)

-- | Cyclic list of all internal multisignature 'AddrPubKey' derivations
-- starting from an offset index.
intMulSigKeys :: AccPubKey -> [XPubKey] -> KeyIndex -> [([AddrPubKey],KeyIndex)]
intMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigKey a ps j) (return j)

-- | Computes an external base58 multisig address from an 'AccPubKey', a
-- list of thirdparty multisig keys and a derivation index.
extMulSigAddr :: AccPubKey -> [XPubKey] -> Int -> KeyIndex -> Maybe String
extMulSigAddr a ps r i = do
    ps <- (map (xPubKey . runAddrPubKey)) <$> extMulSigKey a ps i
    return $ addrToBase58 $ scriptAddr $ sortMulSig $ PayMulSig ps r

-- | Computes an internal base58 multisig address from an 'AccPubKey', a
-- list of thirdparty multisig keys and a derivation index.
intMulSigAddr :: AccPubKey -> [XPubKey] -> Int -> KeyIndex -> Maybe String
intMulSigAddr a ps r i = do
    ps <- (map (xPubKey . runAddrPubKey)) <$> intMulSigKey a ps i
    return $ addrToBase58 $ scriptAddr $ sortMulSig $ PayMulSig ps r

-- | Cyclic list of all external base58 multisig addresses derived from
-- an 'AccPubKey' and a list of thirdparty multisig keys. The list starts
-- at an offset index.
extMulSigAddrs :: AccPubKey -> [XPubKey] -> Int -> KeyIndex 
              -> [(String,KeyIndex)]
extMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigAddr a ps r j) (return j)

-- | Cyclic list of all internal base58 multisig addresses derived from
-- an 'AccPubKey' and a list of thirdparty multisig keys. The list starts
-- at an offset index.
intMulSigAddrs :: AccPubKey -> [XPubKey] -> Int -> KeyIndex 
              -> [(String,KeyIndex)]
intMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigAddr a ps r j) (return j)

