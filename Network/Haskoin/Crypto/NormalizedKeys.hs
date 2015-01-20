module Network.Haskoin.Crypto.NormalizedKeys
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
, toAddr
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

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2, guard, mzero, (<=<))
import Control.Applicative ((<$>))

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Word (Word32)
import Data.Maybe (mapMaybe, fromJust, isJust)
import qualified Data.ByteString as BS (ByteString)

import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Script.Parser
import Network.Haskoin.Network

type KeyIndex = Word32

-- | Data type representing an extended private key at the root of the
-- derivation tree. Master keys have depth 0 and no parents. They are
-- represented as m\/ in BIP32 notation.
newtype MasterKey a = MasterKey { masterKey :: XPrvKey a }
    deriving (Eq, Show, Read)

instance NFData (MasterKey a) where
    rnf (MasterKey m) = rnf m

instance Network a => ToJSON (MasterKey a) where
    toJSON = toJSON . masterKey

instance Network a => FromJSON (MasterKey a) where
    parseJSON = maybe mzero return . loadMasterKey <=< parseJSON

-- | Data type representing a private account key. Account keys are generated
-- from a 'MasterKey' through prime derivation. This guarantees that the
-- 'MasterKey' will not be compromised if the account key is compromised. 
-- 'AccPrvKey' is represented as m\/i'\/ in BIP32 notation.
newtype AccPrvKey a = AccPrvKey { getAccPrvKey :: XPrvKey a }
    deriving (Eq, Show, Read)

instance NFData (AccPrvKey a) where
    rnf (AccPrvKey k) = rnf k

instance Network a => ToJSON (AccPrvKey a) where
    toJSON = toJSON . getAccPrvKey

instance Network a => FromJSON (AccPrvKey a) where
    parseJSON = maybe mzero return . loadPrvAcc <=< parseJSON

-- | Data type representing a public account key. It is computed through
-- derivation from an 'AccPrvKey'. It can not be derived from the 'MasterKey'
-- directly (property of prime derivation). It is represented as M\/i'\/ in
-- BIP32 notation. 'AccPubKey' is used for generating receiving payment
-- addresses without the knowledge of the 'AccPrvKey'.
newtype AccPubKey a = AccPubKey { getAccPubKey :: XPubKey a }
    deriving (Eq, Show, Read)

instance NFData (AccPubKey a) where
    rnf (AccPubKey k) = rnf k

instance Network a => ToJSON (AccPubKey a) where
    toJSON = toJSON . getAccPubKey

instance Network a => FromJSON (AccPubKey a) where
    parseJSON = maybe mzero return . loadPubAcc <=< parseJSON

-- | Data type representing a private address key. Private address keys are
-- generated through a non-prime derivation from an 'AccPrvKey'. Non-prime
-- derivation is used so that the public account key can generate the receiving
-- payment addresses without knowledge of the private account key. 'AccPrvKey'
-- is represented as m\/i'\/0\/j\/ in BIP32 notation if it is a regular
-- receiving address. Internal (change) addresses are represented as
-- m\/i'\/1\/j\/. Non-prime subtree 0 is used for regular receiving addresses
-- and non-prime subtree 1 for internal (change) addresses.
newtype AddrPrvKey a = AddrPrvKey { getAddrPrvKey :: XPrvKey a }
    deriving (Eq, Show, Read)

instance NFData (AddrPrvKey a) where
    rnf (AddrPrvKey k) = rnf k

instance Network a => ToJSON (AddrPrvKey a) where
    toJSON = toJSON . getAddrPrvKey

instance Network a => FromJSON (AddrPrvKey a) where
    parseJSON = fmap AddrPrvKey . parseJSON

-- | Data type representing a public address key. They are generated through
-- non-prime derivation from an 'AccPubKey'. This is a useful feature for
-- read-only wallets. They are represented as M\/i'\/0\/j in BIP32 notation
-- for regular receiving addresses and by M\/i'\/1\/j for internal (change)
-- addresses.
newtype AddrPubKey a = AddrPubKey { getAddrPubKey :: XPubKey a }
    deriving (Eq, Show, Read)

instance NFData (AddrPubKey a) where
    rnf (AddrPubKey k) = rnf k

instance Network a => ToJSON (AddrPubKey a) where
    toJSON = toJSON . getAddrPubKey

instance Network a => FromJSON (AddrPubKey a) where
    parseJSON = fmap AddrPubKey . parseJSON

-- | Create a 'MasterKey' from a seed.
makeMasterKey :: BS.ByteString -> Maybe (MasterKey a)
makeMasterKey bs = MasterKey <$> makeXPrvKey bs

-- | Load a 'MasterKey' from an 'XPrvKey'. This function will fail if the
-- extended private key does not have the properties of a 'MasterKey'.
loadMasterKey :: XPrvKey a -> Maybe (MasterKey a)
loadMasterKey k
    | xPrvDepth  k == 0 && 
      xPrvParent k == 0 && 
      xPrvIndex  k == 0 = Just $ MasterKey k
    | otherwise         = Nothing

-- | Load a private account key from an 'XPrvKey'. This function will fail if
-- the extended private key does not have the properties of a 'AccPrvKey'.
loadPrvAcc :: XPrvKey a -> Maybe (AccPrvKey a)
loadPrvAcc k
    | xPrvDepth k == 1 &&
      xPrvIsPrime k    = Just $ AccPrvKey k
    | otherwise        = Nothing

-- | Load a public account key from an 'XPubKey'. This function will fail if
-- the extended public key does not have the properties of a 'AccPubKey'.
loadPubAcc :: XPubKey a -> Maybe (AccPubKey a)
loadPubAcc k
    | xPubDepth k == 1 &&
      xPubIsPrime k    = Just $ AccPubKey k
    | otherwise        = Nothing

-- | Computes an 'AccPrvKey' from a 'MasterKey' and a derivation index.
accPrvKey :: MasterKey a -> KeyIndex -> Maybe (AccPrvKey a)
accPrvKey (MasterKey par) i = AccPrvKey <$> (f =<< primeSubKey par i)
    where f k = guard (isJust $ prvSubKey k 0) >>
                guard (isJust $ prvSubKey k 1) >>
                return k

-- | Computes an 'AccPubKey' from a 'MasterKey' and a derivation index.
accPubKey :: MasterKey a -> KeyIndex -> Maybe (AccPubKey a)
accPubKey (MasterKey par) i = f <$> primeSubKey par i
    where f = AccPubKey . deriveXPubKey

-- | Computes an external 'AddrPrvKey' from an 'AccPrvKey' and a derivation
-- index.
extPrvKey :: AccPrvKey a -> KeyIndex -> Maybe (AddrPrvKey a)
extPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey extKey i
    where extKey = fromJust $ prvSubKey par 0

-- | Computes an external 'AddrPubKey' from an 'AccPubKey' and a derivation
-- index.
extPubKey :: AccPubKey a -> KeyIndex -> Maybe (AddrPubKey a)
extPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey extKey i
    where extKey = fromJust $ pubSubKey par 0

-- | Computes an internal 'AddrPrvKey' from an 'AccPrvKey' and a derivation
-- index.
intPrvKey :: AccPrvKey a -> KeyIndex -> Maybe (AddrPrvKey a)
intPrvKey (AccPrvKey par) i = AddrPrvKey <$> prvSubKey intKey i
    where intKey = fromJust $ prvSubKey par 1

-- | Computes an internal 'AddrPubKey' from an 'AccPubKey' and a derivation
-- index.
intPubKey :: AccPubKey a -> KeyIndex -> Maybe (AddrPubKey a)
intPubKey (AccPubKey par) i = AddrPubKey <$> pubSubKey intKey i
    where intKey = fromJust $ pubSubKey par 1

-- | Cyclic list of all valid 'AccPrvKey' derived from a 'MasterKey' and
-- starting from an offset index.
accPrvKeys :: MasterKey a -> KeyIndex -> [(AccPrvKey a,KeyIndex)]
accPrvKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPrvKey m j) (return j)

-- | Cyclic list of all valid 'AccPubKey' derived from a 'MasterKey' and
-- starting from an offset index.
accPubKeys :: MasterKey a -> KeyIndex -> [(AccPubKey a,KeyIndex)]
accPubKeys m i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (accPubKey m j) (return j)

-- | Cyclic list of all valid external 'AddrPrvKey' derived from a 'AccPrvKey'
-- and starting from an offset index.
extPrvKeys :: AccPrvKey a -> KeyIndex -> [(AddrPrvKey a,KeyIndex)]
extPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPrvKey a j) (return j)

-- | Cyclic list of all valid external 'AddrPubKey' derived from a 'AccPubKey'
-- and starting from an offset index.
extPubKeys :: AccPubKey a -> KeyIndex -> [(AddrPubKey a,KeyIndex)]
extPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extPubKey a j) (return j)

-- | Cyclic list of all internal 'AddrPrvKey' derived from a 'AccPrvKey' and
-- starting from an offset index.
intPrvKeys :: AccPrvKey a -> KeyIndex -> [(AddrPrvKey a,KeyIndex)]
intPrvKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPrvKey a j) (return j)

-- | Cyclic list of all internal 'AddrPubKey' derived from a 'AccPubKey' and
-- starting from an offset index.
intPubKeys :: AccPubKey a -> KeyIndex -> [(AddrPubKey a,KeyIndex)]
intPubKeys a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intPubKey a j) (return j)

{- Generate addresses -}

-- | Computes an 'Address' from an 'AddrPubKey'.
toAddr :: AddrPubKey a -> Address a
toAddr = xPubAddr . getAddrPubKey

-- | Computes an external address from an 'AccPubKey' and a 
-- derivation index.
extAddr :: AccPubKey a -> KeyIndex -> Maybe (Address a)
extAddr a i = toAddr <$> extPubKey a i

-- | Computes an internal addres from an 'AccPubKey' and a 
-- derivation index.
intAddr :: AccPubKey a -> KeyIndex -> Maybe (Address a)
intAddr a i = toAddr <$> intPubKey a i

-- | Cyclic list of all external addresses derived from a 'AccPubKey'
-- and starting from an offset index.
extAddrs :: AccPubKey a -> KeyIndex -> [(Address a,KeyIndex)]
extAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extAddr a j) (return j)

-- | Cyclic list of all internal addresses derived from a 'AccPubKey'
-- and starting from an offset index.
intAddrs :: AccPubKey a -> KeyIndex -> [(Address a,KeyIndex)]
intAddrs a i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intAddr a j) (return j)

-- | Same as 'extAddrs' with the list reversed.
extAddrs' :: AccPubKey a -> KeyIndex -> [(Address a,KeyIndex)]
extAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (extAddr a j) (return j)

-- | Same as 'intAddrs' with the list reversed.
intAddrs' :: AccPubKey a -> KeyIndex -> [(Address a,KeyIndex)]
intAddrs' a i = mapMaybe f $ cycleIndex' i
    where f j = liftM2 (,) (intAddr a j) (return j)

{- MultiSig -}

-- | Computes a list of external 'AddrPubKey' from an 'AccPubKey', a list
-- of thirdparty multisig keys and a derivation index. This is useful for 
-- computing the public keys associated with a derivation index for
-- multisig accounts.
extMulSigKey :: AccPubKey a -> [XPubKey a] -> KeyIndex -> Maybe [AddrPubKey a]
extMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 0)) $ (getAccPubKey a) : ps

-- | Computes a list of internal 'AddrPubKey' from an 'AccPubKey', a list
-- of thirdparty multisig keys and a derivation index. This is useful for 
-- computing the public keys associated with a derivation index for
-- multisig accounts.
intMulSigKey :: AccPubKey a -> [XPubKey a] -> KeyIndex -> Maybe [AddrPubKey a]
intMulSigKey a ps i = (map AddrPubKey) <$> mulSigSubKey keys i
    where keys = map (fromJust . (flip pubSubKey 1)) $ (getAccPubKey a) : ps

-- | Cyclic list of all external multisignature 'AddrPubKey' derivations 
-- starting from an offset index.
extMulSigKeys :: AccPubKey a
              -> [XPubKey a]
              -> KeyIndex
              -> [([AddrPubKey a],KeyIndex)]
extMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigKey a ps j) (return j)

-- | Cyclic list of all internal multisignature 'AddrPubKey' derivations
-- starting from an offset index.
intMulSigKeys :: AccPubKey a
              -> [XPubKey a]
              -> KeyIndex
              -> [([AddrPubKey a],KeyIndex)]
intMulSigKeys a ps i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigKey a ps j) (return j)

-- | Computes an external multisig address from an 'AccPubKey', a
-- list of thirdparty multisig keys and a derivation index.
extMulSigAddr :: AccPubKey a
              -> [XPubKey a]
              -> Int
              -> KeyIndex
              -> Maybe (Address a)
extMulSigAddr a ps r i = do
    xs <- (map (xPubKey . getAddrPubKey)) <$> extMulSigKey a ps i
    return $ scriptAddr $ sortMulSig $ PayMulSig (map toPubKeyG xs) r

-- | Computes an internal multisig address from an 'AccPubKey', a
-- list of thirdparty multisig keys and a derivation index.
intMulSigAddr :: AccPubKey a
              -> [XPubKey a]
              -> Int
              -> KeyIndex
              -> Maybe (Address a)
intMulSigAddr a ps r i = do
    xs <- (map (xPubKey . getAddrPubKey)) <$> intMulSigKey a ps i
    return $ scriptAddr $ sortMulSig $ PayMulSig (map toPubKeyG xs) r

-- | Cyclic list of all external multisig addresses derived from
-- an 'AccPubKey' and a list of thirdparty multisig keys. The list starts
-- at an offset index.
extMulSigAddrs :: AccPubKey a
               -> [XPubKey a]
               -> Int
               -> KeyIndex 
               -> [(Address a,KeyIndex)]
extMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (extMulSigAddr a ps r j) (return j)

-- | Cyclic list of all internal multisig addresses derived from
-- an 'AccPubKey' and a list of thirdparty multisig keys. The list starts
-- at an offset index.
intMulSigAddrs :: AccPubKey a
               -> [XPubKey a]
               -> Int
               -> KeyIndex 
              -> [(Address a,KeyIndex)]
intMulSigAddrs a ps r i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (intMulSigAddr a ps r j) (return j)

