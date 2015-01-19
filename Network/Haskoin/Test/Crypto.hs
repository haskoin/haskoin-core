{-| 
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto
( ArbitraryByteString(..)
, ArbitraryNotNullByteString(..)
, ArbitraryPoint(..)
, ArbitraryInfPoint(..)
, ArbitraryPrvKey(..)
, ArbitraryPrvKeyC(..)
, ArbitraryPrvKeyU(..)
, ArbitraryPubKey(..)
, ArbitraryPubKeyC(..)
, ArbitraryPubKeyU(..)
, ArbitraryAddress(..)
, ArbitraryPubKeyAddress(..)
, ArbitraryScriptAddress(..)
, ArbitrarySignature(..)
, ArbitraryDetSignature(..)
, ArbitraryXPrvKey(..)
, ArbitraryXPubKey(..)
, ArbitraryDerivPath(..)
, ArbitraryMasterKey(..)
, ArbitraryAccPrvKey(..)
, ArbitraryAccPubKey(..)
, ArbitraryAddrPrvKey(..)
, ArbitraryAddrPubKey(..)
) where

import Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , choose
    , elements
    , frequency
    , oneof
    )

import Control.Applicative ((<$>))

import Data.Bits (clearBit, testBit)
import Data.Maybe (fromJust)
import Data.Word (Word32)

import Network.Haskoin.Test.Util
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Crypto.NormalizedKeys

-- | Arbitrary Point on the secp256k1 curve
newtype ArbitraryPoint = ArbitraryPoint Point
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPoint where
    arbitrary = do
        x <- fromInteger <$> choose (1, toInteger (maxBound :: FieldN))
        return $ ArbitraryPoint $ mulPoint x curveG

-- | Arbitrary Point on the secp256k1 curve with 10% chance 
-- of being the point at infinity
newtype ArbitraryInfPoint = ArbitraryInfPoint Point
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryInfPoint where
    arbitrary = ArbitraryInfPoint <$> frequency 
        [ (1, return makeInfPoint)
        , (9, arbitrary >>= \(ArbitraryPoint p) -> return p)
        ]

-- | Arbitrary private key (can be both compressed or uncompressed)
newtype ArbitraryPrvKey = ArbitraryPrvKey PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKey where
    arbitrary = ArbitraryPrvKey <$> oneof 
        [ arbitrary >>= \(ArbitraryPrvKeyC k) -> return (toPrvKeyG k)
        , arbitrary >>= \(ArbitraryPrvKeyU k) -> return (toPrvKeyG k)
        ]

-- | Arbitrary compressed private key
newtype ArbitraryPrvKeyC = ArbitraryPrvKeyC PrvKeyC
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKeyC where
    arbitrary = do
        i <- fromInteger <$> choose (1, curveN-1)
        return $ ArbitraryPrvKeyC $ fromJust $ makePrvKeyC i
        
-- | Arbitrary uncompressed private key
newtype ArbitraryPrvKeyU = ArbitraryPrvKeyU PrvKeyU
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKeyU where
    arbitrary = do
        i <- fromInteger <$> choose (1, curveN-1)
        return $ ArbitraryPrvKeyU $ fromJust $ makePrvKeyU i

-- | Arbitrary public key (can be both compressed or uncompressed) with its
-- corresponding private key.
data ArbitraryPubKey = ArbitraryPubKey PrvKey PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKey where
    arbitrary = oneof
        [ arbitrary >>= \(ArbitraryPubKeyC k p) -> 
            return $ ArbitraryPubKey (toPrvKeyG k) (toPubKeyG p)
        , arbitrary >>= \(ArbitraryPubKeyU k p) -> 
            return $ ArbitraryPubKey (toPrvKeyG k) (toPubKeyG p)
        ]

-- | Arbitrary compressed public key with its corresponding private key.
data ArbitraryPubKeyC = ArbitraryPubKeyC PrvKeyC PubKeyC
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyC where
    arbitrary = do
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryPubKeyC k $ derivePubKey k

-- | Arbitrary uncompressed public key with its corresponding private key.
data ArbitraryPubKeyU = ArbitraryPubKeyU PrvKeyU PubKeyU
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyU where
    arbitrary = do
        ArbitraryPrvKeyU k <- arbitrary
        return $ ArbitraryPubKeyU k $ derivePubKey k

-- | Arbitrary address (can be a pubkey or script hash address)
newtype ArbitraryAddress a = ArbitraryAddress (Address a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryAddress a) where
    arbitrary = ArbitraryAddress <$> oneof
        [ arbitrary >>= \(ArbitraryPubKeyAddress a) -> return a
        , arbitrary >>= \(ArbitraryScriptAddress a) -> return a
        ]

-- | Arbitrary public key hash address
newtype ArbitraryPubKeyAddress a = ArbitraryPubKeyAddress (Address a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPubKeyAddress a) where
    arbitrary = do
        i <- arbitrary
        return $ ArbitraryPubKeyAddress $ PubKeyAddress i

-- | Arbitrary script hash address
newtype ArbitraryScriptAddress a = ArbitraryScriptAddress (Address a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryScriptAddress a) where
    arbitrary = do
        i <- arbitrary
        return $ ArbitraryScriptAddress $ ScriptAddress i

-- | Arbitrary message hash, private key, nonce and corresponding signature.
-- The signature is generated with a random message, random private key and a
-- random nonce.
data ArbitrarySignature = ArbitrarySignature Word256 PrvKey FieldN Signature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySignature where
    arbitrary = do
        msg  <- arbitrary
        ArbitraryPrvKey prv   <- arbitrary
        ArbitraryPrvKey nonce <- arbitrary
        let k   = prvKeyFieldN nonce
            p   = pubKeyPoint $ derivePubKey nonce
        case unsafeSignMsg msg (prvKeyFieldN prv) (k,p) of
            (Just sig) -> return $ ArbitrarySignature msg prv k sig
            Nothing    -> arbitrary

-- | Arbitrary message hash, private key and corresponding signature. The
-- signature is generated deterministically using a random message and random
-- private key.
data ArbitraryDetSignature = ArbitraryDetSignature Word256 PrvKey Signature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryDetSignature where
    arbitrary = do
        msg  <- arbitrary
        ArbitraryPrvKey prv   <- arbitrary
        return $ ArbitraryDetSignature msg prv $ detSignMsg msg prv

-- | Arbitrary extended private key.
data ArbitraryXPrvKey a = ArbitraryXPrvKey (XPrvKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryXPrvKey a) where
    arbitrary = do
        d <- arbitrary
        p <- arbitrary
        i <- arbitrary
        c <- arbitrary
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryXPrvKey $ XPrvKey d p i c k

-- | Arbitrary extended public key with its corresponding private key.
data ArbitraryXPubKey a = ArbitraryXPubKey (XPrvKey a) (XPubKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryXPubKey a) where
    arbitrary = do
        ArbitraryXPrvKey k <- arbitrary
        return $ ArbitraryXPubKey k $ deriveXPubKey k

data ArbitraryDerivPath = ArbitraryDerivPath DerivPath
    deriving (Eq, Show, Read)

data ArbitraryDeriv = ArbitraryDeriv (Word32, Bool)
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryDerivPath where
    arbitrary = do
        path <- map (\(ArbitraryDeriv x) -> x) <$> arbitrary
        ArbitraryDerivPath <$> elements 
            [ DerivPrv path
            , DerivPub path
            ]

instance Arbitrary ArbitraryDeriv where
    arbitrary = do
        w <- arbitrary
        let i = w `clearBit` 31
            b = w `testBit` 31
        return $ ArbitraryDeriv (i, b)

-- | Arbitrary master key
data ArbitraryMasterKey a = ArbitraryMasterKey (MasterKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMasterKey a) where
    arbitrary = do
        ArbitraryByteString bs <- arbitrary
        case makeMasterKey bs of
            Just k  -> return $ ArbitraryMasterKey k
            Nothing -> arbitrary

-- | Arbitrary private account key with its corresponding master key.
data ArbitraryAccPrvKey a = ArbitraryAccPrvKey (MasterKey a) (AccPrvKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryAccPrvKey a) where
    arbitrary = do
        ArbitraryMasterKey m <- arbitrary
        i <- choose (0,0x7fffffff)
        case accPrvKey m i of
            Just k -> return $ ArbitraryAccPrvKey m k
            Nothing -> arbitrary

-- | Arbitrary public account key with its corresponding master key
-- and private account key.
data ArbitraryAccPubKey a = 
    ArbitraryAccPubKey (MasterKey a) (AccPrvKey a) (AccPubKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryAccPubKey a) where
    arbitrary = do
        ArbitraryAccPrvKey m k <- arbitrary
        let p = AccPubKey $ deriveXPubKey $ getAccPrvKey k
        return $ ArbitraryAccPubKey m k p

-- | Arbitrary private address key with its corresponding master key and
-- private account key.
data ArbitraryAddrPrvKey a =
    ArbitraryAddrPrvKey (MasterKey a) (AccPrvKey a) (AddrPrvKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryAddrPrvKey a) where
    arbitrary = do
        ArbitraryAccPrvKey m k <- arbitrary
        i <- choose (0,0x7fffffff)
        f <- elements [extPrvKey, intPrvKey]
        case f k i of
            Just a  -> return $ ArbitraryAddrPrvKey m k a
            Nothing -> arbitrary

-- | Arbitrary public address key with its corresponding master key,
-- private account key and private address key.
data ArbitraryAddrPubKey a = 
    ArbitraryAddrPubKey (MasterKey a)  (AccPrvKey a)
                        (AddrPrvKey a) (AddrPubKey a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryAddrPubKey a) where
    arbitrary = do
        ArbitraryAddrPrvKey m k a <- arbitrary
        let p = AddrPubKey $ deriveXPubKey $ getAddrPrvKey a
        return $ ArbitraryAddrPubKey m k a p

