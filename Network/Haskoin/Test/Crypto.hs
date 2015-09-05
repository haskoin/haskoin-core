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
, ArbitraryHardPath(..)
, ArbitrarySoftPath(..)
, ArbitraryDerivPath(..)
) where

import Test.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , oneof
    , listOf
    )

import Data.Bits (clearBit)
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
newtype ArbitraryAddress = ArbitraryAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddress where
    arbitrary = ArbitraryAddress <$> oneof
        [ arbitrary >>= \(ArbitraryPubKeyAddress a) -> return a
        , arbitrary >>= \(ArbitraryScriptAddress a) -> return a
        ]

-- | Arbitrary public key hash address
newtype ArbitraryPubKeyAddress = ArbitraryPubKeyAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyAddress where
    arbitrary = do
        i <- arbitrary
        return $ ArbitraryPubKeyAddress $ PubKeyAddress i

-- | Arbitrary script hash address
newtype ArbitraryScriptAddress = ArbitraryScriptAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryScriptAddress where
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
data ArbitraryXPrvKey = ArbitraryXPrvKey XPrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryXPrvKey where
    arbitrary = do
        d <- arbitrary
        p <- arbitrary
        i <- arbitrary
        c <- arbitrary
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryXPrvKey $ XPrvKey d p i c k

-- | Arbitrary extended public key with its corresponding private key.
data ArbitraryXPubKey = ArbitraryXPubKey XPrvKey XPubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryXPubKey where
    arbitrary = do
        ArbitraryXPrvKey k <- arbitrary
        return $ ArbitraryXPubKey k $ deriveXPubKey k

{- Custom derivations -}

genIndex :: Gen Word32
genIndex = (`clearBit` 31) <$> arbitrary

data ArbitraryHardPath = ArbitraryHardPath HardPath
    deriving (Show, Eq)

instance Arbitrary ArbitraryHardPath where
    arbitrary =
        ArbitraryHardPath <$> (go =<< listOf genIndex)
      where
        go []     = elements [ Deriv, DerivPrv, DerivPub ]
        go (i:is) = (:| i) <$> go is

data ArbitrarySoftPath = ArbitrarySoftPath SoftPath
    deriving (Show, Eq)

instance Arbitrary ArbitrarySoftPath where
    arbitrary =
        ArbitrarySoftPath <$> (go =<< listOf genIndex)
      where
        go []     = elements [ Deriv, DerivPrv, DerivPub ]
        go (i:is) = (:/ i) <$> go is

data ArbitraryDerivPath = ArbitraryDerivPath DerivPath
    deriving (Show, Eq)

instance Arbitrary ArbitraryDerivPath where
    arbitrary = do
        xs  <- listOf genIndex
        ys  <- listOf genIndex
        return . ArbitraryDerivPath . goSoft ys =<< goHard xs
      where
        goSoft [] h     = h
        goSoft (i:is) h = (goSoft is h) :/ i
        goHard :: HardOrMixed t => [Word32] -> Gen (DerivPathI t)
        goHard (i:is) = (:| i) <$> goHard is
        goHard []     = elements [ Deriv, DerivPrv, DerivPub ]

