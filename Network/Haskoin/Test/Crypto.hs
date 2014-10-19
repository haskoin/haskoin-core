{-| 
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto
( ArbitraryBigWord(..)
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
) where

import Test.QuickCheck

import Control.Applicative ((<$>), (<*>))

import Data.Maybe
import qualified Data.Sequence as S (fromList)

import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Crypto.NormalizedKeys
import Network.Haskoin.Crypto.Merkle
import Network.Haskoin.Crypto.Bloom

-- | Arbitrary BigWord using arbitrarySizedBoundedIntegral
data ArbitraryBigWord n = ArbitraryBigWord (BigWord n)
    deriving (Eq, Show, Read)

instance BigWordMod n => Arbitrary (ArbitraryBigWord n) where
    arbitrary = ArbitraryBigWord <$> arbitrarySizedBoundedIntegral

-- | Arbitrary Point on the secp256k1 curve
newtype ArbitraryPoint = ArbitraryPoint Point
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPoint where
    arbitrary = do
        ArbitraryBigWord x <- arbitrary
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
        [ arbitrary >>= \(ArbitraryPrvKeyC k) -> return k
        , arbitrary >>= \(ArbitraryPrvKeyU k) -> return k
        ]

-- | Arbitrary compressed private key
newtype ArbitraryPrvKeyC = ArbitraryPrvKeyC PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKeyC where
    arbitrary = do
        i <- fromInteger <$> choose (1, curveN-1)
        return $ ArbitraryPrvKeyC $ fromJust $ makePrvKey i
        
-- | Arbitrary uncompressed private key
newtype ArbitraryPrvKeyU = ArbitraryPrvKeyU PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKeyU where
    arbitrary = do
        i <- fromInteger <$> choose (1, curveN-1)
        return $ ArbitraryPrvKeyU $ fromJust $ makePrvKeyU i

-- | Arbitrary public key (can be both compressed or uncompressed)
newtype ArbitraryPubKey = ArbitraryPubKey PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKey where
    arbitrary = ArbitraryPubKey <$> oneof
        [ arbitrary >>= \(ArbitraryPubKeyC p) -> return p
        , arbitrary >>= \(ArbitraryPubKeyU p) -> return p
        ]

-- | Arbitrary compressed public key
newtype ArbitraryPubKeyC = ArbitraryPubKeyC PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyC where
    arbitrary = do
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryPubKeyC $ derivePubKey k

-- | Arbitrary uncompressed public key
newtype ArbitraryPubKeyU = ArbitraryPubKeyU PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyU where
    arbitrary = do
        ArbitraryPrvKeyU k <- arbitrary
        return $ ArbitraryPubKeyU $ derivePubKey k

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
        ArbitraryBigWord i <- arbitrary
        return $ ArbitraryPubKeyAddress $ PubKeyAddress i

-- | Arbitrary script hash address
newtype ArbitraryScriptAddress = ArbitraryScriptAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryScriptAddress where
    arbitrary = do
        ArbitraryBigWord i <- arbitrary
        return $ ArbitraryScriptAddress $ ScriptAddress i

