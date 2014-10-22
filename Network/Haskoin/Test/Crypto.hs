{-| 
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto
( ArbitraryByteString(..)
, ArbitraryNotNullByteString(..)
, ArbitraryBigWord(..)
, ArbitraryWord512 
, ArbitraryWord256 
, ArbitraryWord160 
, ArbitraryWord128 
, ArbitraryFieldP  
, ArbitraryFieldN  
, ArbitraryTxHash  
, ArbitraryBlockHash
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
, ArbitraryMasterKey(..)
, ArbitraryAccPrvKey(..)
, ArbitraryAccPubKey(..)
, ArbitraryAddrPrvKey(..)
, ArbitraryAddrPubKey(..)
, ArbitraryBloomFlags(..)
, ArbitraryBloomFilter(..)
, ArbitraryFilterLoad(..)
, ArbitraryFilterAdd(..)
) where

import Test.QuickCheck

import Control.Applicative ((<$>), (<*>))

import Data.Maybe
import qualified Data.Sequence as S (fromList)
import qualified Data.ByteString as BS (ByteString, pack, drop, null)

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

-- | Arbitrary strict ByteString
data ArbitraryByteString = ArbitraryByteString BS.ByteString
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        -- to give us some with non-0 offset
        return $ ArbitraryByteString $ BS.drop n bs 

-- | Arbitrary strict ByteString that is not empty
data ArbitraryNotNullByteString = ArbitraryNotNullByteString BS.ByteString
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryNotNullByteString where
    arbitrary = do
        bs <- BS.pack `fmap` (listOf1 arbitrary)
        return $ ArbitraryNotNullByteString bs

-- | Arbitrary BigWord using arbitrarySizedBoundedIntegral
data ArbitraryBigWord n = ArbitraryBigWord (BigWord n)
    deriving (Eq, Show, Read)

instance BigWordMod n => Arbitrary (ArbitraryBigWord n) where
    arbitrary = ArbitraryBigWord <$> arbitrarySizedBoundedIntegral

type ArbitraryWord512 = ArbitraryBigWord Mod512
type ArbitraryWord256 = ArbitraryBigWord Mod256
type ArbitraryWord160 = ArbitraryBigWord Mod160
type ArbitraryWord128 = ArbitraryBigWord Mod128
type ArbitraryFieldP = ArbitraryBigWord ModP
type ArbitraryFieldN = ArbitraryBigWord ModN
type ArbitraryTxHash = ArbitraryBigWord Mod256Tx
type ArbitraryBlockHash = ArbitraryBigWord Mod256Block

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

-- | Arbitrary public key (can be both compressed or uncompressed) with its
-- corresponding private key.
data ArbitraryPubKey = ArbitraryPubKey PrvKey PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKey where
    arbitrary = oneof
        [ arbitrary >>= \(ArbitraryPubKeyC k p) -> return $ ArbitraryPubKey k p
        , arbitrary >>= \(ArbitraryPubKeyU k p) -> return $ ArbitraryPubKey k p
        ]

-- | Arbitrary compressed public key with its corresponding private key.
data ArbitraryPubKeyC = ArbitraryPubKeyC PrvKey PubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPubKeyC where
    arbitrary = do
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryPubKeyC k $ derivePubKey k

-- | Arbitrary uncompressed public key with its corresponding private key.
data ArbitraryPubKeyU = ArbitraryPubKeyU PrvKey PubKey
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
        ArbitraryBigWord i <- arbitrary
        return $ ArbitraryPubKeyAddress $ PubKeyAddress i

-- | Arbitrary script hash address
newtype ArbitraryScriptAddress = ArbitraryScriptAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryScriptAddress where
    arbitrary = do
        ArbitraryBigWord i <- arbitrary
        return $ ArbitraryScriptAddress $ ScriptAddress i

-- | Arbitrary message hash, private key, nonce and corresponding signature.
-- The signature is generated with a random message, random private key and a
-- random nonce.
data ArbitrarySignature = ArbitrarySignature Word256 PrvKey FieldN Signature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySignature where
    arbitrary = do
        ArbitraryBigWord msg  <- arbitrary
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
        ArbitraryBigWord msg  <- arbitrary
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
        ArbitraryBigWord c <- arbitrary
        ArbitraryPrvKeyC k <- arbitrary
        return $ ArbitraryXPrvKey $ XPrvKey d p i c k

-- | Arbitrary extended public key with its corresponding private key.
data ArbitraryXPubKey = ArbitraryXPubKey XPrvKey XPubKey 
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryXPubKey where
    arbitrary = do
        ArbitraryXPrvKey k <- arbitrary
        return $ ArbitraryXPubKey k $ deriveXPubKey k

-- | Arbitrary master key
data ArbitraryMasterKey = ArbitraryMasterKey MasterKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMasterKey where
    arbitrary = do
        ArbitraryByteString bs <- arbitrary
        case makeMasterKey bs of
            Just k  -> return $ ArbitraryMasterKey k
            Nothing -> arbitrary

-- | Arbitrary private account key with its corresponding master key.
data ArbitraryAccPrvKey = ArbitraryAccPrvKey MasterKey AccPrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAccPrvKey where
    arbitrary = do
        ArbitraryMasterKey m <- arbitrary
        i <- choose (0,0x7fffffff)
        case accPrvKey m i of
            Just k -> return $ ArbitraryAccPrvKey m k
            Nothing -> arbitrary

-- | Arbitrary public account key with its corresponding master key
-- and private account key.
data ArbitraryAccPubKey = 
    ArbitraryAccPubKey MasterKey AccPrvKey AccPubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAccPubKey where
    arbitrary = do
        ArbitraryAccPrvKey m k <- arbitrary
        let p = AccPubKey $ deriveXPubKey $ getAccPrvKey k
        return $ ArbitraryAccPubKey m k p

-- | Arbitrary private address key with its corresponding master key and
-- private account key.
data ArbitraryAddrPrvKey = ArbitraryAddrPrvKey MasterKey AccPrvKey AddrPrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrPrvKey where
    arbitrary = do
        ArbitraryAccPrvKey m k <- arbitrary
        i <- choose (0,0x7fffffff)
        f <- elements [extPrvKey, intPrvKey]
        case f k i of
            Just a  -> return $ ArbitraryAddrPrvKey m k a
            Nothing -> arbitrary

-- | Arbitrary public address key with its corresponding master key,
-- private account key and private address key.
data ArbitraryAddrPubKey = 
    ArbitraryAddrPubKey MasterKey AccPrvKey AddrPrvKey AddrPubKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrPubKey where
    arbitrary = do
        ArbitraryAddrPrvKey m k a <- arbitrary
        let p = AddrPubKey $ deriveXPubKey $ getAddrPrvKey a
        return $ ArbitraryAddrPubKey m k a p

-- | Arbitrary bloom filter flags
data ArbitraryBloomFlags = ArbitraryBloomFlags BloomFlags
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryBloomFlags where
    arbitrary = ArbitraryBloomFlags <$> elements 
        [ BloomUpdateNone
        , BloomUpdateAll
        , BloomUpdateP2PubKeyOnly
        ]

-- | Arbitrary bloom filter with its corresponding number of elements
-- and false positive rate.
data ArbitraryBloomFilter = ArbitraryBloomFilter Int Double BloomFilter
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryBloomFilter where
    arbitrary = do
        n     <- choose (0,100000)
        fp    <- choose (1e-8,1)
        tweak <- arbitrary
        ArbitraryBloomFlags fl <- arbitrary
        return $ ArbitraryBloomFilter n fp $ bloomCreate n fp tweak fl

-- | Arbitrary FilterLoad
data ArbitraryFilterLoad = ArbitraryFilterLoad FilterLoad
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryFilterLoad where
    arbitrary = do
        ArbitraryBloomFilter _ _ bf <- arbitrary
        return $ ArbitraryFilterLoad $ FilterLoad bf

-- | Arbitrary FilterAdd
data ArbitraryFilterAdd = ArbitraryFilterAdd FilterAdd
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryFilterAdd where
    arbitrary = do
        ArbitraryByteString bs <- arbitrary
        return $ ArbitraryFilterAdd $ FilterAdd bs

