{-|
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto
( ArbitraryHash512(..)
, ArbitraryHash256(..)
, ArbitraryHash160(..)
, ArbitraryCheckSum32(..)
, ArbitraryByteString(..)
, ArbitraryNotNullByteString(..)
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
, ArbitraryXPrvKey(..)
, ArbitraryXPubKey(..)
, ArbitraryHardPath(..)
, ArbitrarySoftPath(..)
, ArbitraryDerivPath(..)
, ArbitraryParsedPath(..)

) where

import Test.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , oneof
    , vectorOf
    , listOf
    )

import Crypto.Secp256k1 ()

import Data.Bits (clearBit)
import qualified Data.ByteString as BS (pack)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Network.Haskoin.Test.Util
import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.ExtendedKeys
import Data.List (foldl')

newtype ArbitraryHash160 = ArbitraryHash160 Hash160
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryHash160 where
    arbitrary = (ArbitraryHash160 . fromMaybe e . bsToHash160 . BS.pack) <$>
        vectorOf 20 arbitrary
      where
        e = error "Could not read arbitrary 20-byte hash"

newtype ArbitraryHash256 = ArbitraryHash256 Hash256
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryHash256 where
    arbitrary = (ArbitraryHash256 . fromMaybe e . bsToHash256 . BS.pack) <$>
        vectorOf 32 arbitrary
      where
        e = error "Could not read arbitrary 32-byte hash"

newtype ArbitraryHash512 = ArbitraryHash512 Hash512
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryHash512 where
    arbitrary = (ArbitraryHash512 . fromMaybe e . bsToHash512 . BS.pack) <$>
        vectorOf 64 arbitrary
      where
        e = error "Could not read arbitrary 64-byte hash"

newtype ArbitraryCheckSum32 = ArbitraryCheckSum32 CheckSum32
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryCheckSum32 where
    arbitrary = (ArbitraryCheckSum32 . fromMaybe e . bsToCheckSum32 . BS.pack) <$>
        vectorOf 4 arbitrary
      where
        e = error "Could not read arbitrary checksum"

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
        i <- arbitrary
        return $ ArbitraryPrvKeyC $ makePrvKeyC i

-- | Arbitrary uncompressed private key
newtype ArbitraryPrvKeyU = ArbitraryPrvKeyU PrvKeyU
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPrvKeyU where
    arbitrary = do
        i <- arbitrary
        return $ ArbitraryPrvKeyU $ makePrvKeyU i

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
        ArbitraryHash160 i <- arbitrary
        return $ ArbitraryPubKeyAddress $ PubKeyAddress i

-- | Arbitrary script hash address
newtype ArbitraryScriptAddress = ArbitraryScriptAddress Address
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryScriptAddress where
    arbitrary = do
        ArbitraryHash160 i <- arbitrary
        return $ ArbitraryScriptAddress $ ScriptAddress i

-- | Arbitrary message hash, private key, nonce and corresponding signature.
-- The signature is generated with a random message, random private key and a
-- random nonce.
data ArbitrarySignature = ArbitrarySignature Hash256 PrvKey Signature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySignature where
    arbitrary = do
        ArbitraryHash256 msg <- arbitrary
        ArbitraryPrvKey key <- arbitrary
        let sig = signMsg msg key
        return $ ArbitrarySignature msg key sig

-- | Arbitrary extended private key.
data ArbitraryXPrvKey = ArbitraryXPrvKey XPrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryXPrvKey where
    arbitrary = do
        d <- arbitrary
        p <- arbitrary
        i <- arbitrary
        ArbitraryHash256 c <- arbitrary
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

data ArbitraryBip32PathIndex = ArbitraryBip32PathIndex Bip32PathIndex
    deriving (Show,Eq)

instance Arbitrary ArbitraryBip32PathIndex where
    arbitrary = 
        ArbitraryBip32PathIndex <$> oneof [soft, hard]
        where soft = Bip32SoftIndex <$> genIndex
              hard = Bip32HardIndex <$> genIndex

data ArbitraryHardPath = ArbitraryHardPath HardPath
    deriving (Show, Eq)

instance Arbitrary ArbitraryHardPath where
    arbitrary =
        ArbitraryHardPath . foldl' (:|) Deriv <$> listOf genIndex


data ArbitrarySoftPath = ArbitrarySoftPath SoftPath
    deriving (Show, Eq)

instance Arbitrary ArbitrarySoftPath where
    arbitrary =
        ArbitrarySoftPath . foldl' (:/) Deriv <$> listOf genIndex

data ArbitraryDerivPath = ArbitraryDerivPath DerivPath
    deriving (Show, Eq)

instance Arbitrary ArbitraryDerivPath where    
    arbitrary = ArbitraryDerivPath . concatBip32Segments . map (\(ArbitraryBip32PathIndex i) -> i ) <$> arbitrary  
        

data ArbitraryParsedPath = ArbitraryParsedPath ParsedPath 
  deriving (Show, Eq)

instance Arbitrary ArbitraryParsedPath where
    arbitrary = do
        ArbitraryDerivPath d <- arbitrary 
        ArbitraryParsedPath <$> oneof [ pure $ ParsedPrv d
                                      , pure $ ParsedPub d
                                      , pure $ ParsedEmpty d ]


