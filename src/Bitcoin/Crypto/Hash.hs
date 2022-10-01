{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Hashing functions and corresponding data types. Uses functions from the
-- cryptonite library.
module Bitcoin.Crypto.Hash (
    -- * Hashes
    Hash512 (getHash512),
    Hash256 (getHash256),
    Hash160 (getHash160),
    CheckSum32 (getCheckSum32),
    sha512,
    sha256,
    ripemd160,
    sha1,
    doubleSHA256,
    addressHash,
    checkSum32,
    hmac512,
    hmac256,
    split512,
    join512,
    initTaggedHash,
) where

import Control.DeepSeq
import Crypto.Hash (
    Context,
    RIPEMD160 (..),
    SHA1 (..),
    SHA256 (..),
    SHA512 (..),
    hashInit,
    hashUpdates,
    hashWith,
 )
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Binary (Binary (..))
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import Data.Bytes.Serial (Serial (..))
import Data.Either (fromRight)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Bitcoin.Util
import Text.Read as R


-- | 'Word32' wrapped for type-safe 32-bit checksums.
newtype CheckSum32 = CheckSum32
    { getCheckSum32 :: Word32
    }
    deriving (Eq, Ord, Serial, Show, Read, Hashable, Generic, NFData)


instance Serialize CheckSum32 where
    put = serialize
    get = deserialize


instance Binary CheckSum32 where
    put = serialize
    get = deserialize


-- | Type for 512-bit hashes.
newtype Hash512 = Hash512 {getHash512 :: ShortByteString}
    deriving (Eq, Ord, Hashable, Generic, NFData)


-- | Type for 256-bit hashes.
newtype Hash256 = Hash256 {getHash256 :: ShortByteString}
    deriving (Eq, Ord, Hashable, Generic, NFData)


-- | Type for 160-bit hashes.
newtype Hash160 = Hash160 {getHash160 :: ShortByteString}
    deriving (Eq, Ord, Hashable, Generic, NFData)


instance Show Hash512 where
    showsPrec _ = shows . encodeHex . BSS.fromShort . getHash512


instance Read Hash512 where
    readPrec = do
        R.String str <- lexP
        maybe pfail (return . Hash512 . BSS.toShort) (decodeHex (cs str))


instance Show Hash256 where
    showsPrec _ = shows . encodeHex . BSS.fromShort . getHash256


instance Read Hash256 where
    readPrec = do
        R.String str <- lexP
        maybe pfail (return . Hash256 . BSS.toShort) (decodeHex (cs str))


instance Show Hash160 where
    showsPrec _ = shows . encodeHex . BSS.fromShort . getHash160


instance Read Hash160 where
    readPrec = do
        R.String str <- lexP
        maybe pfail (return . Hash160 . BSS.toShort) (decodeHex (cs str))


instance IsString Hash512 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    64 -> Hash512 (BSS.toShort bs)
                    _ -> e
      where
        e = error "Could not decode hash from hex string"


instance Serial Hash512 where
    deserialize = Hash512 . BSS.toShort <$> Get.getByteString 64
    serialize = Put.putByteString . BSS.fromShort . getHash512


instance Serialize Hash512 where
    put = serialize
    get = deserialize


instance Binary Hash512 where
    put = serialize
    get = deserialize


instance IsString Hash256 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    32 -> Hash256 (BSS.toShort bs)
                    _ -> e
      where
        e = error "Could not decode hash from hex string"


instance Serial Hash256 where
    deserialize = Hash256 . BSS.toShort <$> Get.getByteString 32
    serialize = Put.putByteString . BSS.fromShort . getHash256


instance Serialize Hash256 where
    put = serialize
    get = deserialize


instance Binary Hash256 where
    put = serialize
    get = deserialize


instance IsString Hash160 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    20 -> Hash160 (BSS.toShort bs)
                    _ -> e
      where
        e = error "Could not decode hash from hex string"


instance Serial Hash160 where
    deserialize = Hash160 . BSS.toShort <$> Get.getByteString 20
    serialize = Put.putByteString . BSS.fromShort . getHash160


instance Serialize Hash160 where
    put = serialize
    get = deserialize


instance Binary Hash160 where
    put = serialize
    get = deserialize


-- | Calculate SHA512 hash.
sha512 :: ByteArrayAccess b => b -> Hash512
sha512 = Hash512 . BSS.toShort . BA.convert . hashWith SHA512


-- | Calculate SHA256 hash.
sha256 :: ByteArrayAccess b => b -> Hash256
sha256 = Hash256 . BSS.toShort . BA.convert . hashWith SHA256


-- | Calculate RIPEMD160 hash.
ripemd160 :: ByteArrayAccess b => b -> Hash160
ripemd160 = Hash160 . BSS.toShort . BA.convert . hashWith RIPEMD160


-- | Claculate SHA1 hash.
sha1 :: ByteArrayAccess b => b -> Hash160
sha1 = Hash160 . BSS.toShort . BA.convert . hashWith SHA1


-- | Compute two rounds of SHA-256.
doubleSHA256 :: ByteArrayAccess b => b -> Hash256
doubleSHA256 =
    Hash256 . BSS.toShort . BA.convert . hashWith SHA256 . hashWith SHA256


-- | Compute SHA-256 followed by RIPMED-160.
addressHash :: ByteArrayAccess b => b -> Hash160
addressHash =
    Hash160 . BSS.toShort . BA.convert . hashWith RIPEMD160 . hashWith SHA256


{- CheckSum -}

-- | Computes a 32 bit checksum.
checkSum32 :: ByteArrayAccess b => b -> CheckSum32
checkSum32 =
    fromRight (error "Could not decode bytes as CheckSum32")
        . Get.runGetS deserialize
        . BS.take 4
        . BA.convert
        . hashWith SHA256
        . hashWith SHA256


{- HMAC -}

-- | Computes HMAC over SHA-512.
hmac512 :: ByteString -> ByteString -> Hash512
hmac512 key msg =
    Hash512 $ BSS.toShort $ BA.convert (hmac key msg :: HMAC SHA512)


-- | Computes HMAC over SHA-256.
hmac256 :: (ByteArrayAccess k, ByteArrayAccess m) => k -> m -> Hash256
hmac256 key msg =
    Hash256 $ BSS.toShort $ BA.convert (hmac key msg :: HMAC SHA256)


-- | Split a 'Hash512' into a pair of 'Hash256'.
split512 :: Hash512 -> (Hash256, Hash256)
split512 h =
    (Hash256 (BSS.toShort a), Hash256 (BSS.toShort b))
  where
    (a, b) = BS.splitAt 32 . BSS.fromShort $ getHash512 h


-- | Join a pair of 'Hash256' into a 'Hash512'.
join512 :: (Hash256, Hash256) -> Hash512
join512 (a, b) =
    Hash512
        . BSS.toShort
        $ BSS.fromShort (getHash256 a) `BS.append` BSS.fromShort (getHash256 b)


-- | Initialize tagged hash specified in BIP340
--
-- @since 0.21.0
initTaggedHash ::
    -- | Hash tag
    ByteString ->
    Context SHA256
initTaggedHash tag =
    (`hashUpdates` [hashedTag, hashedTag]) $
        hashInit @SHA256
  where
    hashedTag = hashWith SHA256 tag
