{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Hashing functions and HMAC DRBG definition
module Network.Haskoin.Crypto.Hash
( Hash512(getHash512)
, Hash256(getHash256)
, Hash160(getHash160)
, CheckSum32(getCheckSum32)
, sha512
, sha256
, ripemd160
, sha1
, doubleSHA256
, addressHash
, checkSum32
, hmac512
, hmac256
, split512
, join512
, hmacDRBGNew
, hmacDRBGUpd
, hmacDRBGRsd
, hmacDRBGGen
, WorkingState
, EntropyInput
, AdditionalInput
, ProvidedData
, Nonce
, PersString
) where

import           Control.DeepSeq         (NFData)
import           Crypto.Hash             (RIPEMD160 (..), SHA1 (..),
                                          SHA256 (..), SHA512 (..), hashWith)
import           Crypto.MAC.HMAC         (HMAC, hmac)
import           Data.ByteArray          (ByteArrayAccess)
import qualified Data.ByteArray          as BA
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Short   (ShortByteString)
import qualified Data.ByteString.Short   as BSS
import           Data.Either             (fromRight)
import           Data.Hashable           (Hashable)
import           Data.Serialize          (Serialize (..), decode)
import qualified Data.Serialize.Get      as Get
import qualified Data.Serialize.Put      as Put
import           Data.String             (IsString, fromString)
import           Data.String.Conversions (cs)
import           Data.Word               (Word16, Word32)
import           Network.Haskoin.Util
import           Text.Read               as R

newtype CheckSum32 = CheckSum32
    { getCheckSum32 :: Word32
    } deriving (Eq, Ord, Serialize, NFData, Show, Read, Hashable)

newtype Hash512 = Hash512 { getHash512 :: ShortByteString }
    deriving (Eq, Ord, NFData, Hashable)

newtype Hash256 = Hash256 { getHash256 :: ShortByteString }
    deriving (Eq, Ord, NFData, Hashable)

newtype Hash160 = Hash160 { getHash160 :: ShortByteString }
    deriving (Eq, Ord, NFData, Hashable)

instance Show Hash512 where
    showsPrec d k =
        showParen (d > 10) $
        showString "Hash512 " . shows (encodeHex (BSS.fromShort (getHash512 k)))

instance Read Hash512 where
    readPrec =
        parens $ do
            R.Ident "Hash512" <- lexP
            R.String str <- lexP
            maybe pfail return $ Hash512 . BSS.toShort <$> decodeHex (cs str)

instance Show Hash256 where
    showsPrec d k =
        showParen (d > 10) $
        showString "Hash256 " . shows (encodeHex (BSS.fromShort (getHash256 k)))

instance Read Hash256 where
    readPrec =
        parens $ do
            R.Ident "Hash256" <- lexP
            R.String str <- lexP
            maybe pfail return $ Hash256 . BSS.toShort <$> decodeHex (cs str)

instance Show Hash160 where
    showsPrec d k =
        showParen (d > 10) $
        showString "Hash160 " . shows (encodeHex (BSS.fromShort (getHash160 k)))

instance Read Hash160 where
    readPrec =
        parens $ do
            R.Ident "Hash160" <- lexP
            R.String str <- lexP
            maybe pfail return $ Hash160 . BSS.toShort <$> decodeHex (cs str)

instance IsString Hash512 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    64 -> Hash512 (BSS.toShort bs)
                    _  -> e
      where
        e = error "Could not decode hash from hex string"

instance Serialize Hash512 where
    get = Hash512 <$> Get.getShortByteString 64
    put = Put.putShortByteString . getHash512

instance IsString Hash256 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    32 -> Hash256 (BSS.toShort bs)
                    _  -> e
      where
        e = error "Could not decode hash from hex string"

instance Serialize Hash256 where
    get = Hash256 <$> Get.getShortByteString 32
    put = Put.putShortByteString . getHash256

instance IsString Hash160 where
    fromString str =
        case decodeHex $ cs str of
            Nothing -> e
            Just bs ->
                case BS.length bs of
                    20 -> Hash160 (BSS.toShort bs)
                    _  -> e
      where
        e = error "Could not decode hash from hex string"

instance Serialize Hash160 where
    get = Hash160 <$> Get.getShortByteString 20
    put = Put.putShortByteString . getHash160

sha512 :: ByteArrayAccess b => b -> Hash512
sha512 = Hash512 . BSS.toShort . BA.convert . hashWith SHA512

sha256 :: ByteArrayAccess b => b -> Hash256
sha256 = Hash256 . BSS.toShort . BA.convert . hashWith SHA256

ripemd160 :: ByteArrayAccess b => b -> Hash160
ripemd160 = Hash160 . BSS.toShort . BA.convert. hashWith RIPEMD160

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
checkSum32 = fromRight (error "Colud not decode bytes as CheckSum32")
             . decode
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
    Hash512 .
    BSS.toShort $
        BSS.fromShort (getHash256 a) `BS.append` BSS.fromShort (getHash256 b)


{- 10.1.2 HMAC_DRBG with HMAC-SHA256
   http://csrc.nist.gov/publications/nistpubs/800-90A/SP800-90A.pdf
   Constants are based on recommentations in Appendix D section 2 (D.2)
-}

type WorkingState    = (ByteString, ByteString, Word16)
type AdditionalInput = ByteString
type ProvidedData    = ByteString
type EntropyInput    = ByteString
type Nonce           = ByteString
type PersString      = ByteString

-- 10.1.2.2 HMAC DRBG Update FUnction
hmacDRBGUpd :: ProvidedData
            -> ByteString
            -> ByteString
            -> (ByteString, ByteString)
hmacDRBGUpd info k0 v0
    | BS.null info = (k1, v1)        -- 10.1.2.2.3
    | otherwise    = (k2, v2)        -- 10.1.2.2.6
  where
    -- 10.1.2.2.1
    k1 = BSS.fromShort . getHash256 . hmac256 k0 $ v0 `BS.append` (0 `BS.cons` info)
    -- 10.1.2.2.2
    v1 = BSS.fromShort . getHash256 $ hmac256 k1 v0
    -- 10.1.2.2.4
    k2 = BSS.fromShort . getHash256 $ hmac256 k1 $ v1 `BS.append` (1 `BS.cons` info)
    -- 10.1.2.2.5
    v2 = BSS.fromShort . getHash256 $ hmac256 k2 v1

-- 10.1.2.3 HMAC DRBG Instantiation
hmacDRBGNew :: EntropyInput -> Nonce -> PersString -> WorkingState
hmacDRBGNew seed nonce info
    | (BS.length seed + BS.length nonce) * 8 < 384  = error
        "Entropy + nonce input length must be at least 384 bit"
    | (BS.length seed + BS.length nonce) * 8 > 1000 = error
        "Entropy + nonce input length can not be greater than 1000 bit"
    | BS.length info * 8 > 256  = error
        "Maximum personalization string length is 256 bit"
    | otherwise                = (k1, v1, 1)         -- 10.1.2.3.6
  where
    s        = BS.concat [seed, nonce, info] -- 10.1.2.3.1
    k0       = BS.replicate 32 0             -- 10.1.2.3.2
    v0       = BS.replicate 32 1             -- 10.1.2.3.3
    (k1,v1)  = hmacDRBGUpd s k0 v0           -- 10.1.2.3.4

-- 10.1.2.4 HMAC DRBG Reseeding
hmacDRBGRsd :: WorkingState -> EntropyInput -> AdditionalInput -> WorkingState
hmacDRBGRsd (k, v, _) seed info
    | BS.length seed * 8 < 256 = error
        "Entropy input length must be at least 256 bit"
    | BS.length seed * 8 > 1000 = error
        "Entropy input length can not be greater than 1000 bit"
    | otherwise   = (k0, v0, 1)             -- 10.1.2.4.4
  where
    s        = seed `BS.append` info -- 10.1.2.4.1
    (k0, v0) = hmacDRBGUpd s k v     -- 10.1.2.4.2

-- 10.1.2.5 HMAC DRBG Generation
hmacDRBGGen :: WorkingState
            -> Word16
            -> AdditionalInput
            -> (WorkingState, Maybe ByteString)
hmacDRBGGen (k0, v0, c0) bytes info
    | bytes * 8 > 7500 = error "Maximum bits per request is 7500"
    | c0 > 10000       = ((k0, v0, c0), Nothing)  -- 10.1.2.5.1
    | otherwise        = ((k2, v3, c1), Just res) -- 10.1.2.5.8
  where
    (k1, v1)  | BS.null info = (k0, v0)
              | otherwise    = hmacDRBGUpd info k0 v0   -- 10.1.2.5.2
    (tmp, v2) = go (fromIntegral bytes) k1 v1 BS.empty  -- 10.1.2.5.3/4
    res       = BS.take (fromIntegral bytes) tmp        -- 10.1.2.5.5
    (k2, v3)  = hmacDRBGUpd info k1 v2                  -- 10.1.2.5.6
    c1        = c0 + 1                                  -- 10.1.2.5.7
    go l k v acc | BS.length acc >= l = (acc, v)
                 | otherwise = let vn = BSS.fromShort . getHash256 $ hmac256 k v
                               in go l k vn (acc `BS.append` vn)

