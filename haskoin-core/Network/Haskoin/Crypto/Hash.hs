-- | Hashing functions and HMAC DRBG definition
module Network.Haskoin.Crypto.Hash
( Hash512(getHash512)
, Hash256(getHash256)
, Hash160(getHash160)
, CheckSum32(getCheckSum32)
, bsToHash512
, bsToHash256
, bsToHash160
, hash512
, hash256
, hash160
, sha1
, doubleHash256
, bsToCheckSum32
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
) where

import Crypto.Hash
    ( Digest
    , SHA512
    , SHA256
    , SHA1
    , RIPEMD160
    , hash
    )
import Crypto.MAC.HMAC (hmac)

import Control.DeepSeq (NFData, rnf)
import Control.Monad ((<=<), guard)
import Data.Byteable (toBytes)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Text.Read (Lexeme(String, Ident), readPrec, lexP, parens, pfail)
import Data.Serialize (Serialize, get, put)
import Data.Serialize.Get (getByteString)
import Data.Serialize.Put (putByteString)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( null
    , append
    , cons
    , concat
    , take
    , empty
    , length
    , replicate
    , splitAt
    )

import Network.Haskoin.Util

newtype CheckSum32 = CheckSum32 { getCheckSum32 :: ByteString }
    deriving (Eq, Ord)

newtype Hash512 = Hash512 { getHash512 :: ByteString }
    deriving (Eq, Ord)

newtype Hash256 = Hash256 { getHash256 :: ByteString }
    deriving (Eq, Ord)

newtype Hash160 = Hash160 { getHash160 :: ByteString }
    deriving (Eq, Ord)


instance NFData CheckSum32 where
    rnf (CheckSum32 bs) = rnf bs

instance Show CheckSum32 where
    showsPrec d (CheckSum32 bs) = showParen (d > 10) $
        showString "CheckSum32 " . shows (encodeHex bs)

instance Read CheckSum32 where
    readPrec = parens $ do
        Ident "CheckSum32" <- lexP
        String str <- lexP
        maybe pfail return $ bsToCheckSum32 =<< decodeHex (cs str)

instance IsString CheckSum32 where
    fromString =
        fromMaybe e . (bsToCheckSum32 <=< decodeHex) . cs
      where
        e = error "Could not decode checksum"

instance Serialize CheckSum32 where
    get = CheckSum32 <$> getByteString 4
    put (CheckSum32 bs) = putByteString bs


instance NFData Hash512 where
    rnf (Hash512 bs) = rnf bs

instance Show Hash512 where
    showsPrec d (Hash512 bs) = showParen (d > 10) $
        showString "Hash512 " . shows (encodeHex bs)

instance Read Hash512 where
    readPrec = parens $ do
        Ident "Hash512" <- lexP
        String str <- lexP
        maybe pfail return $ bsToHash512 =<< decodeHex (cs str)

instance IsString Hash512 where
    fromString =
        fromMaybe e . (bsToHash512 <=< decodeHex) . cs
      where
        e = error "Could not decode 64-byte hash"

instance Serialize Hash512 where
    get = Hash512 <$> getByteString 64
    put (Hash512 bs) = putByteString bs


instance NFData Hash256 where
    rnf (Hash256 bs) = rnf bs

instance Show Hash256 where
    showsPrec d (Hash256 bs) = showParen (d > 10) $
        showString "Hash256 " . shows (encodeHex bs)

instance Read Hash256 where
    readPrec = parens $ do
        Ident "Hash256" <- lexP
        String str <- lexP
        maybe pfail return $ bsToHash256 =<< decodeHex (cs str)

instance IsString Hash256 where
    fromString =
        fromMaybe e . (bsToHash256 <=< decodeHex) . cs
      where
        e = error "Could not decode 32-byte hash"

instance Serialize Hash256 where
    get = Hash256 <$> getByteString 32
    put (Hash256 bs) = putByteString bs


instance NFData Hash160 where
    rnf (Hash160 bs) = rnf bs

instance Show Hash160 where
    showsPrec d (Hash160 bs) = showParen (d > 10) $
        showString "Hash160 " . shows (encodeHex bs)

instance Read Hash160 where
    readPrec = parens $ do
        Ident "Hash160" <- lexP
        String str <- lexP
        maybe pfail return $ bsToHash160 =<< decodeHex (cs str)

instance IsString Hash160 where
    fromString =
        fromMaybe e . (bsToHash160 <=< decodeHex) . cs
      where
        e = error "Could not decode 20-byte hash"

instance Serialize Hash160 where
    get = Hash160 <$> getByteString 20
    put (Hash160 bs) = putByteString bs


bsToHash512 :: ByteString -> Maybe Hash512
bsToHash512 bs = guard (BS.length bs == 64) >> return (Hash512 bs)

bsToHash256 :: ByteString -> Maybe Hash256
bsToHash256 bs = guard (BS.length bs == 32) >> return (Hash256 bs)

bsToHash160 :: ByteString -> Maybe Hash160
bsToHash160 bs = guard (BS.length bs == 20) >> return (Hash160 bs)

-- | Compute SHA-512.
hash512 :: ByteString -> Hash512
hash512 = Hash512 . (toBytes :: Digest SHA512 -> ByteString) . hash

-- | Compute SHA-256.
hash256 :: ByteString -> Hash256
hash256 = Hash256 . (toBytes :: Digest SHA256 -> ByteString) . hash

-- | Compute RIPEMD-160.
hash160 :: ByteString -> Hash160
hash160 = Hash160 . (toBytes :: Digest RIPEMD160 -> ByteString) . hash

-- | Compute SHA1
sha1 :: ByteString -> Hash160
sha1 = Hash160 . (toBytes :: Digest SHA1 -> ByteString) . hash

-- | Compute two rounds of SHA-256.
doubleHash256 :: ByteString -> Hash256
doubleHash256 = hash256 . getHash256 . hash256

{- CheckSum -}

bsToCheckSum32 :: ByteString -> Maybe CheckSum32
bsToCheckSum32 bs = guard (BS.length bs == 4) >> return (CheckSum32 bs)

-- | Computes a 32 bit checksum.
checkSum32 :: ByteString -> CheckSum32
checkSum32 bs =
    CheckSum32 $ BS.take 4 bs'
  where
    Hash256 bs' = doubleHash256 bs

{- HMAC -}

-- | Computes HMAC over SHA-512.
hmac512 :: ByteString -> ByteString -> Hash512
hmac512 key msg =
    Hash512 $ hmac f 128 key msg
  where
    f bs = let Hash512 bs' = hash512 bs in bs'

-- | Computes HMAC over SHA-256.
hmac256 :: ByteString -> ByteString -> Hash256
hmac256 key msg =
    Hash256 $ hmac f 64 key msg
  where
    f bs = let Hash256 bs' = hash256 bs in bs'

-- | Split a 'Hash512' into a pair of 'Hash256'.
split512 :: Hash512 -> (Hash256, Hash256)
split512 (Hash512 bs) =
    (Hash256 a, Hash256 b)
  where
    (a, b) = BS.splitAt 32 bs

-- | Join a pair of 'Hash256' into a 'Hash512'.
join512 :: (Hash256, Hash256) -> Hash512
join512 (Hash256 a, Hash256 b) = Hash512 $ a `BS.append` b


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
hmacDRBGUpd :: ProvidedData -> ByteString -> ByteString
            -> (ByteString, ByteString)
hmacDRBGUpd info k0 v0
    | BS.null info = (k1, v1)        -- 10.1.2.2.3
    | otherwise    = (k2, v2)        -- 10.1.2.2.6
  where
    -- 10.1.2.2.1
    Hash256 k1 = hmac256 k0 $ v0 `BS.append` (0 `BS.cons` info)
    -- 10.1.2.2.2
    Hash256 v1 = hmac256 k1 v0
    -- 10.1.2.2.4
    Hash256 k2 = hmac256 k1 $ v1 `BS.append` (1 `BS.cons` info)
    -- 10.1.2.2.5
    Hash256 v2 = hmac256 k2 v1

-- 10.1.2.3 HMAC DRBG Instantiation
hmacDRBGNew :: EntropyInput -> Nonce -> PersString -> WorkingState
hmacDRBGNew seed nonce info
    | (BS.length seed + BS.length nonce) * 8 < 384  = error $
        "Entropy + nonce input length must be at least 384 bit"
    | (BS.length seed + BS.length nonce) * 8 > 1000 = error $
        "Entropy + nonce input length can not be greater than 1000 bit"
    | BS.length info * 8 > 256  = error $
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
    | BS.length seed * 8 < 256 = error $
        "Entropy input length must be at least 256 bit"
    | BS.length seed * 8 > 1000 = error $
        "Entropy input length can not be greater than 1000 bit"
    | otherwise   = (k0, v0, 1)             -- 10.1.2.4.4
  where
    s        = seed `BS.append` info -- 10.1.2.4.1
    (k0, v0) = hmacDRBGUpd s k v     -- 10.1.2.4.2

-- 10.1.2.5 HMAC DRBG Generation
hmacDRBGGen :: WorkingState -> Word16 -> AdditionalInput
            -> (WorkingState, Maybe ByteString)
hmacDRBGGen (k0, v0, c0) bytes info
    | bytes * 8 > 7500 = error "Maximum bits per request is 7500"
    | c0 > 10000       = ((k0, v0, c0), Nothing)  -- 10.1.2.5.1
    | otherwise        = ((k2, v3, c1), Just res) -- 10.1.2.5.8
  where
    (k1, v1)  | BS.null info = (k0, v0)
              | otherwise    = hmacDRBGUpd info k0 v0   -- 10.1.2.5.2
    (tmp, v2) = go (fromIntegral bytes) k1 v1 BS.empty -- 10.1.2.5.3/4
    res       = BS.take (fromIntegral bytes) tmp       -- 10.1.2.5.5
    (k2, v3)  = hmacDRBGUpd info k1 v2                 -- 10.1.2.5.6
    c1        = c0 + 1                                 -- 10.1.2.5.7
    go l k v acc | BS.length acc >= l = (acc,v)
                 | otherwise = let vn = getHash256 $ hmac256 k v
                               in go l k vn (acc `BS.append` vn)

