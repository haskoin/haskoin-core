-- | Hashing functions and HMAC DRBG definition
module Network.Haskoin.Crypto.Hash
( CheckSum32
, hash512
, hash256
, hashSha1
, hash160
, hash512BS
, hash256BS
, hashSha1BS
, hash160BS
, doubleHash256
, doubleHash256BS
, chksum32
, hmac512
, hmac512BS
, hmac256
, hmac256BS
, hmacDRBGNew
, hmacDRBGUpd
, hmacDRBGRsd
, hmacDRBGGen
, WorkingState
, murmurHash3
, split512
, join512
, decodeCompact
, encodeCompact
) where

import Control.Monad (replicateM)

import Crypto.Hash 
    ( Digest
    , SHA512
    , SHA256
    , SHA1
    , RIPEMD160
    , hash
    )
import Crypto.MAC.HMAC (hmac)

import Data.Word (Word16, Word32)
import Data.Byteable (toBytes)
import Data.Binary (get)
import Data.Binary.Get (getWord32le)
import Data.Bits 
    ( shiftL
    , shiftR
    , rotateL
    , xor
    , (.&.), (.|.)
    )

import qualified Data.ByteString as BS 
    ( ByteString
    , null
    , append
    , cons
    , concat
    , take
    , empty
    , length
    , replicate
    , drop
    )

import Network.Haskoin.Util 
import Network.Haskoin.Crypto.BigWord 

type CheckSum32 = Word32

run512 :: BS.ByteString -> BS.ByteString
run512 = (toBytes :: Digest SHA512 -> BS.ByteString) . hash

run256 :: BS.ByteString -> BS.ByteString
run256 = (toBytes :: Digest SHA256 -> BS.ByteString) . hash

run160 :: BS.ByteString -> BS.ByteString
run160 = (toBytes :: Digest RIPEMD160 -> BS.ByteString) . hash

runSha1 :: BS.ByteString -> BS.ByteString
runSha1 = (toBytes :: Digest SHA1 -> BS.ByteString) . hash

-- | Computes SHA-512.
hash512 :: BS.ByteString -> Word512
hash512 bs = runGet' get (run512 bs)

-- | Computes SHA-512 and returns the result as a bytestring.
hash512BS :: BS.ByteString -> BS.ByteString
hash512BS bs = run512 bs

-- | Computes SHA-256.
hash256 :: BS.ByteString -> Word256
hash256 bs = runGet' get (run256 bs)

-- | Computes SHA-256 and returns the result as a bytestring.
hash256BS :: BS.ByteString -> BS.ByteString
hash256BS bs = run256 bs

-- | Computes SHA-160.
hashSha1 :: BS.ByteString -> Word160
hashSha1 bs = runGet' get (runSha1 bs)

-- | Computes SHA-160 and returns the result as a bytestring.
hashSha1BS :: BS.ByteString -> BS.ByteString
hashSha1BS bs = runSha1 bs

-- | Computes RIPEMD-160.
hash160 :: BS.ByteString -> Word160
hash160 bs = runGet' get (run160 bs)

-- | Computes RIPEMD-160 and returns the result as a bytestring.
hash160BS :: BS.ByteString -> BS.ByteString
hash160BS bs = run160 bs

-- | Computes two rounds of SHA-256.
doubleHash256 :: BS.ByteString -> Word256
doubleHash256 bs = runGet' get (run256 $ run256 bs)

-- | Computes two rounds of SHA-256 and returns the result as a bytestring.
doubleHash256BS :: BS.ByteString -> BS.ByteString
doubleHash256BS bs = run256 $ run256 bs

{- CheckSum -}

-- | Computes a 32 bit checksum.
chksum32 :: BS.ByteString -> CheckSum32
chksum32 bs = fromIntegral $ (doubleHash256 bs) `shiftR` 224

{- HMAC -}

-- | Computes HMAC over SHA-512.
hmac512 :: BS.ByteString -> BS.ByteString -> Word512
hmac512 key = decode' . (hmac512BS key)

-- | Computes HMAC over SHA-512 and return the result as a bytestring.
hmac512BS :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac512BS key msg = hmac hash512BS 128 key msg

-- | Computes HMAC over SHA-256.
hmac256 :: BS.ByteString -> BS.ByteString -> Word256
hmac256 key = decode' . (hmac256BS key)

-- | Computes HMAC over SHA-256 and return the result as a bytestring.
hmac256BS :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac256BS key msg = hmac hash256BS 64 key msg

-- | Split a 'Word512' into a pair of 'Word256'.
split512 :: Word512 -> (Word256, Word256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

-- | Join a pair of 'Word256' into a 'Word512'.
join512 :: (Word256, Word256) -> Word512
join512 (a,b) = 
    ((fromIntegral a :: Word512) `shiftL` 256) + (fromIntegral b :: Word512)

-- | Decode the compact number used in the difficulty target of a block into an
-- Integer. 
--
-- As described in the Satoshi reference implementation /src/bignum.h:
--
-- The "compact" format is a representation of a whole number N using an
-- unsigned 32bit number similar to a floating point format. The most
-- significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as "number of bytes of N". The lower 23 bits are the mantissa.
-- Bit number 24 (0x800000) represents the sign of N. 
--
-- >    N = (-1^sign) * mantissa * 256^(exponent-3)
decodeCompact :: Word32 -> Integer
decodeCompact c = 
    if neg then (-res) else res
  where
    size = fromIntegral $ c `shiftR` 24
    neg  = (c .&. 0x00800000) /= 0
    wrd  = c .&. 0x007fffff
    res | size <= 3 = (toInteger wrd) `shiftR` (8*(3 - size))
        | otherwise = (toInteger wrd) `shiftL` (8*(size - 3))

-- | Encode an Integer to the compact number format used in the difficulty 
-- target of a block.
encodeCompact :: Integer -> Word32
encodeCompact i 
    | i < 0     = c3 .|. 0x00800000
    | otherwise = c3
  where
    posi = abs i
    s1 = BS.length $ integerToBS posi
    c1 | s1 < 3    = posi `shiftL` (8*(3 - s1))
       | otherwise = posi `shiftR` (8*(s1 - 3))
    (s2,c2) | c1 .&. 0x00800000 /= 0  = (s1 + 1, c1 `shiftR` 8)
            | otherwise               = (s1, c1)
    c3 = fromIntegral $ c2 .|. ((toInteger s2) `shiftL` 24)

{- 10.1.2 HMAC_DRBG with HMAC-SHA256
   http://csrc.nist.gov/publications/nistpubs/800-90A/SP800-90A.pdf 
   Constants are based on recommentations in Appendix D section 2 (D.2)
-}

type WorkingState    = (BS.ByteString, BS.ByteString, Word16)
type AdditionalInput = BS.ByteString
type ProvidedData    = BS.ByteString
type EntropyInput    = BS.ByteString
type Nonce           = BS.ByteString
type PersString      = BS.ByteString

-- 10.1.2.2 HMAC DRBG Update FUnction
hmacDRBGUpd :: ProvidedData -> BS.ByteString -> BS.ByteString
            -> (BS.ByteString, BS.ByteString)
hmacDRBGUpd info k0 v0 
    | BS.null info = (k1,v1) -- 10.1.2.2.3
    | otherwise    = (k2,v2) -- 10.1.2.2.6
  where 
    k1 = hmac256BS k0 $ v0 `BS.append` (0 `BS.cons` info) -- 10.1.2.2.1
    v1 = hmac256BS k1 v0                                  -- 10.1.2.2.2
    k2 = hmac256BS k1 $ v1 `BS.append` (1 `BS.cons` info) -- 10.1.2.2.4
    v2 = hmac256BS k2 v1                                  -- 10.1.2.2.5

-- 10.1.2.3 HMAC DRBG Instantiation
hmacDRBGNew :: EntropyInput -> Nonce -> PersString -> WorkingState
hmacDRBGNew seed nonce info 
    | (BS.length seed + BS.length nonce) * 8 < 384  = error $
        "Entropy + nonce input length must be at least 384 bit"
    | (BS.length seed + BS.length nonce) * 8 > 1000 = error $
        "Entropy + nonce input length can not be greater than 1000 bit"
    | BS.length info * 8 > 256  = error $ 
        "Maximum personalization string length is 256 bit"
    | otherwise                = (k1,v1,1)         -- 10.1.2.3.6
  where 
    s        = BS.concat [seed, nonce, info] -- 10.1.2.3.1
    k0       = BS.replicate 32 0             -- 10.1.2.3.2
    v0       = BS.replicate 32 1             -- 10.1.2.3.3
    (k1,v1)  = hmacDRBGUpd s k0 v0           -- 10.1.2.3.4

-- 10.1.2.4 HMAC DRBG Reseeding
hmacDRBGRsd :: WorkingState -> EntropyInput -> AdditionalInput -> WorkingState
hmacDRBGRsd (k,v,_) seed info 
    | BS.length seed * 8 < 256 = error $ 
        "Entropy input length must be at least 256 bit"
    | BS.length seed * 8 > 1000 = error $ 
        "Entropy input length can not be greater than 1000 bit"
    | otherwise   = (k0,v0,1)             -- 10.1.2.4.4
  where 
    s       = seed `BS.append` info -- 10.1.2.4.1
    (k0,v0) = hmacDRBGUpd s k v     -- 10.1.2.4.2

-- 10.1.2.5 HMAC DRBG Generation
hmacDRBGGen :: WorkingState -> Word16 -> AdditionalInput
            -> (WorkingState, Maybe BS.ByteString)
hmacDRBGGen (k0,v0,c0) bytes info 
    | bytes * 8 > 7500 = error "Maximum bits per request is 7500"
    | c0 > 10000       = ((k0,v0,c0), Nothing)  -- 10.1.2.5.1
    | otherwise        = ((k2,v3,c1), Just res) -- 10.1.2.5.8
  where 
    (k1,v1) | BS.null info = (k0,v0) 
            | otherwise    = hmacDRBGUpd info k0 v0   -- 10.1.2.5.2
    (tmp,v2) = go (fromIntegral bytes) k1 v1 BS.empty -- 10.1.2.5.3/4
    res      = BS.take (fromIntegral bytes) tmp       -- 10.1.2.5.5
    (k2,v3)  = hmacDRBGUpd info k1 v2                 -- 10.1.2.5.6
    c1       = c0 + 1                                 -- 10.1.2.5.7
    go l k v acc | BS.length acc >= l = (acc,v)
                 | otherwise = let vn = hmac256BS k v 
                                   in go l k vn (acc `BS.append` vn)

{- MurmurHash3 -}
    
-- | MurmurHash3 (x86_32). For more details, see
-- <http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp>
-- This code is used in the bloom filters of SPV nodes.
murmurHash3 :: Word32 -> BS.ByteString -> Word32
murmurHash3 nHashSeed bs = h8
  where
    -- Block and tail sizes
    nBlocks = BS.length bs `div` 4
    nTail   = BS.length bs `mod` 4
    -- Data objects
    blocks  = runGet' (replicateM nBlocks getWord32le) bs
    bsTail  = BS.drop (nBlocks*4) bs `BS.append` BS.replicate (4-nTail) 0
    -- Body
    h1   = foldl mix nHashSeed blocks
    -- Tail
    t1   = runGet' getWord32le bsTail
    t2   = t1 * c1
    t3   = t2 `rotateL` 15
    t4   = t3 * c2
    h2   = h1 `xor` t4
    -- Finalization
    h3   = h2 `xor` (fromIntegral $ BS.length bs)
    h4   = h3 `xor` (h3 `shiftR` 16) 
    h5   = h4 * 0x85ebca6b
    h6   = h5 `xor` (h5 `shiftR` 13)
    h7   = h6 * 0xc2b2ae35
    h8   = h7 `xor` (h7 `shiftR` 16)
    -- Mix function
    mix r1 k1 = r4
      where
        k2 = k1 * c1
        k3 = k2 `rotateL` 15
        k4 = k3 * c2
        r2 = r1 `xor` k4
        r3 = r2 `rotateL` 13
        r4 = r3*5 + 0xe6546b64
    -- Constants
    c1 = 0xcc9e2d51 
    c2 = 0x1b873593 

