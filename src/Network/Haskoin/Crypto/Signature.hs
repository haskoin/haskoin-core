{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Crypto.Signature
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

ECDSA signatures using secp256k1 curve. Uses functions from upstream secp256k1
library.
-}
module Network.Haskoin.Crypto.Signature
    ( putSig
    , getSig
    , signHash
    , verifyHashSig
    , isCanonicalHalfOrder
    , decodeStrictSig
    , exportSig
    ) where

import           Control.Monad               (guard, unless, when)
import           Crypto.Secp256k1
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.ByteString.Short       (toShort)
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              as S
import           Data.Serialize.Put          (Putter, putByteString)
import           Network.Haskoin.Crypto.Hash
import           Numeric                     (showHex)

-- | Convert 256-bit hash into a 'Msg' for signing or verification.
hashToMsg :: Hash256 -> Msg
hashToMsg =
    fromMaybe e . msg . encode
  where
    e = error "Could not convert 32-byte hash to secp256k1 message"

-- | Sign a 256-bit hash using secp256k1 elliptic curve.
signHash :: SecKey -> Hash256 -> Sig
signHash k = signMsg k . hashToMsg

-- | Verify an ECDSA signature for a 256-bit hash.
verifyHashSig :: Hash256 -> Sig -> PubKey -> Bool
verifyHashSig h s p =
    verifySig p g m
  where
    (g, _) = normalizeSig s
    m = hashToMsg h

-- | Deserialize an ECDSA signature as commonly encoded in Bitcoin.
getSig :: Get Sig
getSig = do
        l <- lookAhead $ do
            t <- getWord8
            -- 0x30 is DER sequence type
            unless (t == 0x30) $ fail $
                "Bad DER identifier byte 0x" ++ showHex t ". Expecting 0x30"
            l <- getWord8
            when (l == 0x00) $ fail "Indeterminate form unsupported"
            when (l >= 0x80) $ fail "Multi-octect length not supported"
            return $ fromIntegral l
        bs <- getByteString $ l + 2
        case decodeStrictSig bs of
            Just s  -> return s
            Nothing -> fail "Invalid signature"

-- | Serialize an ECDSA signature for Bitcoin use.
putSig :: Putter Sig
putSig s = putByteString $ exportSig s

-- | Is canonical half order.
isCanonicalHalfOrder :: Sig -> Bool
isCanonicalHalfOrder = not . snd . normalizeSig

-- | Decode signature strictly.
decodeStrictSig :: ByteString -> Maybe Sig
decodeStrictSig bs = do
    g <- importSig bs
    let compact = exportCompactSig g
    -- <http://www.secg.org/sec1-v2.pdf Section 4.1.4>
    -- 4.1.4.1 (r and s can not be zero)
    guard $ getCompactSigR compact /= zero
    guard $ getCompactSigS compact /= zero
    guard $ isCanonicalHalfOrder g
    return g
  where
    zero = toShort $ BS.replicate 32 0
