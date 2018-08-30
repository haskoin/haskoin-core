{-# LANGUAGE OverloadedStrings #-}
-- | ECDSA Signatures
module Network.Haskoin.Crypto.Signature
    ( Signature(..)
    , signMsg
    , verifySig
    , isCanonicalHalfOrder
    , decodeLaxSig
    , decodeStrictSig
    ) where

import           Control.DeepSeq             (NFData, rnf)
import           Control.Monad               (guard, unless, when)
import qualified Control.Monad.State         as S (StateT, evalStateT, get, put)
import           Control.Monad.Trans         (lift)
import qualified Crypto.Secp256k1            as EC
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.ByteString.Short       (toShort)
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              (Serialize, encode, get, put)
import           Data.Serialize.Get          (getByteString, getWord8,
                                              lookAhead)
import           Data.Serialize.Put          (putByteString)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Keys.Types
import           Numeric                     (showHex)
import           System.Entropy              (getEntropy)

-- | Data type representing an ECDSA signature.
newtype Signature = Signature { getSignature :: EC.Sig }
    deriving (Read, Show, Eq)

instance NFData Signature where
    rnf (Signature s) = s `seq` ()

-- | Convert 256-bit key into an 'EC.Msg' for signing or verification.
hashToMsg :: Hash256 -> EC.Msg
hashToMsg =
    fromMaybe e . EC.msg . encode
  where
    e = error "Could not convert 32-byte hash to secp256k1 message"

-- <http://www.secg.org/sec1-v2.pdf Section 4.1.3>
-- | Sign a message
signMsg :: Hash256 -> PrvKeyI c -> Signature
signMsg h d = Signature $ EC.signMsg (prvKeySecKey d) (hashToMsg h)

-- | Verify an ECDSA signature
verifySig :: Hash256 -> Signature -> PubKeyI c -> Bool
verifySig h s q =
    EC.verifySig p g m
  where
    (g, _) = EC.normalizeSig $ getSignature s
    m = hashToMsg h
    p = pubKeyPoint q

instance Serialize Signature where
    get = do
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
        case decodeLaxSig bs of
            Just s  -> return s
            Nothing -> fail "Invalid signature"

    put (Signature s) = putByteString $ EC.exportSig s

-- | Is canonical half order.
isCanonicalHalfOrder :: Signature -> Bool
isCanonicalHalfOrder = not . snd . EC.normalizeSig . getSignature

-- | Decode signature (not strictly).
decodeLaxSig :: ByteString -> Maybe Signature
decodeLaxSig bs = Signature <$> EC.laxImportSig bs

-- | Decode signature strictly.
decodeStrictSig :: ByteString -> Maybe Signature
decodeStrictSig bs = do
    g <- EC.importSig bs
    let compact = EC.exportCompactSig g
        sig = Signature g
    -- <http://www.secg.org/sec1-v2.pdf Section 4.1.4>
    -- 4.1.4.1 (r and s can not be zero)
    guard $ EC.getCompactSigR compact /= zero
    guard $ EC.getCompactSigS compact /= zero
    guard $ isCanonicalHalfOrder sig
    return sig
  where
    zero = toShort $ BS.replicate 32 0
