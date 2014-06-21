-- | ECDSA Signatures
module Network.Haskoin.Crypto.ECDSA
( SecretT
, Signature(..)
, withSource
, devURandom
, devRandom
, signMsg
, detSignMsg
, unsafeSignMsg
, verifySig
, genPrvKey
, isCanonicalHalfOrder
) where

import System.IO

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM, guard, unless)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Applicative (Applicative, (<*>), (<$>))
import qualified Control.Monad.State as S
    ( StateT
    , evalStateT
    , get, put
    )

import Data.Maybe (fromJust, fromMaybe)
import Data.Binary (Binary, get, put)
import Data.Binary.Put (putWord8, putByteString)
import Data.Binary.Get (getWord8)

import qualified Data.ByteString as BS 
    ( ByteString
    , length
    , hGet
    , empty
    )
  
import Network.Haskoin.Util 
import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Crypto.Keys 
import Network.Haskoin.Crypto.Point 
import Network.Haskoin.Crypto.BigWord 

-- | Internal state of the 'SecretT' monad
type SecretState m = (WorkingState, (Int -> m BS.ByteString))

-- | StateT monad stack tracking the internal state of HMAC DRBG 
-- pseudo random number generator using SHA-256. The 'SecretT' monad is
-- run with the 'withSource' function by providing it a source of entropy.
type SecretT m a = S.StateT (SecretState m) m a

-- | Run a 'SecretT' monad by providing it a source of entropy. You can
-- use 'devURandom', 'devRandom' or provide your own entropy source function.
withSource :: Monad m => (Int -> m BS.ByteString) -> SecretT m a -> m a
withSource f m = do
    seed  <- f 32 -- Read 256 bits from the random source
    nonce <- f 16 -- Read 128 bits from the random source
    let ws = hmacDRBGNew seed nonce (stringToBS haskoinUserAgent)
    S.evalStateT m (ws,f)

-- | \/dev\/urandom entropy source. This is only available on machines
-- supporting it. This function is meant to be used together with 'withSource'.
devURandom :: Int -> IO BS.ByteString
devURandom i = withBinaryFile "/dev/urandom" ReadMode $ flip BS.hGet i

-- | \/dev\/random entropy source. This is only available on machines
-- supporting it. This function is meant to be used together with 'withSource'.
devRandom :: Int -> IO BS.ByteString
devRandom i = withBinaryFile "/dev/random" ReadMode $ flip BS.hGet i

-- | Generate a new random 'FieldN' value from the 'SecretT' monad. This will
-- invoke the HMAC DRBG routine. Of the internal entropy pool of the HMAC DRBG
-- was stretched too much, this function will reseed it.
nextSecret :: Monad m => SecretT m FieldN
nextSecret = do
    (ws,f) <- S.get
    let (ws',randM) = hmacDRBGGen ws 32 (stringToBS haskoinUserAgent)
    case randM of
        (Just rand) -> do
            S.put (ws',f)
            let randI = bsToInteger rand
            if isIntegerValidKey randI
                then return $ fromInteger randI
                else nextSecret
        Nothing -> do
            seed <- lift $ f 32 -- Read 256 bits to re-seed the PRNG
            let ws0 = hmacDRBGRsd ws' seed (stringToBS haskoinUserAgent)
            S.put (ws0,f)
            nextSecret

-- | Produce a new 'PrvKey' randomly from the 'SecretT' monad.
genPrvKey :: Monad m => SecretT m PrvKey
genPrvKey = liftM (fromJust . makePrvKey . toInteger) nextSecret
        
-- Section 3.2.1 http://www.secg.org/download/aid-780/sec1-v2.pdf
-- Produce a new private/public key pair from the 'SecretT' monad.
genKeyPair :: Monad m => SecretT m (FieldN, Point)
genKeyPair = do
    -- 3.2.1.1 
    d <- nextSecret
    -- 3.2.1.2
    let q = mulPoint d curveG
    -- 3.2.1.3
    return (d,q)

-- | Data type representing an ECDSA signature. 
data Signature = 
    Signature { sigR :: !FieldN 
              , sigS :: !FieldN 
              }
    deriving (Read, Show, Eq)

instance NFData Signature where
    rnf (Signature r s) = rnf r `seq` rnf s

-- Section 4.1.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
-- | Safely sign a message inside the 'SecretT' monad. The 'SecretT' monad will
-- generate a new nonce for each signature.
signMsg :: Monad m => Hash256 -> PrvKey -> SecretT m Signature
signMsg _ (PrvKey  0) = error "signMsg: Invalid private key 0"
signMsg _ (PrvKeyU 0) = error "signMsg: Invalid private key 0"
signMsg h d = do
    -- 4.1.3.1
    (k,p) <- genKeyPair
    case unsafeSignMsg h (prvKeyFieldN d) (k,p) of
        (Just sig) -> return sig
        -- If signing failed, retry with a new nonce
        Nothing    -> signMsg h d

-- | Sign a message using ECDSA deterministic signatures as defined by
-- RFC 6979 <http://tools.ietf.org/html/rfc6979>
detSignMsg :: Hash256 -> PrvKey -> Signature
detSignMsg _ (PrvKey  0) = error "detSignMsg: Invalid private key 0"
detSignMsg _ (PrvKeyU 0) = error "detSignMsg: Invalid private key 0"
detSignMsg h d = go $ hmacDRBGNew (runPut' $ putPrvKey d) (encode' h) BS.empty
  where 
    go ws = case hmacDRBGGen ws 32 BS.empty of
        (_, Nothing)  -> error "detSignMsg: No suitable K value found"
        (ws', Just k) -> 
            let kI   = bsToInteger k
                p    = mulPoint (fromInteger kI) curveG
                sigM = unsafeSignMsg h (prvKeyFieldN d) (fromInteger kI,p)
                in if isIntegerValidKey kI
                       then fromMaybe (go ws') sigM
                       else go ws'
          
-- Signs a message by providing the nonce
-- Re-using the same nonce twice will expose the private keys
-- Use signMsg within the SecretT monad or detSignMsg instead
-- Section 4.1.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
unsafeSignMsg :: Hash256 -> FieldN -> (FieldN, Point) -> Maybe Signature
unsafeSignMsg _ 0 _ = Nothing
unsafeSignMsg h d (k,p) = do
    -- 4.1.3.1 (4.1.3.2 not required)
    (x,_) <- getAffine p
    -- 4.1.3.3
    let r = toFieldN x
    guard (r /= 0)
    -- 4.1.3.4 / 4.1.3.5
    let e = toFieldN h
    -- 4.1.3.6
    let s' = (e + r*d)/k
        -- Canonicalize signatures: s <= order/2
        -- maxBound/2 = (maxBound+1)/2 = order/2 (because order is odd)
        s  = if s' > (maxBound `div` 2) then (-s') else s'
    guard (s /= 0)
    -- 4.1.3.7
    return $ Signature r s

-- Section 4.1.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
-- | Verify an ECDSA signature
verifySig :: Hash256 -> Signature -> PubKey -> Bool
-- 4.1.4.1 (r and s can not be zero)
verifySig _ (Signature 0 _) _ = False
verifySig _ (Signature _ 0) _ = False
verifySig h (Signature r s) q = case getAffine p of
    Nothing      -> False
    -- 4.1.4.7 / 4.1.4.8
    (Just (x,_)) -> (toFieldN x) == r
  where 
    -- 4.1.4.2 / 4.1.4.3
    e  = toFieldN h
    -- 4.1.4.4
    s' = inverseN s
    u1 = e*s'
    u2 = r*s'
    -- 4.1.4.5 (u1*G + u2*q)
    p  = shamirsTrick u1 curveG u2 (pubKeyPoint q)

-- | Returns True if the S component of a Signature is <= order/2.
-- Signatures need to pass this test to be canonical.
isCanonicalHalfOrder :: Signature -> Bool
isCanonicalHalfOrder (Signature _ s) = s <= maxBound `div` 2

instance Binary Signature where
    get = do
        t <- getWord8
        -- 0x30 is DER sequence type
        unless (t == 0x30) (fail $ 
            "Bad DER identifier byte " ++ (show t) ++ ". Expecting 0x30")
        l <- getWord8
        -- Length = (33 + 1 identifier byte + 1 length byte) * 2
        unless (l <= 70) (fail $
            "Bad DER length " ++ (show t) ++ ". Expecting length <= 70")
        isolate (fromIntegral l) $ do
            Signature <$> get <*> get

    put (Signature 0 _) = error "0 is an invalid r value in a Signature"
    put (Signature _ 0) = error "0 is an invalid s value in a Signature"
    put (Signature r s) = do
        putWord8 0x30
        let c = runPut' $ put r >> put s
        putWord8 (fromIntegral $ BS.length c)
        putByteString c


