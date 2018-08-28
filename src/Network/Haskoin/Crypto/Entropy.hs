{-# LANGUAGE OverloadedStrings #-}
-- | ECDSA Signatures
module Network.Haskoin.Crypto.Entropy
    ( SecretT
    , withSource
    , getEntropy
    , genPrvKey
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

-- | Internal state of the 'SecretT' monad
type SecretState m = (WorkingState, Int -> m ByteString)

-- | StateT monad stack tracking the internal state of HMAC DRBG
-- pseudo random number generator using SHA-256. The 'SecretT' monad is
-- run with the 'withSource' function by providing it a source of entropy.
type SecretT m = S.StateT (SecretState m) m

-- | Run a 'SecretT' monad by providing it a source of entropy. You can
-- use 'getEntropy' or provide your own entropy source function.
withSource :: Monad m => (Int -> m ByteString) -> SecretT m a -> m a
withSource f m = do
    seed  <- f 32 -- Read 256 bits from the random source
    nonce <- f 16 -- Read 128 bits from the random source
    let ws = hmacDRBGNew seed nonce "haskoin"
    S.evalStateT m (ws,f)

-- | Generate a new random 'EC.SecKey' value from the 'SecretT' monad. This
-- will invoke the HMAC DRBG routine. Of the internal entropy pool of the HMAC
-- DRBG was stretched too much, this function will reseed it.
nextSecret :: Monad m => SecretT m EC.SecKey
nextSecret = do
    (ws, f) <- S.get
    let (ws', randM) = hmacDRBGGen ws 32 "haskoin"
    case randM of
        (Just rand) -> do
            S.put (ws', f)
            case EC.secKey rand of
                Just key -> return key
                Nothing  -> nextSecret
        Nothing -> do
            seed <- lift $ f 32 -- Read 256 bits to re-seed the PRNG
            let ws0 = hmacDRBGRsd ws' seed "haskoin"
            S.put (ws0, f)
            nextSecret

-- | Produce a new 'PrvKey' randomly from the 'SecretT' monad.
genPrvKey :: Monad m => SecretT m PrvKey
genPrvKey = makePrvKey <$> nextSecret
