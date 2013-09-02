module Haskoin.Wallet 
( XPrivateKey
, XPublicKey
, deriveXPublicKey
) where

import Control.Monad (liftM2, guard)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (encode, decode)
import Data.Word (Word32)
import Data.Bits (shiftR, testBit)
import Data.Maybe (fromJust, isNothing)
import qualified Data.ByteString as BS 
    ( ByteString
    , singleton
    , cons
    , append
    )
import qualified Data.ByteString.Lazy as BL (ByteString)

import Crypto.MAC.HMAC (hmac)
import Haskoin.Util 
    ( stringToBS
    , toLazyBS
    , toStrictBS
    , encode'
    )
import Haskoin.Crypto

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

type ChainCode = Hash256

data XPrivateKey = XPrivateKey 
    { xPrivateKey     :: !PrivateKey
    , xPrivChainCode  :: !ChainCode
    } deriving (Eq, Show)

data XPublicKey = XPublicKey 
    { xPublicKey    :: !PublicKey
    , xPubChainCode :: !ChainCode
    } deriving (Eq, Show)

deriveXPublicKey :: XPrivateKey -> XPublicKey
deriveXPublicKey (XPrivateKey k c) = XPublicKey (derivePublicKey k) c

hmac512 :: BS.ByteString -> BS.ByteString -> Hash512
hmac512 key msg = decode $ toLazyBS $ hmac hash512BS 512 key msg

split512 :: Hash512 -> (Hash256, Hash256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

-- encode private key as 32 bytes (big endian)
prvToBS :: PrivateKey -> BS.ByteString
prvToBS = encode' . (fromIntegral . fromPrivateKey :: PrivateKey -> Hash256)

ckd :: XPrivateKey -> Word32 -> Maybe XPrivateKey
ckd (XPrivateKey kpar cpar) i = do
    k' <- makePrivateKey $ fromIntegral k
    ki <- addPrivateKeys k' kpar
    return $ XPrivateKey ki ci
    where (k, ci) | testBit i 31 = split512 $ hmac512 (encode' cpar) cPrv
                  | otherwise    = split512 $ hmac512 (encode' cpar) cPub
          cPrv = 0x00 `BS.cons` (prvToBS kpar) `BS.append` (encode' i)
          cPub = (encode' $ derivePublicKey kpar) `BS.append` (encode' i)

ckd' :: XPublicKey -> Word32 -> Maybe XPublicKey
ckd' (XPublicKey kpar cpar) i = do
    k' <- derivePublicKey <$> makePrivateKey (fromIntegral k)
    ki <- addPublicKeys k' kpar
    return $ XPublicKey ki ci
    where (k, ci) | testBit i 31 = error $ 
                        "Private derivation is not defined for XPublicKey"
                   | otherwise    = split512 $ hmac512 (encode' cpar) cPub
          cPub  = (encode' kpar) `BS.append` (encode' i)

