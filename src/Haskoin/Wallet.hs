module Haskoin.Wallet 
( XPrivateKey
, XPublicKey
, deriveXPublicKey
, hmac512
, split512
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
privToBS :: PrivateKey -> BL.ByteString
privToBS = encode . (fromIntegral . fromPrivateKey :: PrivateKey -> Hash256)

privateDerivation :: XPrivateKey -> Word32 -> Maybe XPrivateKey
privateDerivation (XPrivateKey kpar cpar) i = do
    ki <- kM
    guard (ki /= 0)
    return $ XPrivateKey (fromJust $ makePrivateKey $ fromIntegral ki) ir
    where 
        kM = liftM2 (+) ilM (Just kn)
        kn = fromInteger (fromPrivateKey kpar) :: FieldN
        ilM | isIntegerValidKey (fromIntegral il) 
                = Just (fromIntegral il :: FieldN)
            | otherwise = Nothing
        (il, ir) 
              -- Private derivation is used
            | testBit i 31 = split512 $ hmac512 encPar cPrv
              -- Public derivation is used
            | otherwise    = split512 $ hmac512 encPar cPub
        cPrv   = 0x00 `BS.cons` encPrv `BS.append` encI
        cPub   = encPub `BS.append` encI
        encI   = toStrictBS $ encode i
        encPrv = toStrictBS $ privToBS kpar
        encPub = toStrictBS $ encode $ derivePublicKey kpar
        encPar = toStrictBS $ encode cpar




