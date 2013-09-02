module Haskoin.Wallet 
( Wallet(..)
, publicWallet
, isPubWallet
, isPrvWallet
, subkey
, subkey'
) where

import Control.Monad (liftM2, guard)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (encode, decode)
import Data.Word (Word32)
import Data.Bits (shiftR, setBit)
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

data DerType = PrvDer | PubDer

data Wallet = 
    XPrivateKey
        { xPrivateKey :: !PrivateKey
        , xChainCode  :: !ChainCode
        , xDepth      :: !Word32
        , xIndex      :: !Word32
        , xParent     :: !Word32
        } 
    | XPublicKey 
        { xPublicKey  :: !PublicKey
        , xChainCode  :: !ChainCode
        , xDepth      :: !Word32
        , xIndex      :: !Word32
        , xParent     :: !Word32
        } 
    deriving (Eq, Show)

publicWallet :: Wallet -> Wallet
publicWallet (XPrivateKey k c d i p) = XPublicKey (derivePublicKey k) c d i p
publicWallet pub = pub

isPubWallet :: Wallet -> Bool
isPubWallet (XPublicKey  _ _ _ _ _) = True
isPubWallet (XPrivateKey _ _ _ _ _) = False

isPrvWallet :: Wallet -> Bool
isPrvWallet = not . isPubWallet

hmac512 :: BS.ByteString -> BS.ByteString -> Hash512
hmac512 key msg = decode $ toLazyBS $ hmac hash512BS 512 key msg

split512 :: Hash512 -> (Hash256, Hash256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

-- encode private key as 32 bytes (big endian)
prvToBS :: PrivateKey -> BS.ByteString
prvToBS = (BS.cons 0x00) . encode' . getKey
    where getKey :: PrivateKey -> Hash256 
          getKey = fromIntegral . fromPrivateKey 

-- Public derivation
subkey :: Wallet -> Word32 -> Maybe Wallet
subkey w i
    | i < 0x80000000 = do
        let pub     = xPublicKey $ publicWallet w
            msg     = (encode' pub) `BS.append` (encode' i)
            (l, c') = split512 $ hmac512 (encode' $ xChainCode w) msg
        pkl <- makePrivateKey $ fromIntegral l
        if isPrvWallet w 
            then do
                pk' <- addPrivateKeys pkl (xPrivateKey w)
                return $ XPrivateKey pk' c' (xDepth w + 1) i (xParent w)
            else do
                pK' <- addPublicKeys (derivePublicKey pkl) (xPublicKey w) 
                return $ XPublicKey pK' c' (xDepth w + 1) i (xParent w)
    | otherwise = error "Derivation index must be smaller than 0x80000000"
    
-- Private derivation
subkey' :: Wallet -> Word32 -> Maybe Wallet
subkey' (XPrivateKey pk c d _ p) i
    | i < 0x80000000 = do
        let i'     = setBit i 31
            msg    = (prvToBS pk) `BS.append` (encode' i')
            (l, c') = split512 $ hmac512 (encode' c) msg
        pkl <- makePrivateKey $ fromIntegral l
        pk' <- addPrivateKeys pk pkl
        return $ XPrivateKey pk' c' (d + 1) i' p
    | otherwise = error "Derivation index must be smaller than 0x80000000"
subkey' _ _ = error "Private derivation is not defined for XPublicKey"

