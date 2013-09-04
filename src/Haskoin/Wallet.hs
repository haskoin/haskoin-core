module Haskoin.Wallet 
( Wallet(..)
, createMasterWallet
, subkey
, subkey'
, toPubWallet
, isPubWallet
, isPrvWallet
, walletID
, walletFP
, walletAddr
, walletPubKey
, walletPrvKey
, walletToBase58
, walletFromBase58
, walletToWIF
) where

import Control.Monad (liftM2, guard, unless, when)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.ByteString as BS 
    ( ByteString
    , singleton
    , cons
    , append
    )
import qualified Data.ByteString as BS (ByteString, take)

import Crypto.MAC.HMAC (hmac)
import Haskoin.Util
    ( stringToBS
    , toLazyBS
    , toStrictBS
    , encode'
    , decodeOrFail'
    , decode'
    , bsToInteger
    )
import Haskoin.Crypto

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

type ChainCode = Hash256

data DerType = PrvDer | PubDer

data Wallet = 
    XPrvKey
        { xDepth      :: !Word8
        , xParent     :: !Word32
        , xIndex      :: !Word32
        , xChainCode  :: !ChainCode
        , xPrvKey     :: !PrvKey
        } 
    | XPubKey 
        { xDepth      :: !Word8
        , xParent     :: !Word32
        , xIndex      :: !Word32
        , xChainCode  :: !ChainCode
        , xPubKey     :: !PubKey
        } 
    deriving (Eq, Show)

toPubWallet :: Wallet -> Wallet
toPubWallet (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)
toPubWallet pub = pub

isPubWallet :: Wallet -> Bool
isPubWallet (XPubKey _ _ _ _ _) = True
isPubWallet (XPrvKey _ _ _ _ _) = False

isPrvWallet :: Wallet -> Bool
isPrvWallet = not . isPubWallet

hmac512 :: BS.ByteString -> BS.ByteString -> Hash512
hmac512 key msg = decode' $ hmac hash512BS 128 key msg

split512 :: Hash512 -> (Hash256, Hash256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

createMasterWallet :: BS.ByteString -> Maybe Wallet
createMasterWallet bs = do
    pk' <- makePrvKey $ fromIntegral pk
    return $ XPrvKey 0 0 0 c pk'
    where (pk,c) = split512 $ hmac512 (stringToBS "Bitcoin seed") bs

-- Public derivation
subkey :: Wallet -> Word32 -> Maybe Wallet
subkey w i
    | i < 0x80000000 = do
        let pub     = xPubKey $ toPubWallet w
            msg     = (encode' pub) `BS.append` (encode' i)
            (l, c') = split512 $ hmac512 (encode' $ xChainCode w) msg
        pkl <- makePrvKey $ fromIntegral l
        if isPrvWallet w 
            then do
                pk' <- addPrvKeys pkl (xPrvKey w)
                return $ XPrvKey (xDepth w + 1) (walletFP w) i c' pk'
            else do
                pK' <- addPubKeys (derivePubKey pkl) (xPubKey w) 
                return $ XPubKey (xDepth w + 1) (walletFP w) i c' pK'
    | otherwise = error "Derivation index must be smaller than 0x80000000"
    
-- Private derivation
subkey' :: Wallet -> Word32 -> Maybe Wallet
subkey' w@(XPrvKey d _ _ c pk) i
    | i < 0x80000000 = do
        let i'     = setBit i 31
            pkBS   = toStrictBS $ runPut $ putPadPrvKey pk
            msg    = pkBS `BS.append` (encode' i')
            (l, c') = split512 $ hmac512 (encode' c) msg
        pkl <- makePrvKey $ fromIntegral l
        pk' <- addPrvKeys pk pkl
        return $ XPrvKey (d + 1) (walletFP w) i' c' pk'
    | otherwise = error "Derivation index must be smaller than 0x80000000"
subkey' _ _ = error "Private derivation is not defined for XPubKey"

-- De-serialize HDW-specific private key
getPadPrvKey :: Get PrvKey
getPadPrvKey = do
    pad <- getWord8
    unless (pad == 0x00) $ fail $
        "Private key must be padded with 0x00"
    getPrvKey -- Compressed version

-- Serialize HDW-specific private key
putPadPrvKey :: PrvKey -> Put 
putPadPrvKey p = putWord8 0x00 >> putPrvKey p

walletID :: Wallet -> Hash160
walletID = hash160 . hash256BS . encode' . xPubKey . toPubWallet

walletFP :: Wallet -> Word32
walletFP = fromIntegral . (`shiftR` 128) . walletID

walletAddr :: Wallet -> BS.ByteString
walletAddr = pubKeyAddr . xPubKey . toPubWallet

walletPubKey :: Wallet -> PubKey
walletPubKey = xPubKey . toPubWallet

walletPrvKey :: Wallet -> PrvKey
walletPrvKey w
    | isPrvWallet w = xPrvKey w
    | otherwise    = error "No private key in a public wallet"

walletToBase58 :: Wallet -> BS.ByteString
walletToBase58 = encodeBase58Check . encode'

walletFromBase58 :: BS.ByteString -> Maybe Wallet
walletFromBase58 bs = do
    bs' <- decodeBase58Check bs
    case decodeOrFail' bs' of
        (Left _)            -> Nothing
        (Right (_, _, res)) -> Just res

walletToWIF :: Wallet -> BS.ByteString
walletToWIF w
    | isPrvWallet w = toWIF $ xPrvKey w
    | otherwise     = error "WIF is only defined for private keys"

instance Binary Wallet where

    get = do
        ver <- getWord32be
        dep <- getWord8
        par <- getWord32be
        idx <- getWord32be
        chn <- get 
        case ver of 
            0X0488b21e -> do
                pub <- get 
                when (isPubKeyU pub) $ fail $
                    "Invalid public key. Only compressed format is supported"
                return $ XPubKey dep par idx chn pub
            0x0488ade4 -> do
                prv <- getPadPrvKey
                return $ XPrvKey dep par idx chn prv
            _ -> fail $ "Invalid wallet version bytes"

    put w = do
        if isPubWallet w 
            then putWord32be 0X0488b21e
            else putWord32be 0x0488ade4 
        putWord8    $ xDepth w
        putWord32be $ xParent w
        putWord32be $ xIndex w
        put $ xChainCode w
        if isPubWallet w
            then do
                when (isPubKeyU (xPubKey w)) $ fail $
                    "Only compressed public keys are supported"
                put $ xPubKey w
            else do
                putPadPrvKey $ xPrvKey w
        

