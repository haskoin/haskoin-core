module Haskoin.Wallet 
( Wallet(..)
, createMasterNode
, publicWallet
, isPubWallet
, isPrvWallet
, subkey
, subkey'
, walletID
, walletFP
, walletAddress
, walletPublicKey
, walletToBase58
, walletFromBase58
) where

import Control.Monad (liftM2, guard, unless)
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
    XPrivateKey
        { xDepth      :: !Word8
        , xParent     :: !Word32
        , xIndex      :: !Word32
        , xChainCode  :: !ChainCode
        , xPrivateKey :: !PrivateKey
        } 
    | XPublicKey 
        { xDepth      :: !Word8
        , xParent     :: !Word32
        , xIndex      :: !Word32
        , xChainCode  :: !ChainCode
        , xPublicKey  :: !PublicKey
        } 
    deriving (Eq, Show)

publicWallet :: Wallet -> Wallet
publicWallet (XPrivateKey d p i c k) = XPublicKey d p i c (derivePublicKey k)
publicWallet pub = pub

isPubWallet :: Wallet -> Bool
isPubWallet (XPublicKey  _ _ _ _ _) = True
isPubWallet (XPrivateKey _ _ _ _ _) = False

isPrvWallet :: Wallet -> Bool
isPrvWallet = not . isPubWallet

hmac512 :: BS.ByteString -> BS.ByteString -> Hash512
hmac512 key msg = decode' $ hmac hash512BS 128 key msg

split512 :: Hash512 -> (Hash256, Hash256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

createMasterNode :: BS.ByteString -> Maybe Wallet
createMasterNode bs = do
    pk' <- makePrivateKey $ fromIntegral pk
    return $ XPrivateKey 0 0 0 c pk'
    where (pk,c) = split512 $ hmac512 (stringToBS "Bitcoin seed") bs

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
                return $ XPrivateKey (xDepth w + 1) (walletFP w) i c' pk'
            else do
                pK' <- addPublicKeys (derivePublicKey pkl) (xPublicKey w) 
                return $ XPublicKey (xDepth w + 1) (walletFP w) i c' pK'
    | otherwise = error "Derivation index must be smaller than 0x80000000"
    
-- Private derivation
subkey' :: Wallet -> Word32 -> Maybe Wallet
subkey' w@(XPrivateKey d _ _ c pk) i
    | i < 0x80000000 = do
        let i'     = setBit i 31
            pkBS   = toStrictBS $ runPut $ putPrivateKey pk
            msg    = pkBS `BS.append` (encode' i')
            (l, c') = split512 $ hmac512 (encode' c) msg
        pkl <- makePrivateKey $ fromIntegral l
        pk' <- addPrivateKeys pk pkl
        return $ XPrivateKey (d + 1) (walletFP w) i' c' pk'
    | otherwise = error "Derivation index must be smaller than 0x80000000"
subkey' _ _ = error "Private derivation is not defined for XPublicKey"

-- De-serialize HDW-specific private key
getPrivateKey :: Get PrivateKey
getPrivateKey = do
    pad <- getWord8
    unless (pad == 0x00) $ fail $
        "Private key must be padded with 0x00"
    h   <- get :: Get Hash256
    let prv = makePrivateKey $ fromIntegral h
    unless (isJust prv) $ fail $
        "De-serialized invalid private key"
    return $ fromJust prv

-- Serialize HDW-specific private key
putPrivateKey :: PrivateKey -> Put 
putPrivateKey p = do
    putWord8 0x00
    put (fromIntegral $ fromPrivateKey p :: Hash256)

walletID :: Wallet -> Hash160
walletID = hash160 . hash256BS . encode' . xPublicKey . publicWallet

walletFP :: Wallet -> Word32
walletFP = fromIntegral . (`shiftR` 128) . walletID

walletAddress :: Wallet -> BS.ByteString
walletAddress = publicKeyAddress . xPublicKey . publicWallet

walletPublicKey :: Wallet -> PublicKey
walletPublicKey = xPublicKey . publicWallet

walletToBase58 :: Wallet -> BS.ByteString
walletToBase58 = encodeBase58Check . encode'

walletFromBase58 :: BS.ByteString -> Maybe Wallet
walletFromBase58 bs = do
    bs' <- decodeBase58Check bs
    case decodeOrFail' bs' of
        (Left _)            -> Nothing
        (Right (_, _, res)) -> Just res

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
                unless (isCompressed pub) $ fail $
                    "Invalid public key. Only compressed format is supported"
                return $ XPublicKey dep par idx chn pub
            0x0488ade4 -> do
                prv <- getPrivateKey
                return $ XPrivateKey dep par idx chn prv
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
                unless (isCompressed (xPublicKey w)) $ fail $
                    "Only compressed public keys are supported"
                put $ xPublicKey w
            else do
                putPrivateKey $ xPrivateKey w
        

