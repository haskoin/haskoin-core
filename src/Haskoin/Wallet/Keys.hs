module Haskoin.Wallet.Keys
( XKey(..)
, XPubKey(..)
, XPrvKey(..)
, makeXPrvKey
, deriveXPubKey
, isXPubKey
, isXPrvKey
, prvSubKey
, pubSubKey
, prvSubKey'
, xPrvIsPrime
, xPubIsPrime
, xPubID
, xPrvID
, xPubFP
, xPrvFP
, xPubAddr
, xPrvAddr
, xPubExport
, xPrvExport
, xPubImport
, xPrvImport
, xKeyImport
, xPrvWIF
) where

import Control.Monad (liftM2, guard, unless, when)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit, testBit)
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

-- BIP32 extended keys
data XPrvKey = XPrvKey
    { xPrvDepth  :: !Word8
    , xPrvParent :: !Word32
    , xPrvIndex  :: !Word32
    , xPrvChain  :: !ChainCode
    , xPrvKey    :: !PrvKey
    } deriving (Eq, Show)

data XPubKey = XPubKey
    { xPubDepth  :: !Word8
    , xPubParent :: !Word32
    , xPubIndex  :: !Word32
    , xPubChain  :: !ChainCode
    , xPubKey    :: !PubKey
    } deriving (Eq, Show)

data XKey = XPrvImport { runPrvImport :: XPrvKey } | 
            XPubImport { runPubImport :: XPubKey }
            deriving (Eq, Show)

makeXPrvKey :: BS.ByteString -> Maybe XPrvKey
makeXPrvKey bs = do
    pk' <- makePrvKey $ fromIntegral pk
    return $ XPrvKey 0 0 0 c pk'
    where (pk,c) = split512 $ hmac512 (stringToBS "Bitcoin seed") bs

deriveXPubKey :: XPrvKey -> XPubKey
deriveXPubKey (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)

isXPubKey :: XKey -> Bool
isXPubKey (XPubImport _) = True
isXPubKey (XPrvImport _) = False

isXPrvKey :: XKey -> Bool
isXPrvKey = not . isXPubKey

-- Public derivation
prvSubKey :: XPrvKey -> Int -> Maybe XPrvKey
prvSubKey xkey child = guardIndex child >> do
    a <- makePrvKey $ fromIntegral b
    k <- addPrvKeys a (xPrvKey xkey)
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    where i     = fromIntegral child
          pK    = xPubKey $ deriveXPubKey xkey
          msg   = BS.append (encode' pK) (encode' i)
          (b,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

-- Public derivation
pubSubKey :: XPubKey -> Int -> Maybe XPubKey
pubSubKey xKey child = guardIndex child >> do
    a  <- makePrvKey $ fromIntegral b
    pK <- addPubKeys (derivePubKey a) (xPubKey xKey)
    return $ XPubKey (xPubDepth xKey + 1) (xPubFP xKey) i c pK
    where i     = fromIntegral child
          msg   = BS.append (encode' $ xPubKey xKey) (encode' i)
          (b,c) = split512 $ hmac512 (encode' $ xPubChain xKey) msg

-- Private derivation
prvSubKey' :: XPrvKey -> Int -> Maybe XPrvKey
prvSubKey' xkey child = guardIndex child >> do
    a  <- makePrvKey $ fromIntegral b
    k  <- addPrvKeys a (xPrvKey xkey)
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    where i     = fromIntegral $ setBit child 31
          msg   = BS.append (bsPadPrvKey $ xPrvKey xkey) (encode' i)
          (b,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

guardIndex :: Int -> Maybe ()
guardIndex child = guard $ child >= 0 && child <= 0x80000000

-- Key was derived through private derivation
xPrvIsPrime :: XPrvKey -> Bool
xPrvIsPrime k = testBit (xPrvIndex k) 31

xPubIsPrime :: XPubKey -> Bool
xPubIsPrime k = testBit (xPubIndex k) 31

-- Key idendifiers
xPrvID :: XPrvKey -> Hash160
xPrvID = xPubID . deriveXPubKey

xPubID :: XPubKey -> Hash160
xPubID = hash160 . hash256BS . encode' . xPubKey 

-- Key fingerprint
xPrvFP :: XPrvKey -> Word32
xPrvFP = fromIntegral . (`shiftR` 128) . xPrvID

xPubFP :: XPubKey -> Word32
xPubFP = fromIntegral . (`shiftR` 128) . xPubID

-- Key address
xPrvAddr :: XPrvKey -> BS.ByteString
xPrvAddr = xPubAddr . deriveXPubKey

xPubAddr :: XPubKey -> BS.ByteString
xPubAddr = pubKeyAddr . xPubKey

-- Base 58 export
xPrvExport :: XPrvKey -> BS.ByteString
xPrvExport = encodeBase58Check . encode' . XPrvImport

xPubExport :: XPubKey -> BS.ByteString
xPubExport = encodeBase58Check . encode' . XPubImport

xPrvImport :: BS.ByteString -> Maybe XPrvKey
xPrvImport bs = do
    bs' <- decodeBase58Check bs
    case decodeOrFail' bs' of
        (Left _)            -> Nothing
        (Right (_, _, res)) -> Just res

xPubImport :: BS.ByteString -> Maybe XPubKey
xPubImport bs = do
    bs' <- decodeBase58Check bs
    case decodeOrFail' bs' of
        (Left _)            -> Nothing
        (Right (_, _, res)) -> Just res

-- Base 58 import
xKeyImport :: BS.ByteString -> Maybe XKey
xKeyImport bs = do
    bs' <- decodeBase58Check bs
    case decodeOrFail' bs' of
        (Left _)            -> Nothing
        (Right (_, _, res)) -> Just res

-- Export to WIF format
xPrvWIF :: XPrvKey -> BS.ByteString
xPrvWIF = toWIF . xPrvKey

instance Binary XPrvKey where

    get = do
        ver <- getWord32be
        unless (ver == 0x0488ade4) $ fail $
            "Get: Invalid version for extended private key"
        dep <- getWord8
        par <- getWord32be
        idx <- getWord32be
        chn <- get 
        prv <- getPadPrvKey
        return $ XPrvKey dep par idx chn prv

    put k = do
        putWord32be  0x0488ade4 
        putWord8     $ xPrvDepth k
        putWord32be  $ xPrvParent k
        putWord32be  $ xPrvIndex k
        put          $ xPrvChain k
        putPadPrvKey $ xPrvKey k

instance Binary XPubKey where

    get = do
        ver <- getWord32be
        unless (ver == 0X0488b21e) $ fail $
            "Get: Invalid version for extended public key"
        dep <- getWord8
        par <- getWord32be
        idx <- getWord32be
        chn <- get 
        pub <- get 
        when (isPubKeyU pub) $ fail $
            "Invalid public key. Only compressed format is supported"
        return $ XPubKey dep par idx chn pub

    put k = do
        putWord32be 0X0488b21e
        putWord8    $ xPubDepth k
        putWord32be $ xPubParent k
        putWord32be $ xPubIndex k
        put         $ xPubChain k
        when (isPubKeyU (xPubKey k)) $ fail $
            "Only compressed public keys are supported"
        put $ xPubKey k
        

instance Binary XKey where

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
                return $ XPubImport $ XPubKey dep par idx chn pub
            0x0488ade4 -> do
                prv <- getPadPrvKey
                return $ XPrvImport $ XPrvKey dep par idx chn prv
            _ -> fail $ "Invalid wallet version bytes"

    put (XPubImport k) = do
        putWord32be 0X0488b21e
        putWord8    $ xPubDepth k
        putWord32be $ xPubParent k
        putWord32be $ xPubIndex k
        put         $ xPubChain k
        when (isPubKeyU (xPubKey k)) $ fail $
            "Only compressed public keys are supported"
        put $ xPubKey k

    put (XPrvImport k) = do
        putWord32be  0x0488ade4 
        putWord8     $ xPrvDepth k
        putWord32be  $ xPrvParent k
        putWord32be  $ xPrvIndex k
        put          $ xPrvChain k
        putPadPrvKey $ xPrvKey k
        
{- Utilities for extended keys -}

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

bsPadPrvKey :: PrvKey -> BS.ByteString
bsPadPrvKey = toStrictBS . runPut . putPadPrvKey 

hmac512 :: BS.ByteString -> BS.ByteString -> Hash512
hmac512 key msg = decode' $ hmac hash512BS 128 key msg

split512 :: Hash512 -> (Hash256, Hash256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)

