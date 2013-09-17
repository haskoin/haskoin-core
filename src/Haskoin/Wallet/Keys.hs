module Haskoin.Wallet.Keys
( XPubKey(..)
, XPrvKey(..)
, makeXPrvKey
, deriveXPubKey
, prvSubKey
, pubSubKey
, primeSubKey
, prvSubKeys
, pubSubKeys
, pubSubKeys2
, pubSubKeys3
, primeSubKeys
, xPrvIsPrime
, xPubIsPrime
, xPubID
, xPrvID
, xPubFP
, xPrvFP
, xPubAddr
, xPubExport
, xPrvExport
, xPubImport
, xPrvImport
, xPrvWIF
) where

import Control.Monad (guard, unless, when)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit, testBit)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as BS 
    ( ByteString
    , append
    )

import Haskoin.Util
    ( stringToBS
    , toStrictBS
    , encode'
    , decodeOrFail'
    )
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Wallet.Tx

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

makeXPrvKey :: BS.ByteString -> Maybe XPrvKey
makeXPrvKey bs = do
    pk' <- makePrvKey $ fromIntegral pk
    return $ XPrvKey 0 0 0 c pk'
    where (pk,c) = split512 $ hmac512 (stringToBS "Bitcoin seed") bs

deriveXPubKey :: XPrvKey -> XPubKey
deriveXPubKey (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)

-- Public derivation of XPrvKey
prvSubKey :: XPrvKey -> Word32 -> Maybe XPrvKey
prvSubKey xkey child = guardIndex child >> do
    k <- addPrvKeys (xPrvKey xkey) a
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) child c k
    where pK    = xPubKey $ deriveXPubKey xkey
          msg   = BS.append (encode' pK) (encode' child)
          (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

-- Public derivation of XPubKey
pubSubKey :: XPubKey -> Word32 -> Maybe XPubKey
pubSubKey xKey child = guardIndex child >> do
    pK <- addPubKeys (xPubKey xKey) a
    return $ XPubKey (xPubDepth xKey + 1) (xPubFP xKey) child c pK
    where msg   = BS.append (encode' $ xPubKey xKey) (encode' child)
          (a,c) = split512 $ hmac512 (encode' $ xPubChain xKey) msg

-- Prime derivation of XPrvKey
primeSubKey :: XPrvKey -> Word32 -> Maybe XPrvKey
primeSubKey xkey child = guardIndex child >> do
    k  <- addPrvKeys (xPrvKey xkey) a
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    where i     = setBit child 31
          msg   = BS.append (bsPadPrvKey $ xPrvKey xkey) (encode' i)
          (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

-- List all valid subkeys start from an offset
prvSubKeys :: XPrvKey -> Word32 -> [XPrvKey]
prvSubKeys k i = mapMaybe (prvSubKey k) [i..0x7fffffff]

pubSubKeys :: XPubKey -> Word32 -> [XPubKey]
pubSubKeys k i = mapMaybe (pubSubKey k) [i..0x7fffffff]

primeSubKeys :: XPrvKey -> Word32 -> [XPrvKey]
primeSubKeys k i = mapMaybe (primeSubKey k) [i..0x7fffffff]

pubSubKeys2 :: XPubKey -> XPubKey -> Word32 -> [Script]
pubSubKeys2 par1 par2 i = mapMaybe (f par1 par2) [i..0x7fffffff]
    where f k1 k2 x = buildMulSig2 <$> (xPubKey <$> pubSubKey k1 x)
                                   <*> (xPubKey <$> pubSubKey k2 x)

pubSubKeys3 :: XPubKey -> XPubKey -> XPubKey -> Word32 -> [Script]
pubSubKeys3 par1 par2 par3 i = mapMaybe (f par1 par2 par3) [i..0x7fffffff]
    where f k1 k2 k3 x = buildMulSig3 <$> (xPubKey <$> pubSubKey k1 x)
                                      <*> (xPubKey <$> pubSubKey k2 x)
                                      <*> (xPubKey <$> pubSubKey k3 x)

guardIndex :: Word32 -> Maybe ()
guardIndex child = guard $ child >= 0 && child < 0x80000000

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
xPubAddr :: XPubKey -> Address
xPubAddr = pubKeyAddr . xPubKey

-- Base 58 export
xPrvExport :: XPrvKey -> BS.ByteString
xPrvExport = encodeBase58Check . encode' 

xPubExport :: XPubKey -> BS.ByteString
xPubExport = encodeBase58Check . encode'

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

