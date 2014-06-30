module Network.Haskoin.Crypto.Keys
( PubKey(..)
, isValidPubKey
, isPubKeyU
, derivePubKey
, pubKeyAddr
, addPubKeys
, PrvKey(..)
, isValidPrvKey
, makePrvKey
, makePrvKeyU
, fromPrvKey
, isPrvKeyU
, addPrvKeys
, putPrvKey
, getPrvKey
, getPrvKeyU
, fromWIF
, toWIF
, curveG
) where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)

import Control.DeepSeq (NFData, rnf)
import Control.Monad (when, unless, guard)
import Control.Applicative ((<$>),(<*>))
import Data.Maybe (isJust, fromJust)

import qualified Data.ByteString as BS 
    ( head, tail
    , last, init
    , cons, snoc
    , length
    )
import Network.Haskoin.Crypto.Curve 
import Network.Haskoin.Crypto.BigWord 
import Network.Haskoin.Crypto.Point 
import Network.Haskoin.Crypto.Base58 
import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Util 

-- | G parameter of the EC curve expressed as a Point
curveG :: Point
curveG = fromJust $ makePoint (fromInteger $ fst pairG) 
                              (fromInteger $ snd pairG)

-- | Elliptic curve public key type. Two constructors are provided for creating
-- compressed and uncompressed public keys from a Point. The use of compressed
-- keys is preferred as it produces shorter keys without compromising security.
-- Uncompressed keys are supported for backwards compatibility.
data PubKey 
    -- | Compressed public key
    = PubKey  { pubKeyPoint :: !Point } 
    -- | Uncompressed public key
    | PubKeyU { pubKeyPoint :: !Point }
    deriving (Read, Show)

instance NFData PubKey where
    rnf (PubKey p) = rnf p
    rnf (PubKeyU p) = rnf p

instance Eq PubKey where
    -- Compression does not matter for InfPoint
    (PubKey  InfPoint) == (PubKeyU InfPoint) = True
    (PubKeyU InfPoint) == (PubKey  InfPoint) = True
    (PubKey  a)        == (PubKey  b)        = a == b
    (PubKeyU a)        == (PubKeyU b)        = a == b
    _                  == _                  = False

-- | Returns True if the public key is valid. This will check if the public
-- key point lies on the curve.
isValidPubKey :: PubKey -> Bool
isValidPubKey = validatePoint . pubKeyPoint

-- | Add a public key to a private key defined by its Word256 value. This will
-- transform the private key into a public key and add the respective public
-- key points together. This is provided as a helper for BIP32 wallet
-- implementations. This function fails for uncompressed keys and returns
-- Nothing if the private key value is >= than the order of the curve N.
addPubKeys :: PubKey -> Word256 -> Maybe PubKey
addPubKeys pub i
    | isPubKeyU pub = error "Add: HDW only supports compressed formats"
    | toInteger i < curveN =
        let pt1 = mulPoint (fromIntegral i :: FieldN) curveG
            pt2 = addPoint (pubKeyPoint pub) pt1
            in if isInfPoint pt2 then Nothing
                                 else Just $ PubKey pt2
    | otherwise = Nothing

-- | Returns True if the public key is uncompressed
isPubKeyU :: PubKey -> Bool
isPubKeyU (PubKey  _) = False
isPubKeyU (PubKeyU _) = True

-- | Derives a public key from a private key. This function will preserve
-- information on key compression (PrvKey becomes PubKey and PrvKeyU becomes
-- PubKeyU)
derivePubKey :: PrvKey -> PubKey
derivePubKey k = case k of
    (PrvKey  d) -> PubKey  $ mulPoint d curveG
    (PrvKeyU d) -> PubKeyU $ mulPoint d curveG

instance Binary PubKey where

    -- Section 2.3.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = go =<< getWord8
      where 
        -- skip 2.3.4.1 and fail. InfPoint is an invalid public key
        go 0 = fail "InfPoint is not a valid public key"
        -- 2.3.4.3 Uncompressed format
        go 4 = getUncompressed
        -- 2.3.4.2 Compressed format
        -- 2 means pY is even, 3 means pY is odd
        go y | y == 2 || y == 3 = getCompressed (even y)
             | otherwise = fail "Get: Invalid public key encoding"

    -- Section 2.3.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put pk = case getAffine (pubKeyPoint pk) of
        -- 2.3.3.1
        Nothing    -> putWord8 0x00
        Just (x,y) -> case pk of
            -- Compressed
            PubKey  _ -> putWord8 (if even y then 2 else 3) >> put x
            -- Uncompressed
            PubKeyU _ -> putWord8 4 >> put x >> put y

getUncompressed :: Get PubKey
getUncompressed = do
    p <- makePoint <$> get <*> get
    unless (isJust p) (fail "Get: Point not on the curve")
    return $ PubKeyU $ fromJust $ p

getCompressed :: Bool -> Get PubKey
getCompressed e = do
    -- 2.1 
    x <- get :: Get FieldP
    -- 2.4.1 (deriving yP)
    let a  = x ^ (3 :: Integer) + (curveA * x) + curveB
        ys = filter matchSign (quadraticResidue a)
    -- We found no square root (mod p)
    when (null ys) (fail $ "No ECC point for x = " ++ (show x))
    let p = makePoint x (head ys)
    -- Additionally, check that the point is on the curve
    unless (isJust p) (fail "Get: Point not on the curve")
    return $ PubKey $ fromJust $ p
  where 
    matchSign a = (even a) == e

-- | Computes an Address value from a public key
pubKeyAddr :: PubKey -> Address
pubKeyAddr = PubKeyAddress . hash160 . hash256BS . encode'

{- Private Keys -}

-- | Elliptic curve private key type. Two constructors are provided for creating
-- compressed or uncompressed private keys. Compression information is stored
-- in private key WIF formats and needs to be preserved to generate the correct
-- addresses from the corresponding public key. 
data PrvKey 
    -- | Compressed private key
    = PrvKey  { prvKeyFieldN :: !FieldN } 
    -- | Uncompressed private key
    | PrvKeyU { prvKeyFieldN :: !FieldN } 
    deriving (Eq, Show, Read)

instance NFData PrvKey where
    rnf (PrvKey p) = rnf p
    rnf (PrvKeyU p) = rnf p

-- | Returns True if the private key is valid. This will check if the integer
-- value representing the private key is greater than 0 and smaller than the
-- curve order N.
isValidPrvKey :: Integer -> Bool
isValidPrvKey = isIntegerValidKey

-- | Builds a compressed private key from an Integer value. Returns Nothing if
-- the Integer would not produce a valid private key. For security, the Integer
-- needs to be generated from a random source with sufficient entropy.
makePrvKey :: Integer -> Maybe PrvKey
makePrvKey i
    | isValidPrvKey i = Just $ PrvKey $ fromInteger i
    | otherwise       = Nothing

-- | Builds an uncompressed private key from an Integer value. Returns Nothing
-- if the Integer would not produce a valid private key. For security, the
-- Integer needs to be generated from a random source with sufficient entropy.
makePrvKeyU :: Integer -> Maybe PrvKey
makePrvKeyU i
    | isValidPrvKey i = Just $ PrvKeyU $ fromInteger i
    | otherwise       = Nothing

-- | Returns the Integer value of a private key
fromPrvKey :: PrvKey -> Integer
fromPrvKey = fromIntegral . prvKeyFieldN

-- | Add two private keys together. One of the keys is defined by a Word256.
-- The functions fails on uncompressed private keys and return Nothing if the
-- Word256 is smaller than the order of the curve N. This is provided
-- as a helper for implementing BIP32 wallets.
addPrvKeys :: PrvKey -> Word256 -> Maybe PrvKey
addPrvKeys key i
    | isPrvKeyU key = error "Add: HDW only supports compressed formats"
    | toInteger i < curveN =
        let r = (prvKeyFieldN key) + (fromIntegral i :: FieldN) 
            in makePrvKey $ toInteger r
    | otherwise = Nothing

-- | Returns True of the private key is uncompressed
isPrvKeyU :: PrvKey -> Bool
isPrvKeyU (PrvKey  _) = False
isPrvKeyU (PrvKeyU _) = True

-- | Serialize a private key into the Data.Binary.Put monad as a 32 byte
-- big endian ByteString. This is useful when a constant length serialization
-- format for private keys is required
putPrvKey :: PrvKey -> Put
putPrvKey k | prvKeyFieldN k == 0 = error "Put: 0 is an invalid private key"
            | otherwise = put $ (fromIntegral (prvKeyFieldN k) :: Word256)

-- | Deserializes a compressed private key from the Data.Binary.Get monad as a
-- 32 byte big endian ByteString.
getPrvKey :: Get PrvKey
getPrvKey = do
        i <- get :: Get Word256
        let res = makePrvKey $ fromIntegral i
        unless (isJust res) $ fail "Get: PrivateKey is invalid"
        return $ fromJust res

-- | Deserializes an uncompressed private key from the Data.Binary.Get monad as
-- a 32 byte big endian ByteString
getPrvKeyU :: Get PrvKey
getPrvKeyU = do
        i <- get :: Get Word256
        let res = makePrvKeyU $ fromIntegral i
        unless (isJust res) $ fail "Get: PrivateKey is invalid"
        return $ fromJust res

-- | Decodes a private key from a WIF encoded String. This function can fail
-- if the input string does not decode correctly as a base 58 string or if 
-- the checksum fails.
-- <http://en.bitcoin.it/wiki/Wallet_import_format>
fromWIF :: String -> Maybe PrvKey
fromWIF str = do
    bs <- decodeBase58Check $ stringToBS str
    -- Check that this is a private key
    guard (BS.head bs == secretPrefix)  
    case BS.length bs of
        33 -> do               -- Uncompressed format
            let i = bsToInteger (BS.tail bs)
            makePrvKeyU i
        34 -> do               -- Compressed format
            guard (BS.last bs == 0x01) 
            let i = bsToInteger $ BS.tail $ BS.init bs
            makePrvKey i
        _  -> Nothing          -- Bad length

-- | Encodes a private key into WIF format
toWIF :: PrvKey -> String
toWIF k = bsToString $ encodeBase58Check $ BS.cons secretPrefix enc
  where 
    enc | isPrvKeyU k = bs
        | otherwise   = BS.snoc bs 0x01
    bs = runPut' $ putPrvKey k

