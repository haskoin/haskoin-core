{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.Haskoin.Crypto.Keys
( PubKeyI(pubKeyCompressed, pubKeyPoint)
, PubKey, PubKeyC, PubKeyU
, makePubKey
, makePubKeyG
, makePubKeyC
, makePubKeyU
, toPubKeyG
, eitherPubKey
, maybePubKeyC
, maybePubKeyU
, isValidPubKey
, derivePubKey
, pubKeyAddr
, PrvKeyI(prvKeyCompressed, prvKeyFieldN)
, PrvKey, PrvKeyC, PrvKeyU
, makePrvKey
, makePrvKeyG
, makePrvKeyC
, makePrvKeyU
, toPrvKeyG
, eitherPrvKey
, maybePrvKeyC
, maybePrvKeyU
, isValidPrvKey
, fromPrvKey
, encodePrvKey
, decodePrvKey
, prvKeyPutMonad
, prvKeyGetMonad
, fromWif
, toWif
, curveG
) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (when, unless, guard)
import Control.DeepSeq (NFData, rnf)

import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import qualified Data.ByteString as BS 
    ( ByteString
    , head, tail
    , last, init
    , cons, snoc
    , length
    )

import Network.Haskoin.Crypto.Curve 
import Network.Haskoin.Crypto.BigWord 
import Network.Haskoin.Crypto.Point 
import Network.Haskoin.Crypto.Base58 
import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Constants
import Network.Haskoin.Util 

-- | G parameter of the EC curve expressed as a Point
curveG :: Point
curveG = fromJust $ makePoint (fromInteger $ fst pairG) 
                              (fromInteger $ snd pairG)

data Generic
data Compressed
data Uncompressed

-- | Elliptic curve public key type. Two constructors are provided for creating
-- compressed and uncompressed public keys from a Point. The use of compressed
-- keys is preferred as it produces shorter keys without compromising security.
-- Uncompressed keys are supported for backwards compatibility.
type PubKey = PubKeyI Generic
type PubKeyC = PubKeyI Compressed
type PubKeyU = PubKeyI Uncompressed

-- Internal type for public keys
data PubKeyI c = PubKeyI 
    { pubKeyPoint      :: !Point 
    , pubKeyCompressed :: !Bool
    } deriving (Eq, Read, Show)

instance NFData (PubKeyI c) where
    rnf (PubKeyI p c) = rnf p `seq` rnf c

-- Constructors for public keys
makePubKey :: Point -> PubKey
makePubKey p = PubKeyI p True

makePubKeyG :: Bool -> Point -> PubKey
makePubKeyG c p = PubKeyI p c

makePubKeyC :: Point -> PubKeyC
makePubKeyC p = PubKeyI p True

makePubKeyU :: Point -> PubKeyU
makePubKeyU p = PubKeyI p False

toPubKeyG :: PubKeyI c -> PubKey
toPubKeyG (PubKeyI p c) = makePubKeyG c p

eitherPubKey :: PubKeyI c -> Either PubKeyU PubKeyC
eitherPubKey pk
    | pubKeyCompressed pk = Right $ makePubKeyC $ pubKeyPoint pk
    | otherwise           = Left  $ makePubKeyU $ pubKeyPoint pk

maybePubKeyC :: PubKeyI c -> Maybe PubKeyC
maybePubKeyC pk
    | pubKeyCompressed pk = Just $ makePubKeyC $ pubKeyPoint pk
    | otherwise           = Nothing

maybePubKeyU :: PubKeyI c -> Maybe PubKeyU
maybePubKeyU pk
    | not (pubKeyCompressed pk) = Just $ makePubKeyU $ pubKeyPoint pk
    | otherwise                 = Nothing

-- | Returns True if the public key is valid. This will check if the public
-- key point lies on the curve.
isValidPubKey :: PubKeyI c -> Bool
isValidPubKey = validatePoint . pubKeyPoint

-- | Derives a public key from a private key. This function will preserve
-- information on key compression (PrvKey becomes PubKey and PrvKeyU becomes
-- PubKeyU)
derivePubKey :: PrvKeyI c -> PubKeyI c
derivePubKey (PrvKeyI d c) = PubKeyI (mulPoint d curveG) c

instance Binary (PubKeyI Generic) where

    get = (toPubKeyG <$> getC) <|> (toPubKeyG <$> getU)
      where
        getC = get :: Get (PubKeyI Compressed)
        getU = get :: Get (PubKeyI Uncompressed)

    put pk = case eitherPubKey pk of
        Left k  -> put k
        Right k -> put k

instance Binary (PubKeyI Compressed) where

    -- Section 2.3.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = getWord8 >>= \y -> do
        -- skip 2.3.4.1 and fail. InfPoint is an invalid public key
        when (y == 0) $ fail "InfPoint is not a valid public key"
        -- 2.3.4.2 Compressed format
        -- 2 means pY is even, 3 means pY is odd
        unless (y `elem` [2,3]) $ fail "Get: Invalid public key encoding"
        -- 2.1 
        x <- get :: Get FieldP
        -- 2.4.1 (deriving yP)
        let a  = x ^ (3 :: Integer) + (curveA * x) + curveB
            ys = filter ((== (even y)) . even) (quadraticResidue a)
        -- We found no square root (mod p)
        when (null ys) (fail $ "No ECC point for x = " ++ (show x))
        let p = makePoint x (head ys)
        -- Additionally, check that the point is on the curve
        unless (isJust p) (fail "Get: Point not on the curve")
        return $ makePubKeyC $ fromJust p

    -- Section 2.3.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put pk = case getAffine (pubKeyPoint pk) of
        -- 2.3.3.1
        Nothing     -> error "Put: Invalid public key"
        Just (x, y) -> putWord8 (if even y then 2 else 3) >> put x

instance Binary (PubKeyI Uncompressed) where

    -- Section 2.3.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = getWord8 >>= \y -> do
        -- skip 2.3.4.1 and fail. InfPoint is an invalid public key
        when (y == 0) $ fail "InfPoint is not a valid public key"
        -- 2.3.4.3 Uncompressed format
        unless (y == 4) $ fail "Get: Invalid public key encoding"
        p <- makePoint <$> get <*> get
        unless (isJust p) (fail "Get: Point not on the curve")
        return $ makePubKeyU $ fromJust $ p

    -- Section 2.3.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put pk = case getAffine (pubKeyPoint pk) of
        -- 2.3.3.1
        Nothing     -> error "Put: Invalid public key"
        Just (x, y) -> putWord8 4 >> put x >> put y

-- | Computes an Address value from a public key
pubKeyAddr :: Binary (PubKeyI c) => PubKeyI c -> Address
pubKeyAddr = PubKeyAddress . hash160 . hash256BS . encode'

{- Private Keys -}

-- | Elliptic curve private key type. Two constructors are provided for creating
-- compressed or uncompressed private keys. Compression information is stored
-- in private key WIF formats and needs to be preserved to generate the correct
-- addresses from the corresponding public key. 

-- Internal private key type
data PrvKeyI c = PrvKeyI
    { prvKeyFieldN     :: !FieldN
    , prvKeyCompressed :: !Bool
    } deriving (Eq, Show, Read)

instance NFData (PrvKeyI c) where
    rnf (PrvKeyI d c) = rnf d `seq` rnf c

type PrvKey = PrvKeyI Generic
type PrvKeyC = PrvKeyI Compressed
type PrvKeyU = PrvKeyI Uncompressed

makePrvKeyI :: Integer -> Bool -> Maybe (PrvKeyI c)
makePrvKeyI d c
    | isValidPrvKey d = Just $ PrvKeyI (fromInteger d) c
    | otherwise       = Nothing

makePrvKey :: Integer -> Maybe PrvKey
makePrvKey d = makePrvKeyI d True

makePrvKeyG :: Bool -> Integer -> Maybe PrvKey
makePrvKeyG c d = makePrvKeyI d c

makePrvKeyC :: Integer -> Maybe PrvKeyC
makePrvKeyC d = makePrvKeyI d True

makePrvKeyU :: Integer -> Maybe PrvKeyU
makePrvKeyU d = makePrvKeyI d False

toPrvKeyG :: PrvKeyI c -> PrvKey
toPrvKeyG (PrvKeyI d c) = PrvKeyI d c

eitherPrvKey :: PrvKeyI c -> Either PrvKeyU PrvKeyC
eitherPrvKey (PrvKeyI d compressed)
    | compressed = Right $ PrvKeyI d compressed
    | otherwise  = Left  $ PrvKeyI d compressed

maybePrvKeyC :: PrvKeyI c -> Maybe PrvKeyC
maybePrvKeyC (PrvKeyI d compressed)
    | compressed = Just $ PrvKeyI d compressed
    | otherwise  = Nothing

maybePrvKeyU :: PrvKeyI c -> Maybe PrvKeyU
maybePrvKeyU (PrvKeyI d compressed)
    | not compressed = Just $ PrvKeyI d compressed
    | otherwise      = Nothing

-- | Returns True if the private key is valid. This will check if the integer
-- value representing the private key is greater than 0 and smaller than the
-- curve order N.
isValidPrvKey :: Integer -> Bool
isValidPrvKey = isIntegerValidKey

-- | Returns the Integer value of a private key
fromPrvKey :: PrvKeyI c -> Integer
fromPrvKey = fromIntegral . prvKeyFieldN

-- | Serialize a private key into the a 32 byte big endian ByteString. This is
-- useful when a constant length serialization format for private keys is
-- required
encodePrvKey :: PrvKeyI c -> BS.ByteString
encodePrvKey = runPut' . prvKeyPutMonad

-- | Deserializes an uncompressed private key from the Data.Binary.Get monad as
-- a 32 byte big endian ByteString
decodePrvKey :: (Integer -> Maybe (PrvKeyI c)) -> BS.ByteString
             -> Maybe (PrvKeyI c)
decodePrvKey f bs = f . fromIntegral =<< (decodeToMaybe bs :: Maybe Word256)

prvKeyGetMonad :: (Integer -> Maybe (PrvKeyI c)) -> Get (PrvKeyI c)
prvKeyGetMonad f = do
    i <- get :: Get Word256
    fromMaybe err $ return <$> f (fromIntegral i)
  where
    err = fail "Get: Invalid private key encoding"

prvKeyPutMonad :: PrvKeyI c -> Put
prvKeyPutMonad k
    | prvKeyFieldN k == 0 = error "Put: 0 is an invalid private key"
    | otherwise           = put (fromIntegral (prvKeyFieldN k) :: Word256)

-- | Decodes a private key from a WIF encoded String. This function can fail
-- if the input string does not decode correctly as a base 58 string or if 
-- the checksum fails.
-- <http://en.bitcoin.it/wiki/Wallet_import_format>
fromWif :: String -> Maybe PrvKey
fromWif str = do
    bs <- decodeBase58Check $ stringToBS str
    -- Check that this is a private key
    guard (BS.head bs == secretPrefix)  
    case BS.length bs of
        33 -> do               -- Uncompressed format
            let i = bsToInteger (BS.tail bs)
            makePrvKeyG False i
        34 -> do               -- Compressed format
            guard (BS.last bs == 0x01) 
            let i = bsToInteger $ BS.tail $ BS.init bs
            makePrvKeyG True i 
        _  -> Nothing          -- Bad length

-- | Encodes a private key into WIF format
toWif :: PrvKeyI c -> String
toWif k = bsToString $ encodeBase58Check $ BS.cons secretPrefix enc
  where 
    enc | prvKeyCompressed k = BS.snoc bs 0x01
        | otherwise          = bs
    bs = encodePrvKey k

