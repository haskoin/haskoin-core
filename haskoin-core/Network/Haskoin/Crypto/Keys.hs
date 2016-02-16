{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

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
, derivePubKey
, pubKeyAddr
, tweakPubKeyC
, PrvKeyI(prvKeyCompressed, prvKeySecKey)
, PrvKey, PrvKeyC, PrvKeyU
, makePrvKey
, makePrvKeyG
, makePrvKeyC
, makePrvKeyU
, toPrvKeyG
, eitherPrvKey
, maybePrvKeyC
, maybePrvKeyU
, encodePrvKey
, decodePrvKey
, prvKeyPutMonad
, prvKeyGetMonad
, fromWif
, toWif
, tweakPrvKeyC
) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), guard, mzero)
import Control.DeepSeq (NFData, rnf)

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Maybe (fromMaybe)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getByteString)
import Data.Binary.Put (Put, putByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( head, tail
    , last, init
    , cons, snoc
    , length, elem, pack
    )
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)

import qualified Crypto.Secp256k1 as EC

import Text.Read (readPrec, parens, lexP, pfail)
import qualified Text.Read as Read (Lexeme(Ident, String))

import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Constants
import Network.Haskoin.Util

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
    { pubKeyPoint      :: !EC.PubKey
    , pubKeyCompressed :: !Bool
    } deriving (Eq)

-- TODO: Test
instance Show PubKey where
    showsPrec d k = showParen (d > 10) $
        showString "PubKey " . shows (encodeHex $ encode' k)

-- TODO: Test
instance Show PubKeyC where
    showsPrec d k = showParen (d > 10) $
        showString "PubKeyC " . shows (encodeHex $ encode' k)

-- TODO: Test
instance Show PubKeyU where
    showsPrec d k = showParen (d > 10) $
        showString "PubKeyU " . shows (encodeHex $ encode' k)

-- TODO: Test
instance Read PubKey where
    readPrec = parens $ do
        Read.Ident "PubKey" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe <=< decodeHex $ cs str

-- TODO: Test
instance Read PubKeyC where
    readPrec = parens $ do
        Read.Ident "PubKeyC" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe <=< decodeHex $ cs str

-- TODO: Test
instance Read PubKeyU where
    readPrec = parens $ do
        Read.Ident "PubKeyU" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe <=< decodeHex $ cs str

-- TODO: Test
instance IsString PubKey where
    fromString str =
        fromMaybe e $ decodeToMaybe <=< decodeHex $ cs str
      where
        e = error "Could not decode public key"

instance IsString PubKeyC where
    fromString str =
        fromMaybe e $ decodeToMaybe <=< decodeHex $ cs str
      where
        e = error "Could not decode compressed public key"

instance IsString PubKeyU where
    fromString str =
        fromMaybe e $ decodeToMaybe <=< decodeHex $ cs str
      where
        e = error "Could not decode uncompressed public key"

instance NFData (PubKeyI c) where
    rnf (PubKeyI p c) = p `seq` rnf c

instance ToJSON PubKey where
    toJSON = String . cs . encodeHex . encode'

instance FromJSON PubKey where
    parseJSON = withText "PubKey" $
        maybe mzero return . (decodeToMaybe =<<) . decodeHex . cs

instance ToJSON PubKeyC where
    toJSON = String . cs . encodeHex . encode'

instance FromJSON PubKeyC where
    parseJSON = withText "PubKeyC" $
        maybe mzero return . (decodeToMaybe =<<) . decodeHex . cs

instance ToJSON PubKeyU where
    toJSON = String . cs . encodeHex . encode'

instance FromJSON PubKeyU where
    parseJSON = withText "PubKeyU" $
        maybe mzero return . (decodeToMaybe =<<) . decodeHex . cs

-- Constructors for public keys
makePubKey :: EC.PubKey -> PubKey
makePubKey p = PubKeyI p True

makePubKeyG :: Bool -> EC.PubKey -> PubKey
makePubKeyG c p = PubKeyI p c

makePubKeyC :: EC.PubKey -> PubKeyC
makePubKeyC p = PubKeyI p True

makePubKeyU :: EC.PubKey -> PubKeyU
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

-- | Derives a public key from a private key. This function will preserve
-- information on key compression ('PrvKey' becomes 'PubKey' and 'PrvKeyU'
-- becomes 'PubKeyU')
derivePubKey :: PrvKeyI c -> PubKeyI c
derivePubKey (PrvKeyI d c) = PubKeyI (EC.derivePubKey d) c

instance Binary PubKey where
    get =
        (toPubKeyG <$> getC) <|> (toPubKeyG <$> getU)
      where
        getC = get :: Get (PubKeyI Compressed)
        getU = get :: Get (PubKeyI Uncompressed)

    put pk = case eitherPubKey pk of
        Left k  -> put k
        Right k -> put k

instance Binary PubKeyC where
    get = do
        bs <- getByteString 33
        guard $ BS.head bs `BS.elem` BS.pack [0x02, 0x03]
        maybe mzero return $ makePubKeyC <$> EC.importPubKey bs

    put pk = putByteString $ EC.exportPubKey True $ pubKeyPoint pk

instance Binary PubKeyU where
    get = do
        bs <- getByteString 65
        guard $ BS.head bs == 0x04
        maybe mzero return $ makePubKeyU <$> EC.importPubKey bs

    put pk = putByteString $ EC.exportPubKey False $ pubKeyPoint pk

-- | Computes an 'Address' from a public key
pubKeyAddr :: Binary (PubKeyI c) => PubKeyI c -> Address
pubKeyAddr = PubKeyAddress . hash160 . getHash256 . hash256 . encode'

-- | Tweak a compressed public key
tweakPubKeyC :: PubKeyC -> Hash256 -> Maybe PubKeyC
tweakPubKeyC pub h =
    makePubKeyC <$> (EC.tweakAddPubKey point =<< tweak)
  where
    point = pubKeyPoint pub
    tweak = EC.tweak $ getHash256 h

{- Private Keys -}

-- | Elliptic curve private key type. Two constructors are provided for creating
-- compressed or uncompressed private keys. Compression information is stored
-- in private key WIF formats and needs to be preserved to generate the correct
-- addresses from the corresponding public key.

-- Internal private key type
data PrvKeyI c = PrvKeyI
    { prvKeySecKey     :: !EC.SecKey
    , prvKeyCompressed :: !Bool
    } deriving (Eq)

instance NFData (PrvKeyI c) where
    rnf (PrvKeyI s b) = s `seq` b `seq` ()

-- TODO: Test
instance Show PrvKey where
    showsPrec d k = showParen (d > 10) $
        showString "PrvKey " . shows (toWif k)

-- TODO: Test
instance Show PrvKeyC where
    showsPrec d k = showParen (d > 10) $
        showString "PrvKeyC " . shows (toWif k)

-- TODO: Test
instance Show PrvKeyU where
    showsPrec d k = showParen (d > 10) $
        showString "PrvKeyU " . shows (toWif k)

-- TODO: Test
instance Read PrvKey where
    readPrec = parens $ do
        Read.Ident "PrvKey" <- lexP
        Read.String str <- lexP
        maybe pfail return $ fromWif $ cs str

-- TODO: Test
instance Read PrvKeyC where
    readPrec = parens $ do
        Read.Ident "PrvKeyC" <- lexP
        Read.String str <- lexP
        key <- maybe pfail return $ fromWif $ cs str
        case eitherPrvKey key of
            Left _  -> pfail
            Right k -> return k

-- TODO: Test
instance Read PrvKeyU where
    readPrec = parens $ do
        Read.Ident "PrvKeyU" <- lexP
        Read.String str <- lexP
        key <- maybe pfail return $ fromWif $ cs str
        case eitherPrvKey key of
            Left k  -> return k
            Right _ -> pfail

-- TODO: Test
instance IsString PrvKey where
    fromString str =
        fromMaybe e $ fromWif $ cs str
      where
        e = error "Could not decode WIF"

-- TODO: Test
instance IsString PrvKeyC where
    fromString str =
        case eitherPrvKey key of
            Left _  -> undefined
            Right k -> k
      where
        key = fromMaybe e $ fromWif $ cs str
        e = error "Could not decode WIF"

-- TODO: Test
instance IsString PrvKeyU where
    fromString str =
        case eitherPrvKey key of
            Left k  -> k
            Right _ -> undefined
      where
        key = fromMaybe e $ fromWif $ cs str
        e = error "Could not decode WIF"

type PrvKey = PrvKeyI Generic
type PrvKeyC = PrvKeyI Compressed
type PrvKeyU = PrvKeyI Uncompressed

makePrvKeyI :: Bool -> EC.SecKey -> PrvKeyI c
makePrvKeyI c d = PrvKeyI d c

makePrvKey :: EC.SecKey -> PrvKey
makePrvKey d = makePrvKeyI True d

makePrvKeyG :: Bool -> EC.SecKey -> PrvKey
makePrvKeyG = makePrvKeyI

makePrvKeyC :: EC.SecKey -> PrvKeyC
makePrvKeyC d = makePrvKeyI True d

makePrvKeyU :: EC.SecKey -> PrvKeyU
makePrvKeyU d = makePrvKeyI False d

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

-- | Serialize private key as 32-byte big-endian 'ByteString'
encodePrvKey :: PrvKeyI c -> ByteString
encodePrvKey (PrvKeyI d _) = EC.getSecKey d

-- | Deserialize private key as 32-byte big-endian 'ByteString'
decodePrvKey :: (EC.SecKey -> PrvKeyI c) -> ByteString -> Maybe (PrvKeyI c)
decodePrvKey f bs = f <$> EC.secKey bs

prvKeyGetMonad :: (EC.SecKey -> PrvKeyI c) -> Get (PrvKeyI c)
prvKeyGetMonad f = do
    bs <- getByteString 32
    fromMaybe err $ return <$> f <$> EC.secKey bs
  where
    err = fail "Get: Invalid private key"

prvKeyPutMonad :: PrvKeyI c -> Put
prvKeyPutMonad (PrvKeyI k _) = putByteString $ EC.getSecKey k

-- | Decodes a private key from a WIF encoded 'ByteString'. This function can
-- fail if the input string does not decode correctly as a base 58 string or if
-- the checksum fails.
-- <http://en.bitcoin.it/wiki/Wallet_import_format>
fromWif :: ByteString -> Maybe PrvKey
fromWif wif = do
    bs <- decodeBase58Check wif
    -- Check that this is a private key
    guard (BS.head bs == secretPrefix)
    case BS.length bs of
        33 -> do               -- Uncompressed format
            makePrvKeyG False <$> EC.secKey (BS.tail bs)
        34 -> do               -- Compressed format
            guard $ BS.last bs == 0x01
            makePrvKeyG True <$> EC.secKey (BS.tail $ BS.init bs)
        _  -> Nothing          -- Bad length

-- | Encodes a private key into WIF format
toWif :: PrvKeyI c -> ByteString
toWif (PrvKeyI k c) = encodeBase58Check $ BS.cons secretPrefix $
    if c then EC.getSecKey k `BS.snoc` 0x01 else EC.getSecKey k


-- | Tweak a private key
tweakPrvKeyC :: PrvKeyC -> Hash256 -> Maybe PrvKeyC
tweakPrvKeyC key h =
    makePrvKeyC <$> (EC.tweakAddSecKey sec =<< tweak)
  where
    sec   = prvKeySecKey key
    tweak = EC.tweak $ getHash256 h

