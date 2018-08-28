{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Keys.Types
    ( PubKeyI(..)
    , PubKey, PubKeyC, PubKeyU
    , PrvKeyI(..)
    , PrvKey, PrvKeyC, PrvKeyU
    , CompressedOrNot, Compressed, Uncompressed
    , makePubKey
    , makePubKeyG
    , makePubKeyC
    , makePubKeyU
    , toPubKeyG
    , eitherPubKey
    , maybePubKeyC
    , maybePubKeyU
    , derivePubKey
    , tweakPubKeyC
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
    , fromMiniKey
    , tweakPrvKeyC
    ) where

import           Control.Applicative         ((<|>))
import           Control.DeepSeq             (NFData, rnf)
import           Control.Monad               (guard, mzero, (<=<))
import qualified Crypto.Secp256k1            as EC
import           Data.Aeson                  (FromJSON, ToJSON, Value (String),
                                              parseJSON, toJSON, withText)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              (Serialize, decode, encode, get,
                                              put)
import           Data.Serialize.Get          (Get, getByteString)
import           Data.Serialize.Put          (Put, putByteString)
import           Data.String                 (IsString, fromString)
import           Data.String.Conversions     (cs)
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Util
import           Text.Read                   (lexP, parens, pfail, readPrec)
import qualified Text.Read                   as Read

data CompressedOrNot
data Compressed
data Uncompressed

-- | Elliptic curve public key type. Two constructors are provided for creating
-- compressed and uncompressed public keys from a Point. The use of compressed
-- keys is preferred as it produces shorter keys without compromising security.
-- Uncompressed keys are supported for backwards compatibility.
type PubKey = PubKeyI CompressedOrNot
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
        showString "PubKey " . shows (encodeHex $ encode k)

-- TODO: Test
instance Show PubKeyC where
    showsPrec d k = showParen (d > 10) $
        showString "PubKeyC " . shows (encodeHex $ encode k)

-- TODO: Test
instance Show PubKeyU where
    showsPrec d k = showParen (d > 10) $
        showString "PubKeyU " . shows (encodeHex $ encode k)

-- TODO: Test
instance Read PubKey where
    readPrec = parens $ do
        Read.Ident "PubKey" <- lexP
        Read.String str <- lexP
        maybe pfail return $ eitherToMaybe . decode <=< decodeHex $ cs str

-- TODO: Test
instance Read PubKeyC where
    readPrec = parens $ do
        Read.Ident "PubKeyC" <- lexP
        Read.String str <- lexP
        maybe pfail return $ eitherToMaybe . decode <=< decodeHex $ cs str

-- TODO: Test
instance Read PubKeyU where
    readPrec = parens $ do
        Read.Ident "PubKeyU" <- lexP
        Read.String str <- lexP
        maybe pfail return $ eitherToMaybe . decode <=< decodeHex $ cs str

-- TODO: Test
instance IsString PubKey where
    fromString str =
        fromMaybe e $ eitherToMaybe . decode <=< decodeHex $ cs str
      where
        e = error "Could not decode public key"

instance IsString PubKeyC where
    fromString str =
        fromMaybe e $ eitherToMaybe . decode <=< decodeHex $ cs str
      where
        e = error "Could not decode compressed public key"

instance IsString PubKeyU where
    fromString str =
        fromMaybe e $ eitherToMaybe . decode <=< decodeHex $ cs str
      where
        e = error "Could not decode uncompressed public key"

instance NFData (PubKeyI c) where
    rnf (PubKeyI p c) = p `seq` rnf c

instance ToJSON PubKey where
    toJSON = String . cs . encodeHex . encode

instance FromJSON PubKey where
    parseJSON = withText "PubKey" $
        maybe mzero return . (eitherToMaybe . decode =<<) . decodeHex . cs

instance ToJSON PubKeyC where
    toJSON = String . cs . encodeHex . encode

instance FromJSON PubKeyC where
    parseJSON = withText "PubKeyC" $
        maybe mzero return . (eitherToMaybe . decode =<<) . decodeHex . cs

instance ToJSON PubKeyU where
    toJSON = String . cs . encodeHex . encode

instance FromJSON PubKeyU where
    parseJSON = withText "PubKeyU" $
        maybe mzero return . (eitherToMaybe . decode =<<) . decodeHex . cs

instance Serialize PubKey where
    get =
        (toPubKeyG <$> getC) <|> (toPubKeyG <$> getU)
      where
        getC = get :: Get (PubKeyI Compressed)
        getU = get :: Get (PubKeyI Uncompressed)

    put pk = case eitherPubKey pk of
        Left k  -> put k
        Right k -> put k

instance Serialize PubKeyC where
    get = do
        bs <- getByteString 33
        guard $ BS.head bs `BS.elem` BS.pack [0x02, 0x03]
        maybe mzero return $ makePubKeyC <$> EC.importPubKey bs

    put pk = putByteString $ EC.exportPubKey True $ pubKeyPoint pk

instance Serialize PubKeyU where
    get = do
        bs <- getByteString 65
        guard $ BS.head bs == 0x04
        maybe mzero return $ makePubKeyU <$> EC.importPubKey bs

    put pk = putByteString $ EC.exportPubKey False $ pubKeyPoint pk

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

-- | Tweak a compressed public key
tweakPubKeyC :: PubKeyC -> Hash256 -> Maybe PubKeyC
tweakPubKeyC pub h =
    makePubKeyC <$> (EC.tweakAddPubKey point =<< tweak)
  where
    point = pubKeyPoint pub
    tweak = EC.tweak (encode h)

{- Private Keys -}

-- Internal private key type
data PrvKeyI c = PrvKeyI
    { prvKeySecKey     :: !EC.SecKey
    , prvKeyCompressed :: !Bool
    } deriving (Eq, Show, Read)

instance NFData (PrvKeyI c) where
    rnf (PrvKeyI k c) = k `seq` c `seq` ()

type PrvKey = PrvKeyI CompressedOrNot
type PrvKeyC = PrvKeyI Compressed
type PrvKeyU = PrvKeyI Uncompressed

-- | Elliptic curve private key type. Two constructors are provided for creating
-- compressed or uncompressed private keys. Compression information is stored
-- in private key WIF formats and needs to be preserved to generate the correct
-- addresses from the corresponding public key.

makePrvKeyI :: Bool -> EC.SecKey -> PrvKeyI c
makePrvKeyI c d = PrvKeyI d c

makePrvKey :: EC.SecKey -> PrvKey
makePrvKey = makePrvKeyI True

makePrvKeyG :: Bool -> EC.SecKey -> PrvKey
makePrvKeyG = makePrvKeyI

makePrvKeyC :: EC.SecKey -> PrvKeyC
makePrvKeyC = makePrvKeyI True

makePrvKeyU :: EC.SecKey -> PrvKeyU
makePrvKeyU = makePrvKeyI False

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
    maybe err (return . f) (EC.secKey bs)
  where
    err = fail "Get: Invalid private key"

prvKeyPutMonad :: PrvKeyI c -> Put
prvKeyPutMonad (PrvKeyI k _) = putByteString $ EC.getSecKey k


-- | Decode Casascius mini private keys (22 or 30 characters)
fromMiniKey :: ByteString -> Maybe PrvKeyU
fromMiniKey bs = do
    guard checkShortKey
    decodePrvKey makePrvKeyU (encode $ sha256 bs)
  where
    checkHash = encode $ sha256 $ bs `BS.append` "?"
    checkShortKey = BS.length bs `elem` [22, 30] && BS.head checkHash == 0x00

-- | Tweak a private key
tweakPrvKeyC :: PrvKeyC -> Hash256 -> Maybe PrvKeyC
tweakPrvKeyC key h =
    makePrvKeyC <$> (EC.tweakAddSecKey sec =<< tweak)
  where
    sec   = prvKeySecKey key
    tweak = EC.tweak (encode h)
