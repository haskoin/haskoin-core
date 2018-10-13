{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Keys.Common
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

ECDSA private and public key functions.
-}
module Network.Haskoin.Keys.Common
    ( -- * Public & Private Keys
      PubKeyI(..)
    , SecKeyI(..)
    , exportPubKey
    , importPubKey
    , wrapPubKey
    , derivePubKeyI
    , wrapSecKey
    , fromMiniKey
    , tweakPubKey
    , tweakSecKey
    , secKeyPut
    , secKeyGet
    , getSecKey
    , secKey
    ) where

import           Control.Applicative         ((<|>))
import           Control.Monad               (guard, mzero, (<=<))
import           Crypto.Secp256k1
import           Data.Aeson                  (FromJSON, ToJSON, Value (String),
                                              parseJSON, toJSON, withText)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Hashable
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              (Serialize, decode, encode, get,
                                              put)
import           Data.Serialize.Get          (Get, getByteString)
import           Data.Serialize.Put          (Putter, putByteString)
import           Data.String                 (IsString, fromString)
import           Data.String.Conversions     (cs)
import           GHC.Generics                (Generic)
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Util

-- | Elliptic curve public key type with expected serialized compression flag.
data PubKeyI = PubKeyI
    { pubKeyPoint      :: !PubKey
    , pubKeyCompressed :: !Bool
    } deriving (Generic, Eq, Show, Read, Hashable)

instance IsString PubKeyI where
    fromString str =
        fromMaybe e $ eitherToMaybe . decode <=< decodeHex $ cs str
      where
        e = error "Could not decode public key"

instance ToJSON PubKeyI where
    toJSON = String . encodeHex . encode

instance FromJSON PubKeyI where
    parseJSON = withText "PubKeyI" $
        maybe mzero return . (eitherToMaybe . decode =<<) . decodeHex

instance Serialize PubKeyI where
    get = c <|> u
      where
        c = do
            bs <- getByteString 33
            guard $ BS.head bs `BS.elem` BS.pack [0x02, 0x03]
            maybe mzero return $ PubKeyI <$> importPubKey bs <*> pure True
        u = do
            bs <- getByteString 65
            guard $ BS.head bs == 0x04
            maybe mzero return $ PubKeyI <$> importPubKey bs <*> pure False

    put pk = putByteString $ exportPubKey (pubKeyCompressed pk) (pubKeyPoint pk)

-- | Wrap a public key from secp256k1 library adding information about compression.
wrapPubKey :: Bool -> PubKey -> PubKeyI
wrapPubKey c p = PubKeyI p c

-- | Derives a public key from a private key. This function will preserve
-- compression flag.
derivePubKeyI :: SecKeyI -> PubKeyI
derivePubKeyI (SecKeyI d c) = PubKeyI (derivePubKey d) c

-- | Tweak a public key.
tweakPubKey :: PubKey -> Hash256 -> Maybe PubKey
tweakPubKey p h = tweakAddPubKey p =<< tweak (encode h)

-- | Elliptic curve private key type with expected public key compression
-- information. Compression information is stored in private key WIF formats and
-- needs to be preserved to generate the correct address from the corresponding
-- public key.
data SecKeyI = SecKeyI
    { secKeyData       :: !SecKey
    , secKeyCompressed :: !Bool
    } deriving (Eq, Show, Read)

-- | Wrap private key with corresponding public key compression flag.
wrapSecKey :: Bool -> SecKey -> SecKeyI
wrapSecKey c d = SecKeyI d c

-- | Deserialize 'SecKey'.
secKeyGet :: Get SecKey
secKeyGet = do
    bs <- getByteString 32
    maybe (fail "invalid private key") return (secKey bs)

-- | Serialize 'SecKey'.
secKeyPut :: Putter SecKey
secKeyPut = putByteString . getSecKey

-- | Decode Casascius mini private keys (22 or 30 characters).
fromMiniKey :: ByteString -> Maybe SecKeyI
fromMiniKey bs = do
    guard checkShortKey
    wrapSecKey False <$> secKey (encode (sha256 bs))
  where
    checkHash = encode $ sha256 $ bs `BS.append` "?"
    checkShortKey = BS.length bs `elem` [22, 30] && BS.head checkHash == 0x00

-- | Tweak a private key.
tweakSecKey :: SecKey -> Hash256 -> Maybe SecKey
tweakSecKey key h = tweakAddSecKey key =<< tweak (encode h)
