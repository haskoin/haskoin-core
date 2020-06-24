{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Haskoin.Keys.Common
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

ECDSA private and public key functions.
-}
module Haskoin.Keys.Common
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
    , getSecKey
    , secKey
    -- ** Private Key Wallet Import Format (WIF)
    , fromWif
    , toWif
    ) where

import           Control.Applicative     ((<|>))
import           Control.DeepSeq
import           Control.Monad           (guard, mzero, (<=<))
import           Crypto.Secp256k1
import           Data.Aeson              (FromJSON, ToJSON (..), Value (String),
                                          parseJSON, withText)
import           Data.Aeson.Encoding     (unsafeToEncoding)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (char7)
import           Data.Hashable
import           Data.Maybe              (fromMaybe)
import           Data.Serialize          (Serialize, decode, encode, get, put)
import           Data.Serialize.Get      (getByteString)
import           Data.Serialize.Put      (putByteString)
import           Data.String             (IsString, fromString)
import           Data.String.Conversions (cs)
import           GHC.Generics            (Generic)
import           Haskoin.Address.Base58
import           Haskoin.Constants
import           Haskoin.Crypto.Hash
import           Haskoin.Util

-- | Elliptic curve public key type with expected serialized compression flag.
data PubKeyI = PubKeyI
    { pubKeyPoint      :: !PubKey
    , pubKeyCompressed :: !Bool
    } deriving (Generic, Eq, Show, Read, Hashable, NFData)

instance IsString PubKeyI where
    fromString str =
        fromMaybe e $ eitherToMaybe . decode <=< decodeHex $ cs str
      where
        e = error "Could not decode public key"

instance ToJSON PubKeyI where
    toJSON = String . encodeHex . encode
    toEncoding s = unsafeToEncoding $ char7 '"' <> hexBuilder (encode s) <> char7 '"'

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
    } deriving (Eq, Show, Read, Generic, NFData)

instance Serialize SecKey where
    get = do
        bs <- getByteString 32
        maybe (fail "invalid private key") return (secKey bs)
    put = putByteString . getSecKey

-- | Wrap private key with corresponding public key compression flag.
wrapSecKey :: Bool -> SecKey -> SecKeyI
wrapSecKey c d = SecKeyI d c

-- | Tweak a private key.
tweakSecKey :: SecKey -> Hash256 -> Maybe SecKey
tweakSecKey key h = tweakAddSecKey key =<< tweak (encode h)

-- | Decode Casascius mini private keys (22 or 30 characters).
fromMiniKey :: ByteString -> Maybe SecKeyI
fromMiniKey bs = do
    guard checkShortKey
    wrapSecKey False <$> secKey (encode (sha256 bs))
  where
    checkHash = encode $ sha256 $ bs `BS.append` "?"
    checkShortKey = BS.length bs `elem` [22, 30] && BS.head checkHash == 0x00

-- | Decode private key from WIF (wallet import format) string.
fromWif :: Network -> Base58 -> Maybe SecKeyI
fromWif net wif = do
    bs <- decodeBase58Check wif
    -- Check that this is a private key
    guard (BS.head bs == getSecretPrefix net)
    case BS.length bs of
        -- Uncompressed format
        33 -> wrapSecKey False <$> secKey (BS.tail bs)
        -- Compressed format
        34 -> do
            guard $ BS.last bs == 0x01
            wrapSecKey True <$> secKey (BS.tail $ BS.init bs)
        -- Bad length
        _  -> Nothing

-- | Encode private key into a WIF string.
toWif :: Network -> SecKeyI -> Base58
toWif net (SecKeyI k c) =
    encodeBase58Check . BS.cons (getSecretPrefix net) $
    if c
        then getSecKey k `BS.snoc` 0x01
        else getSecKey k
