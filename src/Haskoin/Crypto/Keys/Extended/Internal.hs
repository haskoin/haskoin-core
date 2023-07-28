{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Haskoin.Crypto.Keys.Extended.Internal
  ( Fingerprint (..),
    fingerprintToText,
    textToFingerprint,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    withText,
  )
import Data.Aeson.Encoding (text)
import Data.Binary (Binary (..))
import Data.Bytes.Get (getWord32be)
import Data.Bytes.Put (putWord32be)
import Data.Bytes.Serial (Serial (..))
import Data.Either (fromRight)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize (..))
import Data.Serialize qualified as S
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Haskoin.Util.Helpers (decodeHex, encodeHex)
import Text.Read (readEither, readPrec)

-- | Fingerprint of parent
newtype Fingerprint = Fingerprint {get :: Word32}
  deriving (Eq, Ord, Hashable, Typeable, Generic, NFData)

fingerprintToText :: Fingerprint -> Text
fingerprintToText = encodeHex . S.encode

textToFingerprint :: Text -> Either String Fingerprint
textToFingerprint =
  maybe (Left "Fingerprint: invalid hex") Right . decodeHex >=> S.decode

instance Show Fingerprint where
  show = show . Text.unpack . encodeHex . S.encode

instance Read Fingerprint where
  readPrec =
    readPrec
      >>= maybe (fail "Fingerprint: invalid hex") pure . decodeHex
      >>= either (fail . ("Fingerprint: " <>)) pure . S.decode

instance IsString Fingerprint where
  fromString =
    fromRight decodeError
      . S.decode
      . fromMaybe hexError
      . decodeHex
      . Text.pack
    where
      decodeError = error "Fingerprint literal: Unable to decode"
      hexError = error "Fingerprint literal: Invalid hex"

instance Serial Fingerprint where
  serialize = putWord32be . (.get)
  deserialize = Fingerprint <$> getWord32be

instance Binary Fingerprint where
  put = serialize
  get = deserialize

instance Serialize Fingerprint where
  put = serialize
  get = deserialize

instance FromJSON Fingerprint where
  parseJSON = withText "Fingerprint" $ either fail pure . textToFingerprint

instance ToJSON Fingerprint where
  toJSON = toJSON . fingerprintToText
  toEncoding = text . fingerprintToText