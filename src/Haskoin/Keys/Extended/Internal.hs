{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Haskoin.Keys.Extended.Internal (
    Fingerprint (..)
) where

import           Control.DeepSeq   (NFData)
import           Data.Binary       (Binary (..))
import           Data.Bytes.Get    (getWord32be)
import           Data.Bytes.Put    (putWord32be)
import           Data.Bytes.Serial (Serial (..))
import           Data.Either       (fromRight)
import           Data.Hashable     (Hashable)
import           Data.Maybe        (fromMaybe)
import           Data.Serialize    (Serialize (..))
import qualified Data.Serialize    as S
import           Data.String       (IsString (..))
import qualified Data.Text         as Text
import           Data.Typeable     (Typeable)
import           Data.Word         (Word32)
import           GHC.Generics      (Generic)
import           Haskoin.Util      (decodeHex, encodeHex)
import           Text.Read         (readPrec)

-- | Fingerprint of parent
newtype Fingerprint = Fingerprint { unFingerprint :: Word32 }
    deriving (Eq, Ord, Hashable, Typeable, Generic, NFData)

instance Show Fingerprint where
    show = show . Text.unpack . encodeHex . S.encode

instance Read Fingerprint where
    readPrec =
        readPrec
            >>= maybe (fail "Fingerprint: invalid hex") pure . decodeHex
            >>= either (fail . ("Fingerprint: " <>)) pure . S.decode

instance IsString Fingerprint where
    fromString = fromRight decodeError
        . S.decode
        . fromMaybe hexError
        . decodeHex
        . Text.pack
     where
       decodeError  = error "Fingerprint literal: Unable to decode"
       hexError = error "Fingerprint literal: Invalid hex"

instance Serial Fingerprint where
    serialize = putWord32be . unFingerprint
    deserialize = Fingerprint <$> getWord32be

instance Binary Fingerprint where
    put = serialize
    get = deserialize

instance Serialize Fingerprint where
    put = serialize
    get = deserialize
