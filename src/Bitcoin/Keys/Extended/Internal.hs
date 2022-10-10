{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitcoin.Keys.Extended.Internal (
    Fingerprint (..),
    fingerprintToText,
    textToFingerprint,
) where

import Bitcoin.Util (decodeHex, encodeHex)
import qualified Bitcoin.Util as U
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Binary (Binary (..))
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BSL
import Data.Either (fromRight)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Read (readEither, readPrec)


-- | Fingerprint of parent
newtype Fingerprint = Fingerprint {unFingerprint :: Word32}
    deriving (Eq, Ord, Hashable, Typeable, Generic, NFData)


fingerprintToText :: Fingerprint -> Text
fingerprintToText = encodeHex . U.encodeS


textToFingerprint :: Text -> Either String Fingerprint
textToFingerprint = maybe (Left "Fingerprint: invalid hex") Right . decodeHex >=> U.decode . BSL.fromStrict


instance Show Fingerprint where
    show = show . Text.unpack . encodeHex . U.encodeS


instance Read Fingerprint where
    readPrec =
        readPrec
            >>= maybe (fail "Fingerprint: invalid hex") pure . decodeHex
            >>= either (fail . ("Fingerprint: " <>)) pure . U.decode . BSL.fromStrict


instance IsString Fingerprint where
    fromString =
        fromRight decodeError
            . U.decode
            . BSL.fromStrict
            . fromMaybe hexError
            . decodeHex
            . Text.pack
      where
        decodeError = error "Fingerprint literal: Unable to decode"
        hexError = error "Fingerprint literal: Invalid hex"


instance Binary Fingerprint where
    put = Put.putWord32be . unFingerprint
    get = Fingerprint <$> Get.getWord32be
