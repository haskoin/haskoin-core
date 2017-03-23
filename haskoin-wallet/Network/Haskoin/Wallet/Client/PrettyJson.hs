{-# LANGUAGE CPP #-}
module Network.Haskoin.Wallet.Client.PrettyJson
( encodePretty )
where

import qualified Data.ByteString.Lazy  as BL
import Data.Aeson                                (ToJSON)
import Data.Aeson.Encode.Pretty        as Export (Config (..),
                                                  defConfig,
                                                  encodePretty')

-- aeson-pretty 0.8.0 introduces a new way to specify indentation
#if MIN_VERSION_aeson_pretty(0,8,0)
import Data.Aeson.Encode.Pretty        as Export (Indent(..))
jsonIndent :: Indent
jsonIndent = Spaces 2
#else
jsonIndent :: Int
jsonIndent = 2
#endif

encodePretty :: ToJSON a => a -> BL.ByteString
encodePretty = encodePretty' defConfig{ confIndent = jsonIndent }
