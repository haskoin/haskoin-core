{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Address where

import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson                      as A
import qualified Data.Array                      as Arr
import           Data.Bits
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as C
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Serialize                  as S
import           Data.String
import           Data.String.Conversions
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Base58
import           Network.Haskoin.Crypto.Bech32
import           Network.Haskoin.Crypto.CashAddr
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Util
import           Text.Read                       as R

-- | Data type representing a Bitcoin address
data Address
    -- | Public Key Hash Address
    = PubKeyAddress { getAddrHash :: !Hash160 }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: !Hash160 }
    -- | SegWit Public Key Hash Address
    | WitnessPubKeyAddress { getAddrHash :: !Hash160 }
    -- | SegWit Script Hash Address
    | WitnessScriptAddress { getScriptHash :: !Hash256 }
    deriving (Eq, Ord)

base58get :: Get Address
base58get = do
        pfx <- getWord8
        addr <- S.get
        f pfx addr
      where
        f x a | x == addrPrefix   = return (PubKeyAddress a)
              | x == scriptPrefix = return (ScriptAddress a)
              | otherwise = fail "Does not recognize address prefix"

base58put :: Putter Address
base58put (PubKeyAddress h) = do
        putWord8 addrPrefix
        put h
base58put (ScriptAddress h) = do
        putWord8 scriptPrefix
        put h

instance Show Address where
    showsPrec d a = showParen (d > 10) $
        showString "Address " . shows (addrToString a)

instance Read Address where
    readPrec = parens $ do
        R.Ident "Address" <- lexP
        R.String str <- lexP
        maybe pfail return $ stringToAddr $ cs str

instance IsString Address where
    fromString =
        fromMaybe e . stringToAddr . cs
      where
        e = error "Could not decode bitcoin address"

instance NFData Address where
    rnf (PubKeyAddress h) = rnf h
    rnf (ScriptAddress h) = rnf h

instance FromJSON Address where
    parseJSON = withText "address" $ maybe mzero return . stringToAddr . cs

instance ToJSON Address where
    toJSON = A.String . cs . addrToString

-- | Transforms an Address into an encoded String
addrToString :: Address -> ByteString
addrToString = encodeBase58Check . runPut . base58put

-- | Decodes an Address from an encoded String. This function can fail
-- if the String is not properly encoded or its checksum fails.
stringToAddr :: ByteString -> Maybe Address
stringToAddr = eitherToMaybe . runGet base58get <=< decodeBase58Check
