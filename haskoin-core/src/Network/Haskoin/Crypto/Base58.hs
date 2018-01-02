{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Base58
( Address(..)
, addrToBase58
, base58ToAddr
, encodeBase58
, decodeBase58
, encodeBase58Check
, decodeBase58Check
) where

import           Control.DeepSeq             (NFData, rnf)
import           Control.Monad               (guard, mzero)
import           Data.Aeson                  (FromJSON, ToJSON, Value (String),
                                              parseJSON, toJSON, withText)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C
import           Data.Maybe                  (fromJust, fromMaybe, isJust,
                                              listToMaybe)
import           Data.Serialize              (Serialize, decode, encode, get,
                                              put)
import           Data.Serialize.Get          (getByteString, getWord8)
import           Data.Serialize.Put          (putByteString, putWord8)
import           Data.String                 (IsString, fromString)
import           Data.String.Conversions     (cs)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Util
import           Numeric                     (readInt, showIntAtBase)
import           Text.Read                   (lexP, parens, pfail, readPrec)
import qualified Text.Read                   as Read (Lexeme (Ident, String))

b58Data :: ByteString
b58Data = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58 :: Int -> Char
b58 = C.index b58Data

b58' :: Char -> Maybe Int
b58' = flip C.elemIndex b58Data

encodeBase58I :: Integer -> ByteString
encodeBase58I i = cs $ showIntAtBase 58 b58 i ""

decodeBase58I :: ByteString -> Maybe Integer
decodeBase58I s =
    case go of
        Just (r,[]) -> Just r
        _           -> Nothing
  where
    p  = isJust . b58'
    f  = fromMaybe e . b58'
    go = listToMaybe $ readInt 58 p f (cs s)
    e  = error "Could not decode base58"

-- | Encode a 'ByteString' to a base 58 representation.
encodeBase58 :: ByteString -> ByteString
encodeBase58 bs =
    l `mappend` r
  where
    (z, b) = BS.span (== 0) bs
    l = BS.replicate (BS.length z) (BS.index b58Data 0) -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = encodeBase58I $ bsToInteger b

-- | Decode a base58-encoded 'ByteString'. This can fail if the input
-- 'ByteString' contains invalid base58 characters such as 0, O, l, I.
decodeBase58 :: ByteString -> Maybe ByteString
decodeBase58 t =
    BS.append prefix <$> r
  where
    (z, b) = BS.span (== BS.index b58Data 0) t
    prefix = BS.replicate (BS.length z) 0 -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> decodeBase58I b

-- | Computes a checksum for the input 'ByteString' and encodes the input and
-- the checksum to a base58 representation.
encodeBase58Check :: ByteString -> ByteString
encodeBase58Check bs =
    encodeBase58 $ BS.append bs $ encode $ checkSum32 bs

-- | Decode a base58-encoded string that contains a checksum. This function
-- returns 'Nothing' if the input string contains invalid base58 characters or
-- if the checksum fails.
decodeBase58Check :: ByteString -> Maybe ByteString
decodeBase58Check bs = do
    rs <- decodeBase58 bs
    let (res, chk) = BS.splitAt (BS.length rs - 4) rs
    guard $ chk == encode (checkSum32 res)
    return res

-- | Data type representing a Bitcoin address
data Address
    -- | Public Key Hash Address
    = PubKeyAddress { getAddrHash :: !Hash160 }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: !Hash160 }
       deriving (Eq, Ord)

instance Serialize Address where
    get = do
        pfx <- getWord8
        bs <- getByteString 20
        let addr = fromJust (bsToHash160 bs)
        f pfx addr
      where
        f x a | x == addrPrefix   = return (PubKeyAddress a)
              | x == scriptPrefix = return (ScriptAddress a)
              | otherwise = fail "Does not recognize address prefix"
    put (PubKeyAddress h) = do
        putWord8 addrPrefix
        putByteString (hash160ToBS h)
    put (ScriptAddress h) = do
        putWord8 scriptPrefix
        putByteString (hash160ToBS h)

-- TODO: Test
instance Show Address where
    showsPrec d a = showParen (d > 10) $
        showString "Address " . shows (addrToBase58 a)

-- TODO: Test
instance Read Address where
    readPrec = parens $ do
        Read.Ident "Address" <- lexP
        Read.String str <- lexP
        maybe pfail return $ base58ToAddr $ cs str

-- TODO: Test
instance IsString Address where
    fromString =
        fromMaybe e . base58ToAddr . cs
      where
        e = error "Could not decode bitcoin address"

instance NFData Address where
    rnf (PubKeyAddress h) = rnf h
    rnf (ScriptAddress h) = rnf h

instance FromJSON Address where
    parseJSON = withText "Address" $
        maybe mzero return . base58ToAddr . cs

instance ToJSON Address where
    toJSON = String . cs . addrToBase58

-- | Transforms an Address into a base58 encoded String
addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58Check . encode

-- | Decodes an Address from a base58 encoded String. This function can fail
-- if the String is not properly encoded as base58 or the checksum fails.
base58ToAddr :: ByteString -> Maybe Address
base58ToAddr str = do
    val <- decodeBase58Check str
    eitherToMaybe $ decode val

