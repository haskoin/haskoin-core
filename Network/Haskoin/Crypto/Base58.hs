module Network.Haskoin.Crypto.Base58
( Address(..)
, addrToBase58
, base58ToAddr
, encodeBase58
, decodeBase58
, encodeBase58Check
, decodeBase58Check
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (guard, mzero)
import Control.Applicative ((<$>),(<*>))

import Data.Char (ord, chr)
import Data.Word (Word8)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Numeric (showIntAtBase, readInt)
import Data.String (fromString)
import Data.Aeson
    ( Value (String)
    , FromJSON
    , ToJSON
    , parseJSON
    , toJSON
    , withText 
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Network
import Network.Haskoin.Util 

b58Data :: BS.ByteString
b58Data = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> BS.elemIndex w b58Data

encodeBase58I :: Integer -> BS.ByteString
encodeBase58I i = 
    fromString $ showIntAtBase (58 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b58 . fromIntegral

decodeBase58I :: BS.ByteString -> Maybe Integer
decodeBase58I s = case go of 
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b58' . fromIntegral . ord
    p = isJust . c 
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 58 p f (B8.unpack s)

-- | Encode a bytestring to a base 58 representation.
encodeBase58 :: BS.ByteString -> BS.ByteString
encodeBase58 bs = BS.append l r
  where 
    (z,b) = BS.span (== 0) bs
    l = BS.map b58 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = encodeBase58I $ bsToInteger b

-- | Decode a base 58 encoded bytestring. This can fail if the input bytestring
-- contains invalid base 58 characters such as 0,O,l,I
decodeBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBase58 bs = r >>= return . (BS.append prefix)
  where 
    (z,b)  = BS.span (== (b58 0)) bs
    prefix = BS.map (fromJust . b58') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> decodeBase58I b

-- | Computes a checksum for the input bytestring and encodes the input and
-- the checksum to a base 58 representation.
encodeBase58Check :: BS.ByteString -> BS.ByteString
encodeBase58Check bs = encodeBase58 $ BS.append bs chk
  where 
    chk = encode' $ chksum32 bs

-- | Decode a base 58 encoded bytestring that contains a checksum. This
-- function returns Nothing if the input bytestring contains invalid base 58
-- characters or if the checksum fails.
decodeBase58Check :: BS.ByteString -> Maybe BS.ByteString
decodeBase58Check bs = do
    rs <- decodeBase58 bs
    let (res,chk) = BS.splitAt ((BS.length rs) - 4) rs
    guard $ chk == (encode' $ chksum32 res)
    return res

-- |Data type representing a Bitcoin address
data Address a
    -- | Public Key Hash Address
    = PubKeyAddress { getAddrHash :: Word160 }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: Word160 }
       deriving (Eq, Ord, Show, Read)

instance NFData (Address a) where
    rnf (PubKeyAddress h) = rnf h
    rnf (ScriptAddress h) = rnf h

instance Network a => FromJSON (Address a) where
    parseJSON = withText "Address" $ 
        maybe mzero return . base58ToAddr . T.unpack

instance Network a => ToJSON (Address a) where
    toJSON = String . T.pack . addrToBase58

-- | Transforms an Address into a base58 encoded String
addrToBase58 :: forall a. Network a => Address a -> String
addrToBase58 addr = bsToString $ encodeBase58Check $ case addr of
    PubKeyAddress i -> BS.cons (addrPrefix net) $ encode' i
    ScriptAddress i -> BS.cons (scriptPrefix net) $ encode' i
  where
    net = undefined :: a

-- | Decodes an Address from a base58 encoded String. This function can fail
-- if the String is not properly encoded as base58 or the checksum fails.
base58ToAddr :: forall a. Network a => String -> Maybe (Address a)
base58ToAddr str = do
    val <- decodeBase58Check $ stringToBS str
    let f | BS.head val == addrPrefix net   = Just PubKeyAddress
          | BS.head val == scriptPrefix net = Just ScriptAddress
          | otherwise = Nothing
    f <*> decodeToMaybe (BS.tail val)
  where
    net = undefined :: a
