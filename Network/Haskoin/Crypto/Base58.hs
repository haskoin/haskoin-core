module Network.Haskoin.Crypto.Base58
( Address(..)
, addrToBase58
, base58ToAddr
, encodeBase58
, decodeBase58
, encodeBase58Check
, decodeBase58Check
) where

import Control.Monad (guard)
import Control.Applicative ((<$>),(<*>))

import Data.Char (ord)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Aeson
    ( Value (String)
    , FromJSON
    , ToJSON
    , parseJSON
    , toJSON
    , withText 
    )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Util 

b58String :: String
b58String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Data :: BS.ByteString
b58Data = BS.pack $ map (fromIntegral . ord) b58String

b58Data' :: M.Map Word8 Int
b58Data' = M.fromList $ zip (BS.unpack b58Data) [0..57]

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> M.lookup w b58Data'

encodeBase58I :: Integer -> BS.ByteString
encodeBase58I 0 = BS.pack [b58 0]
encodeBase58I i
    | i >= 0 = go BS.empty i
    | otherwise = error "encodeBase58 is not defined for negative Integers"
  where 
    go acc 0 = acc
    go acc n = go (BS.cons (fromIntegral b) acc) q
      where 
        (q,r) = n `quotRem` 58
        b     = b58 $ fromIntegral r

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
      | otherwise = integerToBS <$> foldl f (Just 0) (BS.unpack b)
    f i w  = do
        n <- fromIntegral <$> b58' w
        p <- i
        return $ p*58 + n

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
data Address 
    -- | Public Key Hash Address
    = PubKeyAddress { getAddrHash :: Hash160 }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: Hash160 }
       deriving (Eq, Show)

instance FromJSON Address where
    parseJSON = withText "Address not a string: " $ \a -> do
        let s = T.unpack a
        maybe (fail $ "Not a Bitcoin address: " ++ s) return $ base58ToAddr s

instance ToJSON Address where
    toJSON = String . T.pack . addrToBase58

-- | Transforms an Address into a base58 encoded String
addrToBase58 :: Address -> String
addrToBase58 addr = bsToString $ encodeBase58Check $ case addr of
    PubKeyAddress i -> BS.cons addrPrefix $ encode' i
    ScriptAddress i -> BS.cons scriptPrefix $ encode' i

-- | Decodes an Address from a base58 encoded String. This function can fail
-- if the String is not properly encoded as base58 or the checksum fails.
base58ToAddr :: String -> Maybe Address
base58ToAddr str = do
    val <- decodeBase58Check $ stringToBS str
    let f | BS.head val == addrPrefix   = Just PubKeyAddress
          | BS.head val == scriptPrefix = Just ScriptAddress
          | otherwise = Nothing
    f <*> decodeToMaybe (BS.tail val)

