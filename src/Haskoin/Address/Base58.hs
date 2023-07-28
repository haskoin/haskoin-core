{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Address.Base58
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Support for legacy 'Base58' addresses. Superseded by Bech32 for Bitcoin SegWit
-- (BTC) and CashAddr for Bitcoin Cash (BCH).
module Haskoin.Address.Base58
  ( -- * Base58
    Base58,
    encodeBase58,
    decodeBase58,
    encodeBase58Check,
    decodeBase58Check,
  )
where

import Control.Monad (guard)
import Data.Array (Array, assocs, listArray, (!), (//))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Bytes.Get ()
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (serialize))
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Haskoin.Crypto.Hash (checkSum32)
import Haskoin.Util.Helpers (bsToInteger, integerToBS)
import Numeric (readInt, showIntAtBase)

-- | 'Base58' classic Bitcoin address format.
type Base58 = Text

-- | Symbols for Base58 encoding.
b58Data :: ByteString
b58Data = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Array :: Array Int Word8
b58Array = listArray (0, 57) (B.unpack b58Data)

b58InvArray :: Array Word8 (Maybe Int)
b58InvArray =
  listArray
    (minBound, maxBound)
    (repeat Nothing)
    // map swap (assocs b58Array)
  where
    swap (i, c) = (c, Just i)

-- | Convert a number less than or equal to provided integer into a 'Base58'
-- character.
b58 :: Int -> Word8
b58 = (b58Array !)

-- | Convert a 'Base58' character into the number it represents.
b58' :: Word8 -> Maybe Int
b58' = (b58InvArray !)

-- | Encode an arbitrary-length 'Integer' into a 'Base58' string. Leading zeroes
-- will not be part of the resulting string.
encodeBase58I :: Integer -> Base58
encodeBase58I i = cs $ showIntAtBase 58 (chr . fromIntegral . b58) i ""

-- | Decode a 'Base58' string into an arbitrary-length 'Integer'.
decodeBase58I :: Base58 -> Maybe Integer
decodeBase58I s =
  case go of
    Just (r, []) -> Just r
    _ -> Nothing
  where
    p = isJust . b58' . fromIntegral . ord
    f = fromMaybe e . b58' . fromIntegral . ord
    go = listToMaybe $ readInt 58 p f (cs s)
    e = error "Could not decode base58"

-- | Encode an arbitrary 'ByteString' into a its 'Base58' representation,
-- preserving leading zeroes.
encodeBase58 :: ByteString -> Base58
encodeBase58 bs =
  l <> r
  where
    (z, b) = B.span (== 0) bs
    l = cs $ B.replicate (B.length z) (b58 0) -- preserve leading 0's
    r
      | B.null b = T.empty
      | otherwise = encodeBase58I $ bsToInteger b

-- | Decode a 'Base58'-encoded 'Text' to a 'ByteString'.
decodeBase58 :: Base58 -> Maybe ByteString
decodeBase58 t =
  B.append prefix <$> r
  where
    (z, b) = B.span (== b58 0) (cs t)
    prefix = B.replicate (B.length z) 0 -- preserve leading 1's
    r
      | B.null b = Just B.empty
      | otherwise = integerToBS <$> decodeBase58I (cs b)

-- | Computes a checksum for the input 'ByteString' and encodes the input and
-- the checksum as 'Base58'.
encodeBase58Check :: ByteString -> Base58
encodeBase58Check bs =
  (encodeBase58 . B.append bs . runPutS . serialize . checkSum32) bs

-- | Decode a 'Base58'-encoded string that contains a checksum. This function
-- returns 'Nothing' if the input string contains invalid 'Base58' characters or
-- if the checksum fails.
decodeBase58Check :: Base58 -> Maybe ByteString
decodeBase58Check bs = do
  rs <- decodeBase58 bs
  let (res, chk) = B.splitAt (B.length rs - 4) rs
  guard $ chk == (runPutS . serialize . checkSum32) res
  return res
