{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  This module defines various utility functions used across the
  Network.Haskoin modules.
-}
module Network.Haskoin.Util
    (
      -- * ByteString Helpers
      bsToInteger
    , integerToBS
    , encodeHex
    , decodeHex
    , getBits

      -- * Maybe & Either Helpers
    , eitherToMaybe
    , maybeToEither
    , liftEither
    , liftMaybe

      -- * Other Helpers
    , updateIndex
    , matchTemplate
    , convertBits

      -- * Triples
    , fst3
    , snd3
    , lst3

      -- * JSON Utilities
    , dropFieldLabel
    , dropSumLabels

    ) where

import           Control.Monad          (guard)
import           Control.Monad.Except   (ExceptT (..))
import           Data.Aeson.Types       (Options (..), SumEncoding (..),
                                         defaultOptions, defaultTaggedObject)
import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Char              (toLower)
import           Data.List
import           Data.Word              (Word8)

-- ByteString helpers

-- | Decode a big endian Integer from a bytestring.
bsToInteger :: ByteString -> Integer
bsToInteger = BS.foldr f 0 . BS.reverse
  where
    f w n = toInteger w .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.reverse $ BS.unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)

encodeHex :: ByteString -> ByteString
encodeHex = B16.encode

-- | Decode hexadecimal 'ByteString'. This function can fail if the string
-- contains invalid hexadecimal (0-9, a-f, A-F) characters
decodeHex :: ByteString -> Maybe ByteString
decodeHex bs =
    let (x, b) = B16.decode bs
    in guard (b == BS.empty) >> return x

-- | Obtain 'Int' bits from beginning of 'ByteString'. Resulting 'ByteString'
-- will be smallest required to hold that many bits, padded with zeroes to the
-- right.
getBits :: Int -> ByteString -> ByteString
getBits b bs
    | r == 0 = BS.take q bs
    | otherwise = i `BS.snoc` l
  where
    (q, r) = b `quotRem` 8
    s = BS.take (q + 1) bs
    i = BS.init s
    l = BS.last s .&. (0xff `shiftL` (8 - r)) -- zero unneeded bits

-- Maybe and Either monad helpers

-- | Transforms an 'Either' value into a 'Maybe' value. 'Right' is mapped to 'Just'
-- and 'Left' is mapped to 'Nothing'. The value inside 'Left' is lost.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing

-- | Transforms a 'Maybe' value into an 'Either' value. 'Just' is mapped to
-- 'Right' and 'Nothing' is mapped to 'Left'. You also pass in an error value
-- in case 'Left' is returned.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right

-- | Lift a 'Either' computation into the 'ExceptT' monad
liftEither :: Monad m => Either b a -> ExceptT b m a
liftEither = ExceptT . return

-- | Lift a 'Maybe' computation into the 'ExceptT' monad
liftMaybe :: Monad m => b -> Maybe a -> ExceptT b m a
liftMaybe err = liftEither . maybeToEither err

-- Various helpers

-- | Applies a function to only one element of a list defined by its index.  If
-- the index is out of the bounds of the list, the original list is returned.
updateIndex :: Int      -- ^ The index of the element to change
            -> [a]      -- ^ The list of elements
            -> (a -> a) -- ^ The function to apply
            -> [a]      -- ^ The result with one element changed
updateIndex i xs f
    | i < 0 || i >= length xs = xs
    | otherwise = l ++ (f h : r)
  where
    (l,h:r) = splitAt i xs

-- | Use the list @[b]@ as a template and try to match the elements of @[a]@
-- against it. For each element of @[b]@ return the (first) matching element of
-- @[a]@, or 'Nothing'. Output list has same size as @[b]@ and contains results
-- in same order. Elements of @[a]@ can only appear once.
matchTemplate :: [a]              -- ^ The input list
              -> [b]              -- ^ The list to serve as a template
              -> (a -> b -> Bool) -- ^ The comparison function
              -> [Maybe a]        -- ^ Results of the template matching
matchTemplate [] bs _ = replicate (length bs) Nothing
matchTemplate _  [] _ = []
matchTemplate as (b:bs) f = case break (`f` b) as of
    (l,r:rs) -> Just r  : matchTemplate (l ++ rs) bs f
    _        -> Nothing : matchTemplate as bs f

-- | Returns the first value of a triple.
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- | Returns the second value of a triple.
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

-- | Returns the last value of a triple.
lst3 :: (a,b,c) -> c
lst3 (_,_,c) = c

dropFieldLabel :: Int -> Options
dropFieldLabel n = defaultOptions
    { fieldLabelModifier = map toLower . drop n
    , omitNothingFields  = False -- TODO: aeson issue #293 prompted this
    }

dropSumLabels :: Int -> Int -> String -> Options
dropSumLabels c f tag = (dropFieldLabel f)
    { constructorTagModifier = map toLower . drop c
    , sumEncoding = defaultTaggedObject { tagFieldName = tag }
    }

-- | Convert from one power-of-two base to another.
convertBits :: Bool -> Int -> Int -> [Word] -> ([Word], Bool)
convertBits pad frombits tobits i = (reverse yout, rem)
  where
    (xacc, xbits, xout) = foldl' outer (0, 0, []) i
    (yout, rem)
        | pad && xbits /= 0 =
            let xout' = (xacc `shiftL` (tobits - xbits)) .&. maxv : xout
            in (xout', False)
        | pad = (xout, False)
        | xbits /= 0 = (xout, True)
        | otherwise = (xout, False)
    maxv = 1 `shiftL` tobits - 1
    max_acc = 1 `shiftL` (frombits + tobits - 1) - 1
    outer (acc, bits, out) it =
        let acc' = ((acc `shiftL` frombits) .|. it) .&. max_acc
            bits' = bits + frombits
            (out', bits'') = inner acc' out bits'
        in (acc', bits'', out')
    inner acc out bits
        | bits >= tobits =
            let bits' = bits - tobits
                out' = ((acc `shiftR` bits') .&. maxv) : out
            in inner acc out' bits'
        | otherwise = (out, bits)
