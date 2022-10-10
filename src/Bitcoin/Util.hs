{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- This module defines various utility functions used across the library.
module Bitcoin.Util (
    -- * ByteString Helpers
    bsToInteger,
    integerToBS,
    hexBuilder,
    encodeHex,
    encodeHexLazy,
    decodeHex,
    decodeHexLazy,
    getBits,

    -- * Maybe & Either Helpers
    eitherToMaybe,
    maybeToEither,
    liftMaybe,

    -- * Other Helpers
    updateIndex,
    matchTemplate,
    convertBits,

    -- * Triples
    fst3,
    snd3,
    lst3,

    -- * Serialization Helpers
    encodeS,
    decode,
    runGet,
    putList,
    getList,
    putMaybe,
    getMaybe,
    putLengthBytes,
    getLengthBytes,
    putInteger,
    getInteger,
    putInt32be,
    getInt32be,
    putInt64be,
    getInt64be,
    getIntMap,
    putIntMap,
    getTwo,
    putTwo,
) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Except (ExceptT (..), except)
import Data.Bifunctor (bimap)
import Data.Binary (Binary, Get, Put)
import qualified Data.Binary as Bin
import qualified Data.Binary as Get
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Builder (Builder, lazyByteStringHex)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Base16 as BL16
import Data.Int (Int32, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl', unfoldr)
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import Data.Word (Word8)


-- ByteString helpers

-- | Decode a big endian 'Integer' from a 'ByteString'.
bsToInteger :: BSL.ByteString -> Integer
bsToInteger = BSL.foldr f 0 . BSL.reverse
  where
    f w n = toInteger w .|. shiftL n 8


-- | Encode an 'Integer' to a 'ByteString' as big endian.
integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0 = BS.reverse $ BS.unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)


hexBuilder :: BSL.ByteString -> Builder
hexBuilder = lazyByteStringHex


encodeHex :: ByteString -> Text
encodeHex = B16.encodeBase16


-- | Encode as string of human-readable hex characters.
encodeHexLazy :: BSL.ByteString -> TL.Text
encodeHexLazy = BL16.encodeBase16


decodeHex :: Text -> Maybe ByteString
decodeHex = eitherToMaybe . B16.decodeBase16 . E.encodeUtf8


-- | Decode string of human-readable hex characters.
decodeHexLazy :: TL.Text -> Maybe BSL.ByteString
decodeHexLazy = eitherToMaybe . BL16.decodeBase16 . EL.encodeUtf8


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

-- | Transform an 'Either' value into a 'Maybe' value. 'Right' is mapped to
-- 'Just' and 'Left' is mapped to 'Nothing'. The value inside 'Left' is lost.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing


-- | Transform a 'Maybe' value into an 'Either' value. 'Just' is mapped to
-- 'Right' and 'Nothing' is mapped to 'Left'. Default 'Left' required.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right


-- | Lift a 'Maybe' computation into the 'ExceptT' monad.
liftMaybe :: Monad m => b -> Maybe a -> ExceptT b m a
liftMaybe err = except . maybeToEither err


-- Various helpers

-- | Applies a function to only one element of a list defined by its index.  If
-- the index is out of the bounds of the list, the original list is returned.
updateIndex ::
    -- | index of the element to change
    Int ->
    -- | list of elements
    [a] ->
    -- | function to apply
    (a -> a) ->
    -- | result with one element changed
    [a]
updateIndex i xs f
    | i < 0 || i >= length xs = xs
    | otherwise = l ++ (f h : r)
  where
    (l, h : r) = splitAt i xs


-- | Use the list @[b]@ as a template and try to match the elements of @[a]@
-- against it. For each element of @[b]@ return the (first) matching element of
-- @[a]@, or 'Nothing'. Output list has same size as @[b]@ and contains results
-- in same order. Elements of @[a]@ can only appear once.
matchTemplate ::
    -- | input list
    [a] ->
    -- | list to serve as a template
    [b] ->
    -- | comparison function
    (a -> b -> Bool) ->
    [Maybe a]
matchTemplate [] bs _ = replicate (length bs) Nothing
matchTemplate _ [] _ = []
matchTemplate as (b : bs) f = case break (`f` b) as of
    (l, r : rs) -> Just r : matchTemplate (l ++ rs) bs f
    _ -> Nothing : matchTemplate as bs f


-- | Returns the first value of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


-- | Returns the second value of a triple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b


-- | Returns the last value of a triple.
lst3 :: (a, b, c) -> c
lst3 (_, _, c) = c


-- | Convert from one power-of-two base to another, as long as it fits in a
-- 'Word'.
convertBits :: Bool -> Int -> Int -> [Word] -> ([Word], Bool)
convertBits pad frombits tobits i = (reverse yout, rem')
  where
    (xacc, xbits, xout) = foldl' outer (0, 0, []) i
    (yout, rem')
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


--
-- Serialization helpers
--

encodeS :: Binary a => a -> ByteString
encodeS = BSL.toStrict . Bin.encode


decode :: Binary a => BSL.ByteString -> Either String a
decode = bimap lst3 lst3 . Get.decodeOrFail


runGet :: Get a -> BSL.ByteString -> Either String a
runGet parser = bimap lst3 lst3 . Get.runGetOrFail parser


putInt32be :: Int32 -> Put
putInt32be n
    | n < 0 = Put.putWord32be (complement (fromIntegral (abs n)) + 1)
    | otherwise = Put.putWord32be (fromIntegral (abs n))


getInt32be :: Get Int32
getInt32be = do
    n <- Get.getWord32be
    if testBit n 31
        then return (negate (complement (fromIntegral n) + 1))
        else return (fromIntegral n)


putInt64be :: Int64 -> Put
putInt64be n
    | n < 0 = Put.putWord64be (complement (fromIntegral (abs n)) + 1)
    | otherwise = Put.putWord64be (fromIntegral (abs n))


getInt64be :: Get Int64
getInt64be = do
    n <- Get.getWord64be
    if testBit n 63
        then return (negate (complement (fromIntegral n) + 1))
        else return (fromIntegral n)


putInteger :: Integer -> Put
putInteger n
    | n >= lo && n <= hi = do
        Put.putWord8 0x00
        putInt32be (fromIntegral n)
    | otherwise = do
        Put.putWord8 0x01
        Put.putWord8 (fromIntegral (signum n))
        let len = (nrBits (abs n) + 7) `div` 8
        Put.putWord64be (fromIntegral len)
        mapM_ Put.putWord8 (unroll (abs n))
  where
    lo = fromIntegral (minBound :: Int32)
    hi = fromIntegral (maxBound :: Int32)


getInteger :: Get Integer
getInteger =
    Get.getWord8 >>= \case
        0 -> fromIntegral <$> getInt32be
        _ -> do
            sign <- Get.getWord8
            bytes <- getList Get.getWord8
            let v = roll bytes
            return $! if sign == 0x01 then v else -v


putMaybe :: (a -> Put) -> Maybe a -> Put
putMaybe f Nothing = Put.putWord8 0x00
putMaybe f (Just x) = Put.putWord8 0x01 >> f x


getMaybe :: Get a -> Get (Maybe a)
getMaybe f =
    Get.getWord8 >>= \case
        0x00 -> return Nothing
        0x01 -> Just <$> f
        _ -> fail "Not a Maybe"


putLengthBytes :: ByteString -> Put
putLengthBytes bs = do
    Put.putWord64be (fromIntegral (BS.length bs))
    Put.putByteString bs


getLengthBytes :: Get ByteString
getLengthBytes = do
    len <- fromIntegral <$> Get.getWord64be
    Get.getByteString len


--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)


roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b


nrBits :: (Ord a, Integral a) => a -> Int
nrBits k =
    let expMax = until (\e -> 2 ^ e > k) (* 2) 1
        findNr :: Int -> Int -> Int
        findNr lo hi
            | mid == lo = hi
            | 2 ^ mid <= k = findNr mid hi
            | 2 ^ mid > k = findNr lo mid
          where
            mid = (lo + hi) `div` 2
     in findNr (expMax `div` 2) expMax


-- | Read as a list of pairs of int and element.
getIntMap :: Get Int -> Get a -> Get (IntMap a)
getIntMap i m = IntMap.fromList <$> getList (getTwo i m)


putIntMap :: (Int -> Put) -> (a -> Put) -> IntMap a -> Put
putIntMap f g = putList (putTwo f g) . IntMap.toAscList


putTwo :: (a -> Put) -> (b -> Put) -> (a, b) -> Put
putTwo f g (x, y) = f x >> g y


getTwo :: Get a -> Get b -> Get (a, b)
getTwo f g = (,) <$> f <*> g


putList :: (a -> Put) -> [a] -> Put
putList f ls = do
    Put.putWord64be (fromIntegral (length ls))
    mapM_ f ls


getList :: Get a -> Get [a]
getList f = do
    l <- fromIntegral <$> Get.getWord64be
    replicateM l f
