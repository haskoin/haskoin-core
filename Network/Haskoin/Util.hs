{-|
  This module defines various utility functions used across the
  Network.Haskoin modules.
-}
module Network.Haskoin.Util
(
  -- * ByteString helpers
  bsToInteger
, integerToBS
, encodeHex
, decodeHex

  -- * Data.Binary helpers
, encode'
, decode'
, runPut'
, runGet'
, decodeOrFail'
, runGetOrFail'
, fromDecode
, fromRunGet
, decodeToEither
, decodeToMaybe
, isolate

  -- * Maybe and Either monad helpers
, isLeft
, isRight
, fromRight
, fromLeft
, eitherToMaybe
, maybeToEither
, liftEither
, liftMaybe

  -- * Various helpers
, updateIndex
, matchTemplate

  -- * Triples
, fst3
, snd3
, lst3

  -- * MonadState
, modify'

  -- * JSON Utilities
, dropFieldLabel
, dropSumLabels

) where

import Control.Monad (guard)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Control.Monad.State (MonadState, get, put)

import Data.Word (Word8)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Char (toLower)
import Data.Binary.Put (Put, runPut)
import Data.Binary (Binary, encode, decode, decodeOrFail)
import Data.Binary.Get (Get, runGetOrFail, getByteString, ByteOffset, runGet)
import Data.Aeson.Types
    (Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
    (toStrict, fromStrict, toStrict)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
    (pack, null, empty, foldr', reverse, unfoldr)

-- ByteString helpers

-- | Decode a big endian Integer from a bytestring.
bsToInteger :: ByteString -> Integer
bsToInteger = BS.foldr' f 0 . BS.reverse
  where
    f w n = (toInteger w) .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.reverse $ BS.unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

encodeHex :: ByteString -> ByteString
encodeHex = B16.encode

-- | Decode hexadecimal 'ByteString'. This function can fail if the string
-- contains invalid hexadecimal (0-9, a-f, A-F) characters
decodeHex :: ByteString -> Maybe ByteString
decodeHex bs =
    let (x, b) = B16.decode bs
    in guard (b == BS.empty) >> return x

-- Data.Binary helpers

-- | Strict version of 'Data.Binary.encode'
encode' :: Binary a => a -> ByteString
encode' = BL.toStrict . encode

-- | Strict version of 'Data.Binary.decode'
decode' :: Binary a => ByteString -> a
decode' = decode . BL.fromStrict

-- | Strict version of 'Data.Binary.runGet'
runGet' :: Binary a => Get a -> ByteString -> a
runGet' m = (runGet m) . BL.fromStrict

-- | Strict version of 'Data.Binary.runPut'
runPut' :: Put -> ByteString
runPut' = BL.toStrict . runPut

-- | Strict version of 'Data.Binary.decodeOrFail'
decodeOrFail' ::
    Binary a =>
    ByteString ->
    Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)
decodeOrFail' bs = case decodeOrFail $ BL.fromStrict bs of
    Left  (lbs, o, err) -> Left  (BL.toStrict lbs, o, err)
    Right (lbs, o, res) -> Right (BL.toStrict lbs, o, res)

-- | Strict version of 'Data.Binary.runGetOrFail'
runGetOrFail' ::
    Binary a => Get a -> ByteString ->
    Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)
runGetOrFail' m bs = case runGetOrFail m $ BL.fromStrict bs of
    Left  (lbs, o, err) -> Left  (BL.toStrict lbs, o, err)
    Right (lbs, o, res) -> Right (BL.toStrict lbs, o, res)

-- | Try to decode a 'Data.Binary' value. If decoding succeeds, apply the
-- function to the result. Otherwise, return the default value.
fromDecode :: Binary a
           => ByteString    -- ^ The bytestring to decode
           -> b             -- ^ Default value to return when decoding fails
           -> (a -> b)      -- ^ Function to apply when decoding succeeds
           -> b             -- ^ Final result
fromDecode bs def f = either (const def) (f . lst) $ decodeOrFail' bs
  where
    lst (_,_,c) = c

-- | Try to run a 'Data.Binary.Get' monad. If decoding succeeds, apply a
-- function to the result. Otherwise, return the default value.
fromRunGet :: Binary a
           => Get a         -- ^ The Get monad to run
           -> ByteString    -- ^ The bytestring to decode
           -> b             -- ^ Default value to return when decoding fails
           -> (a -> b)      -- ^ Function to apply when decoding succeeds
           -> b             -- ^ Final result
fromRunGet m bs def f = either (const def) (f . lst) $ runGetOrFail' m bs
  where
    lst (_,_,c) = c

-- | Decode a 'Data.Binary' value. A 'Right' value is returned with the result
-- upon success. Otherwise a 'Left' value with the error message is returned.
decodeToEither :: Binary a => ByteString -> Either String a
decodeToEither bs = case decodeOrFail' bs of
    Left  (_,_,err) -> Left err
    Right (_,_,res) -> Right res

-- | Decode a 'Data.Binary' value. A 'Just' value is returned with the result
-- upon success. Otherwise, 'Nothing' is returned.
decodeToMaybe :: Binary a => ByteString -> Maybe a
decodeToMaybe bs = fromDecode bs Nothing Just

-- | Isolate a 'Data.Binary.Get' monad for the next 'Int' bytes. Only the next
-- 'Int' bytes of the input 'ByteString' will be available for the 'Get' monad
-- to consume. This function will fail if the Get monad fails or some of the
-- input is not consumed.
isolate :: Binary a => Int -> Get a -> Get a
isolate i g = do
    bs <- getByteString i
    case runGetOrFail' g bs of
        Left (_, _, err) -> fail err
        Right (unconsumed, _, res)
            | BS.null unconsumed -> return res
            | otherwise          -> fail "Isolate: unconsumed input"

-- Maybe and Either monad helpers

-- | Returns 'True' if the 'Either' value is 'Right'
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Returns 'True' if the 'Either' value is 'Left'
isLeft :: Either a b -> Bool
isLeft = not . isRight

-- | Extract the 'Right' value from an 'Either' value. Fails if the value is
-- 'Left'
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Either.fromRight: Left"

-- | Extract the 'Left' value from an 'Either' value. Fails if the value is 'Right'
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Either.fromLeft: Right"

-- | Transforms an 'Either' value into a 'Maybe' value. 'Right' is mapped to 'Just'
-- and 'Left' is mapped to 'Nothing'. The value inside 'Left' is lost.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

-- | Transforms a 'Maybe' value into an 'Either' value. 'Just' is mapped to
-- 'Right' and 'Nothing' is mapped to 'Left'. You also pass in an error value
-- in case 'Left' is returned.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err m = maybe (Left err) Right m

-- | Lift a 'Either' computation into the 'EitherT' monad
liftEither :: Monad m => Either b a -> EitherT b m a
liftEither = hoistEither

-- | Lift a 'Maybe' computation into the 'EitherT' monad
liftMaybe :: Monad m => b -> Maybe a -> EitherT b m a
liftMaybe err = liftEither . (maybeToEither err)

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
matchTemplate as (b:bs) f = case break (flip f b) as of
    (l,(r:rs)) -> (Just r) : matchTemplate (l ++ rs) bs f
    _          -> Nothing  : matchTemplate as bs f

-- | Returns the first value of a triple.
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- | Returns the second value of a triple.
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

-- | Returns the last value of a triple.
lst3 :: (a,b,c) -> c
lst3 (_,_,c) = c

-- | Strict evaluation of the new state
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = get >>= \x -> put $! f x

dropFieldLabel :: Int -> Options
dropFieldLabel n = defaultOptions
    { fieldLabelModifier = map toLower . drop n
    , omitNothingFields  = True
    }

dropSumLabels :: Int -> Int -> String -> Options
dropSumLabels c f tag = (dropFieldLabel f)
    { constructorTagModifier = map toLower . drop c
    , sumEncoding = defaultTaggedObject { tagFieldName = tag }
    }

