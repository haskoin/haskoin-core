module Network.Haskoin.Crypto.BigWord
(
-- Useful type aliases

-- Data types
  BigWord(..)
, BigWordMod(..)

, Mod31
, Word31

-- Functions
, bigWordParser
) where

{-
import Test.QuickCheck
    ( Arbitrary
    , arbitrary
    , arbitrarySizedBoundedIntegral
    )
-}
-- import Control.Monad (unless, guard, mzero, (<=<))
import Control.DeepSeq (NFData, rnf)
import Data.Bits (Bits(..), FiniteBits(..))

{- 

import Data.Binary (Binary, get, put)
import Data.Binary.Get
    ( getWord64be
    , getWord32be
    , getWord8
    , getByteString
    , Get
    )
import Data.Binary.Put
    ( putWord64be
    , putWord32be
    , putWord8
    , putByteString
    )
import Data.Aeson
    ( Value (String)
    , FromJSON
    , ToJSON
    , parseJSON
    , toJSON
    , withText
    )
-}
-- import Data.Ratio (numerator, denominator)
-- import qualified Data.ByteString as BS (head, length, reverse)
-- import qualified Data.Text as T (pack, unpack)

-- import Network.Haskoin.Crypto.Curve
-- import Network.Haskoin.Crypto.NumberTheory
-- import Control.Monad ((<=<))
import Network.Haskoin.Util
import Safe (toEnumMay)
import Data.String
import Text.Parsec 
import Text.Parsec.Language 
import Text.Parsec.Token


type Word31 = BigWord Mod31
data Mod31

newtype BigWord n = BigWord { getBigWordInteger :: Integer }
    deriving (Eq, Ord, Read, Show)

instance NFData (BigWord n) where
    rnf (BigWord n) = rnf n

class BigWordMod a where
    rFromInteger :: Integer -> BigWord a
    rBitSize     :: BigWord a -> Int

instance BigWordMod Mod31 where
    rFromInteger i = BigWord $ i `mod` 2 ^ (31 :: Int)
    rBitSize     _ = 31

instance BigWordMod n => Num (BigWord n) where
    fromInteger = rFromInteger
    (BigWord i1) + (BigWord i2) = fromInteger $ i1 + i2
    (BigWord i1) * (BigWord i2) = fromInteger $ i1 * i2
    negate (BigWord i) = fromInteger $ negate i
    abs r = r
    signum (BigWord i) = fromInteger $ signum i

instance BigWordMod n => Bits (BigWord n) where
    (BigWord i1) .&. (BigWord i2) = fromInteger $ i1 .&. i2
    (BigWord i1) .|. (BigWord i2) = fromInteger $ i1 .|. i2
    (BigWord i1) `xor` (BigWord i2) = fromInteger $ i1 `xor` i2
    complement (BigWord i) = fromInteger $ complement i
    bitSizeMaybe = Just . rBitSize
    bitSize = rBitSize
    shift (BigWord i) j = fromInteger $ shift i j
    testBit (BigWord i) = testBit i
    bit n = fromInteger $ bit n
    popCount (BigWord i) = popCount i
    isSigned _ = False
    rotate x i = shift x i' .|. shift x (i' - rBitSize x)
      where
        i' = i `mod` rBitSize x

instance BigWordMod n => FiniteBits (BigWord n) where
    finiteBitSize = rBitSize

instance BigWordMod n => Bounded (BigWord n) where
    minBound = fromInteger 0
    maxBound = fromInteger (-1)

instance BigWordMod n => Real (BigWord n) where
    toRational (BigWord i) = toRational i

instance BigWordMod n => Enum (BigWord n) where
    succ r@(BigWord i)
        | r == maxBound = error "BigWord: tried to take succ of maxBound"
        | otherwise = fromInteger $ succ i
    pred r@(BigWord i)
        | r == minBound = error "BigWord: tried to take pred of minBound"
        | otherwise = fromInteger $ pred i
    toEnum i
        | toInteger i >= (toInteger minW) && toInteger i <= (toInteger maxW) = r
        | otherwise = error "BigWord: toEnum is outside of bounds"
      where
        f i = if toInteger i >= (toInteger minW) && toInteger i <= (toInteger maxW)
                then Just r
                else Nothing
        minW = minFrom r
        maxW = maxFrom r
        r = fromInteger . toEnum $ i
        minFrom :: BigWordMod a => BigWord a -> BigWord a
        minFrom _ = minBound
        maxFrom :: BigWordMod a => BigWord a -> BigWord a
        maxFrom _ = maxBound
    fromEnum (BigWord i) = fromEnum i


instance BigWordMod n => Integral (BigWord n) where
    (BigWord i1) `quot` (BigWord i2) = fromInteger $ i1 `quot` i2
    (BigWord i1) `rem` (BigWord i2) = fromInteger $ i1 `rem` i2
    (BigWord i1) `div` (BigWord i2) = fromInteger $ i1 `div` i2
    (BigWord i1) `mod` (BigWord i2) = fromInteger $ i1 `mod` i2
    (BigWord i1) `quotRem` (BigWord i2) = (fromInteger a, fromInteger b)
      where
        (a,b) = i1 `quotRem` i2
    (BigWord i1) `divMod` (BigWord i2) = (fromInteger a, fromInteger b)
      where
        (a,b) = i1 `divMod` i2
    toInteger (BigWord i) = i


-- TODO: Test

instance ToHaskoinString (BigWord n) where
    toHaskoinString (BigWord i) = show i
instance (BigWordMod n) => FromHaskoinString (BigWord n) where
    fromHaskoinString = eitherToMaybe . parseBigWord -- toEnumMay <=< readMay
instance (BigWordMod n) => IsString (BigWord n) where
    fromString = derivFromString "Could not decode extended private key: " 

parseBigWord :: (BigWordMod n) => String -> Either ParseError (BigWord n)
parseBigWord x = parse bigWordParser "bigWordParser" x
bigWordParser :: (BigWordMod n) => Parsec String () (BigWord n)  
bigWordParser = do
  i <- integer . makeTokenParser $ haskellStyle 
  case toEnumMay . fromIntegral $ i of
    Just w -> return w
    Nothing -> fail $ "parseBigWord, " ++ show i


