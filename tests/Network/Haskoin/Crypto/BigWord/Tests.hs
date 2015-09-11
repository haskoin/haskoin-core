module Network.Haskoin.Crypto.BigWord.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Int (Int32)
import Data.Word (Word8, Word32)
import qualified Data.ByteString as BS (length, index)
import Data.Bits
    ( isSigned
    , bit
    , testBit
    , shift
    , bitSizeMaybe
    , popCount
    , rotate
    , (.&.), (.|.)
    , xor, complement
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Internals
    ( BigWordMod(..)
    , BigWord(..)
    , mulInverse
    , quadraticResidue
    , curveP
    )

tests :: [Test]
tests =
    [ testGroup "Number Theory"
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
        , testProperty "a * inv(a) = 1 (mod p) in FieldP" inverseModP
        , testProperty "a * inv(a) = 1 (mod n) in FieldN" inverseModN
        , testProperty "sqrt( a^2 ) = a (mod p)" sqrtP
        ]
    , testGroup "BigWord Numeric"
        [ testProperty "BigWord fromInteger" ringFromInteger
        , testProperty "BigWord addition" ringAddition
        , testProperty "BigWord multiplication" ringMult
        , testProperty "BigWord negation" ringNegate
        , testProperty "BigWord abs" ringAbs
        , testProperty "BigWord signum" ringSignum
        ]
    , testGroup "BigWord Bits"
        [ testProperty "BigWord AND" ringAnd
        , testProperty "BigWord OR" ringOr
        , testProperty "BigWord XOR" ringXor
        , testProperty "BigWord Complement" ringComplement
        , testProperty "BigWord Shift" ringShift
        , testProperty "BigWord Bitsize" ringBitsize
        , testProperty "BigWord Testbit" ringTestbit
        , testProperty "BigWord Bit" ringBit
        , testProperty "BigWord PopCount" ringPopCount
        , testProperty "BigWord IsSigned" ringIsSigned
        , testProperty "BigWord Rotation" ringRotate
        ]
    , testGroup "BigWord Bounded"
        [ testProperty "BigWord minBound" ringMinBound
        , testProperty "BigWord maxBound" ringMaxBound
        ]
    , testGroup "BigWord Enum"
        [ testProperty "BigWord succ" ringSucc
        , testProperty "BigWord pred" ringPred
        , testProperty "BigWord toEnum" ringToEnum
        , testProperty "BigWord fromEnum" ringFromEnum
        ]
    , testGroup "BigWord Integral"
        [ testProperty "BigWord Quot" ringQuot
        , testProperty "BigWord Rem" ringRem
        , testProperty "BigWord Div" ringDiv
        , testProperty "BigWord Mod" ringMod
        , testProperty "BigWord QuotRem" ringQuotRem
        , testProperty "BigWord DivMod" ringDivMod
        , testProperty "BigWord toInteger" ringToInteger
        ]
    , testGroup "BigWord binary representation"
        [ testProperty "size (put FieldP) = 32" putModPSize
        , testProperty "Verify DER of (put FieldN)" putModNSize
        ]
    ]

-- Define Test32 as BigWord Mod32 to compare it against Word32
data Mod32
type Test32 = BigWord Mod32

instance BigWordMod Mod32 where
    rFromInteger i = BigWord $ i `mod` 2 ^ (32 :: Integer)
    rBitSize     _ = 32

{- Number Theory -}

inverseMod :: Integer -> Property
inverseMod i = p > 0 ==> (p * (mulInverse p curveP)) `mod` curveP == 1
  where
    p = abs i

inverseModP :: FieldP -> Property
inverseModP r = r > 0 ==> r/r == 1

inverseModN :: FieldN -> Property
inverseModN r = r > 0 ==> r/r == 1

sqrtP :: FieldP -> Bool
sqrtP x = (a == x && b == (-x)) || (a == (-x) && b == x)
  where
    (a:b:_) = quadraticResidue (x^(2 :: Int))

{- BigWord Numeric -}

ringFromInteger :: Integer -> Bool
ringFromInteger i = getBigWordInteger ring == fromIntegral model
  where
    model = fromInteger i :: Word32
    ring  = fromInteger i :: Test32

ringAddition :: Integer -> Integer -> Bool
ringAddition i1 i2 = getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) + (fromInteger i2) :: Word32
    ring  = (fromInteger i1) + (fromInteger i2) :: Test32

ringMult :: Integer -> Integer -> Bool
ringMult i1 i2 = getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) * (fromInteger i2) :: Word32
    ring  = (fromInteger i1) * (fromInteger i2) :: Test32

ringNegate :: Integer -> Bool
ringNegate i = getBigWordInteger ring == fromIntegral model
  where
    model = negate (fromInteger i) :: Word32
    ring  = negate (fromInteger i) :: Test32

ringAbs :: Integer -> Bool
ringAbs i = getBigWordInteger ring == fromIntegral model
  where
    model = abs (fromInteger i) :: Word32
    ring  = abs (fromInteger i) :: Test32

ringSignum :: Integer -> Bool
ringSignum i = getBigWordInteger ring == fromIntegral model
  where
    model = signum (fromInteger i) :: Word32
    ring  = signum (fromInteger i) :: Test32

{- BigWord Bits -}

ringAnd :: Integer -> Integer -> Bool
ringAnd i1 i2 = getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) .&. (fromInteger i2) :: Word32
    ring  = (fromInteger i1) .&. (fromInteger i2) :: Test32

ringOr :: Integer -> Integer -> Bool
ringOr i1 i2 = getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) .|. (fromInteger i2) :: Word32
    ring  = (fromInteger i1) .|. (fromInteger i2) :: Test32

ringXor :: Integer -> Integer -> Bool
ringXor i1 i2 = getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) `xor` (fromInteger i2) :: Word32
    ring  = (fromInteger i1) `xor` (fromInteger i2) :: Test32

ringComplement :: Integer -> Bool
ringComplement i = getBigWordInteger ring == fromIntegral model
  where
    model = complement (fromInteger i) :: Word32
    ring  = complement (fromInteger i) :: Test32

ringShift :: Integer -> Word8 -> Bool
ringShift i j = getBigWordInteger ring == fromIntegral model
  where
    model = shift (fromInteger i) (fromIntegral j) :: Word32
    ring  = shift (fromInteger i) (fromIntegral j) :: Test32

ringBitsize :: Integer -> Bool
ringBitsize i = ring == model
  where
    model = bitSizeMaybe ((fromInteger i) :: Word32)
    ring  = bitSizeMaybe ((fromInteger i) :: Test32)

ringTestbit :: Integer -> Word8 -> Bool
ringTestbit i j = ring == model
  where
    model = testBit ((fromInteger i) :: Word32) (fromIntegral j)
    ring  = testBit ((fromInteger i) :: Test32) (fromIntegral j)

ringBit :: Word8 -> Bool
ringBit i = getBigWordInteger ring == fromIntegral model
  where
    model = bit (fromIntegral i) :: Word32
    ring  = bit (fromIntegral i) :: Test32

ringPopCount :: Integer -> Bool
ringPopCount i = ring == model
  where
    model = popCount ((fromInteger i) :: Word32)
    ring  = popCount ((fromInteger i) :: Test32)

ringIsSigned :: Integer -> Bool
ringIsSigned i = ring == model
  where
    model = isSigned ((fromInteger i) :: Word32)
    ring  = isSigned ((fromInteger i) :: Test32)

ringRotate :: (Integer, Int) -> Bool
ringRotate (i, n) = getBigWordInteger ring == fromIntegral model
  where
    model = rotate ((fromInteger i) :: Word32) n
    ring  = rotate ((fromInteger i) :: Test32) n

{- BigWord Bounded -}

ringMinBound :: Bool
ringMinBound = (minBound :: Test32) - 1 == (maxBound :: Test32)

ringMaxBound :: Bool
ringMaxBound = (maxBound :: Test32) + 1 == (minBound :: Test32)

{- BigWord Enum -}

ringSucc :: Integer -> Property
ringSucc i = (fromIntegral i) /= maxB ==>
    getBigWordInteger ring == fromIntegral model
  where
    model = succ (fromInteger i) :: Word32
    ring  = succ (fromInteger i) :: Test32
    maxB   = maxBound :: Word32

ringPred :: Integer -> Property
ringPred i = (fromIntegral i) /= minB ==>
    getBigWordInteger ring == fromIntegral model
  where
    model = pred (fromInteger i) :: Word32
    ring  = pred (fromInteger i) :: Test32
    minB   = minBound :: Word32

ringToEnum :: Int32 -> Property
ringToEnum w = w >= 0 ==> getBigWordInteger ring == fromIntegral model
  where
    model = toEnum (fromIntegral w) :: Word32
    ring  = toEnum (fromIntegral w) :: Test32

ringFromEnum :: Int32 -> Property
ringFromEnum i = i >= 0 ==> model == ring
  where
    model = fromEnum (fromIntegral i :: Word32)
    ring  = fromEnum (fromIntegral i :: Test32)

{- BigWord Integral -}

ringQuot :: Integer -> Integer -> Property
ringQuot i1 i2 = i2 /= 0 ==> getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) `quot` (fromInteger i2) :: Word32
    ring  = (fromInteger i1) `quot` (fromInteger i2) :: Test32

ringRem :: Integer -> Integer -> Property
ringRem i1 i2 = i2 /= 0 ==> getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) `rem` (fromInteger i2) :: Word32
    ring  = (fromInteger i1) `rem` (fromInteger i2) :: Test32

ringDiv :: Integer -> Integer -> Property
ringDiv i1 i2 = i2 /= 0 ==> getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) `div` (fromInteger i2) :: Word32
    ring  = (fromInteger i1) `div` (fromInteger i2) :: Test32

ringMod :: Integer -> Integer -> Property
ringMod i1 i2 = i2 /= 0 ==> getBigWordInteger ring == fromIntegral model
  where
    model = (fromInteger i1) `mod` (fromInteger i2) :: Word32
    ring  = (fromInteger i1) `mod` (fromInteger i2) :: Test32

ringQuotRem :: Integer -> Integer -> Property
ringQuotRem i1 i2 = i2 /= 0 ==>
    (getBigWordInteger r1 == fromIntegral m1) &&
    (getBigWordInteger r2 == fromIntegral m2)
  where
    (m1,m2) = (fromInteger i1) `quotRem` (fromInteger i2) :: (Word32, Word32)
    (r1,r2) = (fromInteger i1) `quotRem` (fromInteger i2) :: (Test32, Test32)

ringDivMod :: Integer -> Integer -> Property
ringDivMod i1 i2 = i2 /= 0 ==>
    (getBigWordInteger r1 == fromIntegral m1) &&
    (getBigWordInteger r2 == fromIntegral m2)
  where
    (m1,m2) = (fromInteger i1) `divMod` (fromInteger i2) :: (Word32, Word32)
    (r1,r2) = (fromInteger i1) `divMod` (fromInteger i2) :: (Test32, Test32)

ringToInteger :: Test32 -> Bool
ringToInteger r@(BigWord i) = toInteger r == i

{- BigWord binary representation -}

putModPSize :: FieldP -> Bool
putModPSize r = BS.length (encode' r) == 32

putModNSize :: FieldN -> Property
putModNSize r = r > 0 ==>
    (  a == 0x02    -- DER type is Integer
    && b <= 33      -- Can't be bigger than 32 + 0x00 padding
    && l == fromIntegral (b + 2) -- Advertised length matches
    && c < 0x80     -- High byte is never 1
    )
  where
    bs = encode' r
    a  = BS.index bs 0
    b  = BS.index bs 1
    c  = BS.index bs 2
    l  = BS.length bs

