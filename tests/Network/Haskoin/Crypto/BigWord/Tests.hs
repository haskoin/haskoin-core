module Network.Haskoin.Crypto.BigWord.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Bits 
    ( isSigned
    , bit
    , testBit
    , shift
    , bitSize
    , popCount
    , (.&.), (.|.)
    , xor, complement
    )
import Data.Word (Word8, Word32)
import qualified Data.ByteString as BS (length, index)

import Network.Haskoin.Crypto.Arbitrary 
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.NumberTheory
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Number Theory" 
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
        , testProperty "a * inv(a) = 1 (mod p) in FieldP" inverseModP
        , testProperty "a * inv(a) = 1 (mod n) in FieldN" inverseModN
        , testProperty "sqrt( a^2 ) = a (mod p)" sqrtP
        ],
      testGroup "BigWord Numeric"
        [ testProperty "BigWord fromInteger" ringFromInteger
        , testProperty "BigWord addition" ringAddition
        , testProperty "BigWord multiplication" ringMult
        , testProperty "BigWord negation" ringNegate
        , testProperty "BigWord abs" ringAbs
        , testProperty "BigWord signum" ringSignum
        ],
      testGroup "BigWord Bits"
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
        ],
      testGroup "BigWord Bounded"
        [ testProperty "BigWord minBound" ringMinBound
        , testProperty "BigWord maxBound" ringMaxBound
        ],
      testGroup "BigWord Enum"
        [ testProperty "BigWord succ" ringSucc
        , testProperty "BigWord pred" ringPred
        , testProperty "BigWord toEnum" ringToEnum
        , testProperty "BigWord fromEnum" ringFromEnum
        ],
      testGroup "BigWord Integral"
        [ testProperty "BigWord Quot" ringQuot
        , testProperty "BigWord Rem" ringRem
        , testProperty "BigWord Div" ringDiv
        , testProperty "BigWord Mod" ringMod
        , testProperty "BigWord QuotRem" ringQuotRem
        , testProperty "BigWord DivMod" ringDivMod
        , testProperty "BigWord toInteger" ringToInteger
        ],
      testGroup "BigWord Binary"
        [ testProperty "get( put(Word512) ) = Word512" getPutWord512
        , testProperty "get( put(Word256) ) = Word256" getPutWord256
        , testProperty "get( put(Word160) ) = Word160" getPutWord160
        , testProperty "get( put(Word128) ) = Word128" getPutWord128
        , testProperty "get( put(FieldP) ) = FieldP" getPutModP
        , testProperty "size( put(FieldP) ) = 32" putModPSize
        , testProperty "get( put(FieldN) ) = FieldN" getPutModN
        , testProperty "Verify DER of put(FieldN)" putModNSize
        ],
      testGroup "BigWord Read Show"
        [ testProperty "read( show(Word512) ) = Word512" readShowWord512
        , testProperty "read( show(Word256) ) = Word256" readShowWord256
        , testProperty "read( show(Word160) ) = Word160" readShowWord160
        , testProperty "read( show(Word128) ) = Word128" readShowWord128
        , testProperty "read( show(FieldP) ) = FieldP" readShowModP
        , testProperty "read( show(FieldN) ) = FieldN" readShowModN
        ]
    ]

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
    model = bitSize ((fromInteger i) :: Word32) 
    ring  = bitSize ((fromInteger i) :: Test32) 

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

{- BigWord Bounded -}

ringMinBound :: Test32 -> Bool
ringMinBound _ = (minBound :: Test32) - 1 == (maxBound :: Test32)

ringMaxBound :: Test32 -> Bool
ringMaxBound _ = (maxBound :: Test32) + 1 == (minBound :: Test32)

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

ringToEnum :: Word32 -> Bool
ringToEnum w = getBigWordInteger ring == fromIntegral model
  where 
    model = toEnum (fromIntegral w) :: Word32
    ring  = toEnum (fromIntegral w) :: Test32

ringFromEnum :: Integer -> Bool
ringFromEnum i = model == ring
  where 
    model = fromEnum ((fromInteger i) :: Word32)
    ring  = fromEnum ((fromInteger i) :: Test32)

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

{- BigWord Binary -}

getPutWord512 :: Word512 -> Bool
getPutWord512 r = r == (decode' $ encode' r)

getPutWord256 :: Word256 -> Bool
getPutWord256 r = r == (decode' $ encode' r)

getPutWord160 :: Word160 -> Bool
getPutWord160 r = r == (decode' $ encode' r)

getPutWord128 :: Word128 -> Bool
getPutWord128 r = r == (decode' $ encode' r)

getPutModP :: FieldP -> Bool
getPutModP r = r == (decode' $ encode' r)

putModPSize :: FieldP -> Bool
putModPSize r = BS.length (encode' r) == 32

getPutModN :: FieldN -> Property
getPutModN r = r > 0 ==> r == (decode' $ encode' r)

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

{- BigWord Read Show -}

readShowWord512 :: Word512 -> Bool
readShowWord512 r = r == (read $ show r)

readShowWord256 :: Word256 -> Bool
readShowWord256 r = r == (read $ show r)

readShowWord160 :: Word160 -> Bool
readShowWord160 r = r == (read $ show r)

readShowWord128 :: Word128 -> Bool
readShowWord128 r = r == (read $ show r)

readShowModP :: FieldP -> Bool
readShowModP r = r == (read $ show r)

readShowModN :: FieldN -> Property
readShowModN r = r > 0 ==> r == (read $ show r)
