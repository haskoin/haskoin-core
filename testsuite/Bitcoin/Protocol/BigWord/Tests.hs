module Bitcoin.Protocol.BigWord.Tests ( tests ) where

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( (.&.) )
import QuickCheckUtils

import Data.List
import Data.Bits
import Data.Word
import Data.Int
import Bitcoin.Protocol.BigWord

tests = 
    [ testGroup "BitWise AND Word128"  
        [ testProperty "vs Integer" (meta_and1 :: Word128 -> Word128 -> Bool)
        , testProperty "x AND Zero" (meta_and2 :: Word128 -> Bool)
        , testProperty "x AND x" (meta_and3 :: Word128 -> Bool)
        , testProperty "x AND maxBound" (meta_and4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise AND Word160"
        [ testProperty "vs Integer" (meta_and1 :: Word160 -> Word160 -> Bool)
        , testProperty "x AND Zero" (meta_and2 :: Word160 -> Bool)
        , testProperty "x AND x" (meta_and3 :: Word160 -> Bool)
        , testProperty "x AND maxBound" (meta_and4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise AND Word256"
        [ testProperty "vs Integer" (meta_and1 :: Word256 -> Word256 -> Bool)
        , testProperty "x AND Zero" (meta_and2 :: Word256 -> Bool)
        , testProperty "x AND x" (meta_and3 :: Word256 -> Bool)
        , testProperty "x AND maxBound" (meta_and4 :: Word256 -> Bool)
        ]
    , testGroup "BitWise OR Word128"  
        [ testProperty "vs Integer" (meta_or1 :: Word128 -> Word128 -> Bool)
        , testProperty "x OR Zero" (meta_or2 :: Word128 -> Bool)
        , testProperty "x OR x" (meta_or3 :: Word128 -> Bool)
        , testProperty "x OR maxBound" (meta_or4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise OR Word160"
        [ testProperty "vs Integer" (meta_or1 :: Word160 -> Word160 -> Bool)
        , testProperty "x OR Zero" (meta_or2 :: Word160 -> Bool)
        , testProperty "x OR x" (meta_or3 :: Word160 -> Bool)
        , testProperty "x OR maxBound" (meta_or4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise OR Word256"
        [ testProperty "vs Integer" (meta_or1 :: Word256 -> Word256 -> Bool)
        , testProperty "x OR Zero" (meta_or2 :: Word256 -> Bool)
        , testProperty "x OR x" (meta_or3 :: Word256 -> Bool)
        , testProperty "x OR maxBound" (meta_or4 :: Word256 -> Bool)
        ]
    , testGroup "BitWise XOR Word128"  
        [ testProperty "vs Integer" (meta_xor1 :: Word128 -> Word128 -> Bool)
        , testProperty "x XOR Zero" (meta_xor2 :: Word128 -> Bool)
        , testProperty "x XOR x" (meta_xor3 :: Word128 -> Bool)
        , testProperty "x XOR maxBound" (meta_xor4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise XOR Word160"
        [ testProperty "vs Integer" (meta_xor1 :: Word160 -> Word160 -> Bool)
        , testProperty "x XOR Zero" (meta_xor2 :: Word160 -> Bool)
        , testProperty "x XOR x" (meta_xor3 :: Word160 -> Bool)
        , testProperty "x XOR maxBound" (meta_xor4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise XOR Word256"
        [ testProperty "vs Integer" (meta_xor1 :: Word256 -> Word256 -> Bool)
        , testProperty "x XOR Zero" (meta_xor2 :: Word256 -> Bool)
        , testProperty "x XOR x" (meta_xor3 :: Word256 -> Bool)
        , testProperty "x XOR maxBound" (meta_xor4 :: Word256 -> Bool)
        ]
    , testGroup "Bitwise Complement"
        [ testProperty "Word128 Complement" (meta_comp :: Word128 -> Bool)
        , testProperty "Word160 Complement" (meta_comp :: Word160 -> Bool)
        , testProperty "Word256 Complement" (meta_comp :: Word256 -> Bool)
        ]
    , testGroup "Bitwise Left Shift Word128"
        [ testProperty "vs Integer" 
            (meta_shiftL1 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word128 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Left Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shiftL1 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word160 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Left Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shiftL1 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word256 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Shift Word128"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shift2 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word128 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shift2 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word160 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shift2 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word256 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Right Shift Word128"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word128 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word128 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Right Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word160 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word160 -> Int16 -> Bool)
        ]
    , testGroup "Bitwise Right Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word256 -> Int16 -> Bool)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word256 -> Int16 -> Bool)
        ]
    , testGroup "BitSize"
        [ testProperty "Word128 BitSize" bitSize128
        , testProperty "Word160 BitSize" bitSize160
        , testProperty "Word256 BitSize" bitSize256
        ]
    , testGroup "isSigned"
        [ testProperty "Word128 isSigned" (meta_isSigned :: Word128 -> Bool)
        , testProperty "Word160 isSigned" (meta_isSigned :: Word160 -> Bool)
        , testProperty "Word256 isSigned" (meta_isSigned :: Word256 -> Bool)
        ]
    , testGroup "numeric addition Word128"
        [ testProperty "vs Integer" 
            (meta_plus1 :: Word128 -> Word128 -> Bool)
        , testProperty "associativity" 
            (meta_plus2 :: Word128 -> Word128 -> Word128 -> Bool)
        , testProperty "commutativity" 
            (meta_plus3 :: Word128 -> Word128 -> Bool)
        , testProperty "additive identity" 
            (meta_plus4 :: Word128 -> Bool)
        , testProperty "distributivity" 
            (meta_plus5 :: Word128 -> Word128 -> Word128 -> Bool)
        , testProperty "power addition" 
            (meta_plus6 :: Word128 -> Word128 -> Word128 -> Bool)
        ]
    , testGroup "numeric addition Word160"
        [ testProperty "vs Integer" 
            (meta_plus1 :: Word160 -> Word160 -> Bool)
        , testProperty "associativity" 
            (meta_plus2 :: Word160 -> Word160 -> Word160 -> Bool)
        , testProperty "commutativity" 
            (meta_plus3 :: Word160 -> Word160 -> Bool)
        , testProperty "additive identity" 
            (meta_plus4 :: Word160 -> Bool)
        , testProperty "distributivity" 
            (meta_plus5 :: Word160 -> Word160 -> Word160 -> Bool)
        , testProperty "power addition" 
            (meta_plus6 :: Word160 -> Word160 -> Word160 -> Bool)
        ]
    , testGroup "numeric addition Word256"
        [ testProperty "vs Integer" 
            (meta_plus1 :: Word256 -> Word256 -> Bool)
        , testProperty "associativity" 
            (meta_plus2 :: Word256 -> Word256 -> Word256 -> Bool)
        , testProperty "commutativity" 
            (meta_plus3 :: Word256 -> Word256 -> Bool)
        , testProperty "additive identity" 
            (meta_plus4 :: Word256 -> Bool)
        , testProperty "distributivity" 
            (meta_plus5 :: Word256 -> Word256 -> Word256 -> Bool)
        , testProperty "power addition" 
            (meta_plus6 :: Word256 -> Word256 -> Word256 -> Bool)
        ]
    , testGroup "numeric substraction Word128"
        [ testProperty "vs Integer" 
            (meta_sub1 :: Word128 -> Word128 -> Bool)
        , testProperty "inverse terms" 
            (meta_sub2 :: Word128 -> Word128 -> Bool)
        , testProperty "subtraction identity" 
            (meta_sub3 :: Word128 -> Bool)
        , testProperty "distributivity" 
            (meta_sub4 :: Word128 -> Word128 -> Word128 -> Bool)
        ]
    , testGroup "numeric substraction Word160"
        [ testProperty "vs Integer" 
            (meta_sub1 :: Word160 -> Word160 -> Bool)
        , testProperty "inverse terms" 
            (meta_sub2 :: Word160 -> Word160 -> Bool)
        , testProperty "substraction identity" 
            (meta_sub3 :: Word160 -> Bool)
        , testProperty "distributivity" 
            (meta_sub4 :: Word160 -> Word160 -> Word160 -> Bool)
        ]
    , testGroup "numeric substraction Word256"
        [ testProperty "vs Integer" 
            (meta_sub1 :: Word256 -> Word256 -> Bool)
        , testProperty "inverse terms" 
            (meta_sub2 :: Word256 -> Word256 -> Bool)
        , testProperty "substraction identity" 
            (meta_sub3 :: Word256 -> Bool)
        , testProperty "distributivity" 
            (meta_sub4 :: Word256 -> Word256 -> Word256 -> Bool)
        ]
    ]

meta_and1 a b = fromIntegral (a .&. b) == (ma .&. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_and2 a = a .&. 0 == 0
meta_and3 a = a .&. a == a
meta_and4 a = a .&. maxBound == a

meta_or1 a b = fromIntegral (a .|. b) == (ma .|. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_or2 a = a .|. 0 == a
meta_or3 a = a .|. a == a
meta_or4 a = a .|. maxBound == maxBound

meta_xor1 a b = fromIntegral (a `xor` b) == (ma `xor` mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_xor2 a = a `xor` 0 == a
meta_xor3 a = a `xor` a == 0
meta_xor4 a = a `xor` maxBound == complement a

meta_comp a = complement (complement a) == a

-- compare with Integer shiftL
meta_shiftL1 a b = fromIntegral (a `shiftL` fromb) == model
    where model = (ma `shiftL` fromb) `mod` (2 ^ (bitSize a))
          ma    = fromIntegral a :: Integer
          fromb = fromIntegral b :: Int

-- powers of 2
meta_shiftL2 a b = (a `shiftL` fromb) == result
    where result | abs fromb >= bitSize a = 0
                 | b >= 0             = a * 2 ^ fromb
                 | b < 0              = a `quot` (2 ^ (abs fromb))
          fromb                       = fromIntegral b
    
-- Integer powers of 2
meta_shiftL3 a b = 
    fromIntegral (a `shiftL` fromb) == model `mod` (2 ^ (bitSize a))
        where model | b >= 0 = ma * 2 ^ fromb
                    | b < 0  = ma `quot` (2 ^ (abs fromb))
              ma             = fromIntegral a :: Integer
              fromb          = fromIntegral b :: Int

-- compare with Integer shift
meta_shift1 a b = fromIntegral (a `shift` fromb) == model
    where model = (ma `shift` fromb) `mod` (2 ^ (bitSize a))
          ma    = fromIntegral a :: Integer
          fromb = fromIntegral b :: Int

-- powers of 2
meta_shift2 a b = (a `shift` fromb) == result
    where result | abs fromb >= bitSize a = 0
                 | b >= 0             = a * 2 ^ fromb
                 | b < 0              = a `quot` (2 ^ (abs fromb))
          fromb                       = fromIntegral b
    
-- Integer powers of 2
meta_shift3 a b = 
    fromIntegral (a `shift` fromb) == model `mod` (2 ^ (bitSize a))
        where model | b >= 0 = ma * 2 ^ fromb
                    | b < 0  = ma `quot` (2 ^ (abs fromb))
              ma             = fromIntegral a :: Integer
              fromb          = fromIntegral b :: Int

-- compare with Integer shiftR
meta_shiftR1 a b = fromIntegral (a `shiftR` fromb) == model 
    where model = (ma `shiftR` fromb) `mod` (2 ^ (bitSize a))
          ma    = fromIntegral a :: Integer
          fromb = fromIntegral b :: Int

-- Powers of 2
meta_shiftR2 a b = (a `shiftR` fromb) == result
    where result | abs fromb >= bitSize a = 0 
                 | b >= 0             = a `quot` (2 ^ fromb)
                 | b < 0              = a * 2 ^ (abs fromb)
          fromb                       = fromIntegral b
    
meta_shiftR3 a b = 
    fromIntegral (a `shiftR` fromb) == model `mod` (2 ^ (bitSize a))
        where model | b >= 0 = ma `quot` (2 ^ fromb)
                    | b < 0  = ma * 2 ^ (abs fromb)
              ma             = fromIntegral a :: Integer
              fromb          = fromIntegral b :: Int

bitSize128 :: Word128 -> Bool
bitSize128 a = bitSize a == 128

bitSize160 :: Word160 -> Bool
bitSize160 a = bitSize a == 160

bitSize256 :: Word256 -> Bool
bitSize256 a = bitSize a == 256

meta_isSigned a = isSigned a == False

meta_plus1 a b = fromIntegral (a + b) == (ma + mb) `mod` (2 ^ (bitSize a))
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

meta_plus2 a b c = (a + b) + c == a + (b + c)

meta_plus3 a b = a + b == b + a

meta_plus4 a = a + 0 == a

meta_plus5 a b c = a * (b + c) == (a * b) + (a * c)

meta_plus6 a b c = a ^ (b + c) == (a ^ b) * (a ^ c)

meta_sub1 a b = fromIntegral (a - b) == (ma - mb) `mod` (2 ^ (bitSize a))
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

meta_sub2 a b = a - b == (-b) + a

meta_sub3 a = a - 0 == a

meta_sub4 a b c = a * (b - c) == (a * b) - (a * c)

meta_mul1 a b = fromIntegral (a * b) == (ma * mb) `mod` (2 ^ (bitSize a))
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

meta_mul2 a b c = (a * b) * c == a * (b * c)

meta_mul3 a b = a * b == b * a

meta_mul4 a = a * 1 == a

meta_mul5 a b c = a ^ (b * c) == (a ^ b) ^ c

