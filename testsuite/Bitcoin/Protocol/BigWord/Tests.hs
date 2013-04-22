module Bitcoin.Protocol.BigWord.Tests ( tests ) where

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( (.&.) )
import QuickCheckUtils

import Data.List
import Data.Bits
import Data.Word
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
            (meta_shiftL1 :: Word128 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word128 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word128 -> Int -> Property)
        ]
    , testGroup "Bitwise Left Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shiftL1 :: Word160 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word160 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word160 -> Int -> Property)
        ]
    , testGroup "Bitwise Left Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shiftL1 :: Word256 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftL2 :: Word256 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftL3 :: Word256 -> Int -> Property)
        ]
    , testGroup "Bitwise Shift Word128"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word128 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shift2 :: Word128 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word128 -> Int -> Property)
        ]
    , testGroup "Bitwise Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word160 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shift2 :: Word160 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word160 -> Int -> Property)
        ]
    , testGroup "Bitwise Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shift1 :: Word256 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shift2 :: Word256 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shift3 :: Word256 -> Int -> Property)
        ]
    , testGroup "Bitwise Right Shift Word128"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word128 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word128 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word128 -> Int -> Property)
        ]
    , testGroup "Bitwise Right Shift Word160"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word160 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word160 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word160 -> Int -> Property)
        ]
    , testGroup "Bitwise Right Shift Word256"
        [ testProperty "vs Integer" 
            (meta_shiftR1 :: Word256 -> Int -> Property)
        , testProperty "shift as power" 
            (meta_shiftR2 :: Word256 -> Int -> Property)
        , testProperty "shift as Integer power" 
            (meta_shiftR3 :: Word256 -> Int -> Property)
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

meta_shiftL1 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shiftL` b) == model
        where model = (ma `shiftL` b) `mod` 2^(bitSize a)
              ma = fromIntegral a :: Integer

meta_shiftL2 a b = (abs b) <= (bitSize a) ==> (a `shiftL` b) == result
    where result | abs b == bitSize a = 0 
                 | b >= 0             = a * 2^b
                 | b < 0              = a `quot` 2^(abs b) 
    
meta_shiftL3 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shiftL` b) == model `mod` 2^(bitSize a)
        where model | abs b == bitSize a = 0
                    | b >= 0             = ma * 2^b
                    | b < 0              = ma `quot` 2^(abs b)
              ma = fromIntegral a :: Integer

meta_shift1 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shift` b) == model
        where model = (ma `shift` b) `mod` 2^(bitSize a)
              ma = fromIntegral a :: Integer

meta_shift2 a b = (abs b) <= (bitSize a) ==> (a `shift` b) == result
    where result | abs b == bitSize a = 0 
                 | b >= 0             = a * 2^b
                 | b < 0              = a `quot` 2^(abs b) 
    
meta_shift3 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shift` b) == model `mod` 2^(bitSize a)
        where model | abs b == bitSize a = 0
                    | b >= 0             = ma * 2^b
                    | b < 0              = ma `quot` 2^(abs b)
              ma = fromIntegral a :: Integer

meta_shiftR1 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shiftR` b) == model `mod` 2^(bitSize a)
        where model = (ma `shiftR` b) 
              ma = fromIntegral a :: Integer

meta_shiftR2 a b = (abs b) <= (bitSize a) ==> (a `shiftR` b) == result
    where result | abs b == bitSize a = 0 
                 | b >= 0             = a `quot` 2^b
                 | b < 0              = a * 2^(abs b)
    
meta_shiftR3 a b = (abs b) <= (bitSize a) ==> 
    fromIntegral (a `shiftR` b) == model `mod` 2^(bitSize a)
        where model | abs b == bitSize a = 0
                    | b >= 0             = ma `quot` 2^b
                    | b < 0              = ma * 2^(abs b)
              ma = fromIntegral a :: Integer

bitSize128 :: Word128 -> Bool
bitSize128 a = bitSize a == 128

bitSize160 :: Word160 -> Bool
bitSize160 a = bitSize a == 160

bitSize256 :: Word256 -> Bool
bitSize256 a = bitSize a == 256

meta_isSigned a = isSigned a == False

