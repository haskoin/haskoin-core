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
    , testGroup "Bitwise Left Shift"
        [ testProperty "Word128 shiftL" 
            (meta_shiftL1 :: Word128 -> Int -> Property)
        , testProperty "Word160 shiftL" 
            (meta_shiftL1 :: Word160 -> Int -> Property)
        , testProperty "Word256 shiftL" 
            (meta_shiftL1 :: Word256 -> Int -> Property)
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

meta_shiftL1 a b = (abs b) <= 1024 ==> fromIntegral (a `shiftL` b) == model
    where model = (ma `shiftL` b) `mod` 2^(bitSize a)
          ma = fromIntegral a :: Integer


