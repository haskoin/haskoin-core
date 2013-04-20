module Bitcoin.Protocol.BigWord.Tests ( tests ) where

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( (.&.) )
import QuickCheckUtils

import Data.List
import Data.Bits
import Bitcoin.Protocol.BigWord

tests = 
    [ testGroup "BitWise AND Word128"  
        [ testProperty "vs Integer" (meta_bitand1 :: Word128 -> Word128 -> Bool)
        , testProperty "x AND Zero" (meta_bitand2 :: Word128 -> Bool)
        , testProperty "x AND x" (meta_bitand3 :: Word128 -> Bool)
        , testProperty "x AND maxBound" (meta_bitand4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise AND Word160"
        [ testProperty "vs Integer" (meta_bitand1 :: Word160 -> Word160 -> Bool)
        , testProperty "x AND Zero" (meta_bitand2 :: Word160 -> Bool)
        , testProperty "x AND x" (meta_bitand3 :: Word160 -> Bool)
        , testProperty "x AND maxBound" (meta_bitand4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise AND Word256"
        [ testProperty "vs Integer" (meta_bitand1 :: Word256 -> Word256 -> Bool)
        , testProperty "x AND Zero" (meta_bitand2 :: Word256 -> Bool)
        , testProperty "x AND x" (meta_bitand3 :: Word256 -> Bool)
        , testProperty "x AND maxBound" (meta_bitand4 :: Word256 -> Bool)
        ]
    , testGroup "BitWise OR Word128"  
        [ testProperty "vs Integer" (meta_bitor1 :: Word128 -> Word128 -> Bool)
        , testProperty "x OR Zero" (meta_bitor2 :: Word128 -> Bool)
        , testProperty "x OR x" (meta_bitor3 :: Word128 -> Bool)
        , testProperty "x OR maxBound" (meta_bitor4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise OR Word160"
        [ testProperty "vs Integer" (meta_bitor1 :: Word160 -> Word160 -> Bool)
        , testProperty "x OR Zero" (meta_bitor2 :: Word160 -> Bool)
        , testProperty "x OR x" (meta_bitor3 :: Word160 -> Bool)
        , testProperty "x OR maxBound" (meta_bitor4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise OR Word256"
        [ testProperty "vs Integer" (meta_bitor1 :: Word256 -> Word256 -> Bool)
        , testProperty "x OR Zero" (meta_bitor2 :: Word256 -> Bool)
        , testProperty "x OR x" (meta_bitor3 :: Word256 -> Bool)
        , testProperty "x OR maxBound" (meta_bitor4 :: Word256 -> Bool)
        ]
    , testGroup "BitWise XOR Word128"  
        [ testProperty "vs Integer" (meta_bitxor1 :: Word128 -> Word128 -> Bool)
        , testProperty "x XOR Zero" (meta_bitxor2 :: Word128 -> Bool)
        , testProperty "x XOR x" (meta_bitxor3 :: Word128 -> Bool)
        , testProperty "x XOR maxBound" (meta_bitxor4 :: Word128 -> Bool)
        ]
    , testGroup "BitWise XOR Word160"
        [ testProperty "vs Integer" (meta_bitxor1 :: Word160 -> Word160 -> Bool)
        , testProperty "x XOR Zero" (meta_bitxor2 :: Word160 -> Bool)
        , testProperty "x XOR x" (meta_bitxor3 :: Word160 -> Bool)
        , testProperty "x XOR maxBound" (meta_bitxor4 :: Word160 -> Bool)
        ]
    , testGroup "BitWise XOR Word256"
        [ testProperty "vs Integer" (meta_bitxor1 :: Word256 -> Word256 -> Bool)
        , testProperty "x XOR Zero" (meta_bitxor2 :: Word256 -> Bool)
        , testProperty "x XOR x" (meta_bitxor3 :: Word256 -> Bool)
        , testProperty "x XOR maxBound" (meta_bitxor4 :: Word256 -> Bool)
        ]
    , testGroup "Bitwise Complement"
        [ testProperty "Word128 Complement" (meta_bitcomp :: Word128 -> Bool)
        , testProperty "Word160 Complement" (meta_bitcomp :: Word160 -> Bool)
        , testProperty "Word256 Complement" (meta_bitcomp :: Word256 -> Bool)
        ]
    ]

meta_bitand1 a b = fromIntegral (a .&. b) == (ma .&. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_bitand2 a = a .&. 0 == 0
meta_bitand3 a = a .&. a == a
meta_bitand4 a = a .&. maxBound == a

meta_bitor1 a b = fromIntegral (a .|. b) == (ma .|. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_bitor2 a = a .|. 0 == a
meta_bitor3 a = a .|. a == a
meta_bitor4 a = a .|. maxBound == maxBound

meta_bitxor1 a b = fromIntegral (a `xor` b) == (ma `xor` mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer
meta_bitxor2 a = a `xor` 0 == a
meta_bitxor3 a = a `xor` a == 0
meta_bitxor4 a = a `xor` maxBound == complement a

meta_bitcomp a = complement (complement a) == a

