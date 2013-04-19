import Test.QuickCheck hiding ( (.&.) )
import QuickCheckUtils

import Data.Bits
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.Addr

{- BigWord properties -}

-- Test against Integer implementation
prop_bitand_1 :: (Bits a, Integral a, Bounded a) => a -> a -> Bool
prop_bitand_1 a b = fromIntegral (a .&. b) == (ma .&. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

prop_bitand_2 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitand_2 a = a .&. 0 == 0

prop_bitand_3 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitand_3 a = a .&. a == a

prop_bitand_4 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitand_4 a = a .&. maxBound == a

prop_bitor_1 :: (Bits a, Integral a, Bounded a) => a -> a -> Bool
prop_bitor_1 a b = fromIntegral (a .|. b) == (ma .|. mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

prop_bitor_2 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitor_2 a = a .|. 0 == a

prop_bitor_3 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitor_3 a = a .|. a == a

prop_bitor_4 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitor_4 a = a .|. maxBound == maxBound

prop_bitxor_1 :: (Bits a, Integral a) => a -> a -> Bool
prop_bitxor_1 a b = fromIntegral (a `xor` b) == (ma `xor` mb)
    where ma = fromIntegral a :: Integer
          mb = fromIntegral b :: Integer

prop_bitxor_2 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitxor_2 a = a `xor` 0 == a

prop_bitxor_3 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitxor_3 a = a `xor` a == 0

prop_bitxor_4 :: (Bits a, Integral a, Bounded a) => a -> Bool
prop_bitxor_4 a = a `xor` maxBound == complement a

prop_bitcomp :: (Bits a, Integral a) => a -> Bool
prop_bitcomp a = complement (complement a) == a

prop_identity :: BitcoinProtocol a => a -> Bool
prop_identity t = runGet bitcoinGet (runPut . bitcoinPut $ t) == t

