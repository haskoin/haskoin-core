module Bitcoin.Type.Hash
( Word128
, BigWord
) where

import Data.Word
import Data.Bits

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative

import qualified Bitcoin.Type as Bitcoin

type Word128 = BigWord Word64 Word64

data BigWord a b = BigWord !a !b
    deriving (Eq, Ord, Bounded)

instance (Ord a, Bits a, Integral a, Bounded a
         ,Ord b, Bits b, Integral b, Bounded b) 
         => Bits (BigWord a b) where

    (BigWord ah al) .&. (BigWord bh bl) = BigWord (ah .&. bh) (al .&. bl)

    (BigWord ah al) .|. (BigWord bh bl) = BigWord (ah .|. bh) (al .|. bl)

    xor (BigWord ah al) (BigWord bh bl) = BigWord (xor ah bh) (xor al bl)

    complement (BigWord h l) = BigWord (complement h) (complement l) 

    shiftL (BigWord h l) i = 
        let low  = l `shiftL` i
            high = h `shiftL` i
            size = bitSize l
            ovfl = if i <= size
                    then fromIntegral $ l `shiftR` (size - i)
                    else (fromIntegral l) `shiftL` (i - size)
            in BigWord (high .|. ovfl) low

    shiftR (BigWord h l) i = 
        let low  = l `shiftR` i
            high = h `shiftR` i
            size = bitSize l
            ovfl = if i <= size
                    then (fromIntegral h) `shiftL` (size - i)
                    else fromIntegral $ h `shiftR` (i - size)
            in BigWord high (low .|. ovfl)

    bitSize (BigWord h l) = bitSize h + bitSize l

    isSigned _ = False

instance (Ord a, Bits a, Integral a, Bounded a
         ,Ord b, Bits b, Integral b, Bounded b) 
         => Num (BigWord a b) where

    BigWord ah al + BigWord bh bl = BigWord hsum lsum
        where lsum = al + bl 
              carr = if lsum < al then 1 else 0
              hsum = ah + bh + carr

    BigWord ah al - BigWord bh bl = BigWord hsub lsub
        where lsub = al - bl
              carr = if lsub > al then 1 else 0
              hsub = ah - bh - carr

    x * y = go x y 0
        where go x y r
                | y == 0       = r
                | y .&. 1 == 1 = go (x `shiftL` 1) (y `shiftR` 1) (r+x)
                | otherwise    = go (x `shiftL` 1) (y `shiftR` 1) r

    negate = (0-)
    abs a = a
    signum a = if a > 0 then 1 else 0
    fromInteger i = r
        where r@(BigWord _ b) = BigWord 
                              (fromIntegral $ i `shiftR` (bitSize b)) 
                              (fromIntegral i)

instance (Bounded a, Bits a, Integral a, Bounded b, Bits b, Integral b)
         => Show (BigWord a b) where
        show = show . fromIntegral


