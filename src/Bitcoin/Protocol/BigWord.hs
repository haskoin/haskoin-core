module Bitcoin.Protocol.BigWord
( Word128
, Word160
, Word256
, BigWord(..)
) where

import Data.Word
import Data.Bits

type Word128 = BigWord Word64 Word64
type Word160 = BigWord Word32 Word128
type Word256 = BigWord Word128 Word128

data BigWord a b = BigWord { bwFirst :: !a, bwSecond :: !b }
    deriving (Eq, Ord, Bounded)

instance (Ord a, Bits a, Integral a, Bounded a
         ,Ord b, Bits b, Integral b, Bounded b) 
         => Bits (BigWord a b) where

    {-# SPECIALIZE instance Bits Word128 #-}
    {-# SPECIALIZE instance Bits Word160 #-}
    {-# SPECIALIZE instance Bits Word256 #-}

    (BigWord ah al) .&. (BigWord bh bl) = BigWord (ah .&. bh) (al .&. bl)

    (BigWord ah al) .|. (BigWord bh bl) = BigWord (ah .|. bh) (al .|. bl)

    xor (BigWord ah al) (BigWord bh bl) = BigWord (xor ah bh) (xor al bl)

    complement (BigWord h l) = BigWord (complement h) (complement l)

    shift bw i = fromIntegral (model `shift` i)
        where model = fromIntegral bw :: Integer

--    Keeping this for reference only
--
--    shiftL bw@(BigWord h l) i
--        | abs i >= bitSize bw = 0
--        | i >= 0 = 
--            let low  = l `shiftL` i
--                high = h `shiftL` i
--                size = bitSize l
--                ovfl = if i <= size
--                    then fromIntegral $ l `shiftR` (size - i)
--                    else (fromIntegral l) `shiftL` (i - size)
--                in BigWord (high .|. ovfl) low
--        | otherwise = shiftR bw (abs i)
--
--    shiftR bw@(BigWord h l) i 
--        | abs i >= bitSize bw = 0
--        | i >= 0 =
--            let low  = l `shiftR` i
--                high = h `shiftR` i
--                size = bitSize l
--                ovfl = if i <= size
--                        then (fromIntegral h) `shiftL` (size - i)
--                        else fromIntegral $ h `shiftR` (i - size)
--                in BigWord high (low .|. ovfl)
--        | otherwise = shiftL bw (abs i)

    bitSize (BigWord h l) = bitSize h + bitSize l

    isSigned _ = False

instance (Ord a, Bits a, Integral a, Bounded a, Num a
         ,Ord b, Bits b, Integral b, Bounded b, Num b) 
         => Num (BigWord a b) where

    {-# SPECIALIZE instance Num Word128 #-}
    {-# SPECIALIZE instance Num Word160 #-}
    {-# SPECIALIZE instance Num Word256 #-}

    (BigWord ah al) + (BigWord bh bl) = BigWord hsum lsum
        where lsum = al + bl 
              carr = if lsum < al then 1 else 0
              hsum = ah + bh + carr

    (BigWord ah al) - (BigWord bh bl) = BigWord hsub lsub
        where lsub = al - bl
              carr = if lsub > al then 1 else 0
              hsub = ah - bh - carr

    x * y = fromIntegral (mx * my)
        where mx = fromIntegral x :: Integer
              my = fromIntegral y :: Integer

--    x * y = go x y 0
--        where go x y r
--                | y == 0       = r
--                | y .&. 1 == 1 = go (x `shiftL` 1) (y `shiftR` 1) (r+x)
--                | otherwise    = go (x `shiftL` 1) (y `shiftR` 1) r

    negate = (0-)
    abs a = a
    signum a = if a > 0 then 1 else 0

    fromInteger i = BigWord first second
        where second = fromInteger i
              size   = bitSize second
              first  = fromInteger $ i `shiftR` size

instance ( Bounded a, Eq a, Num a, Enum a
         , Bounded b, Eq b, Num b, Enum b)
          => Enum (BigWord a b) where

    {-# SPECIALIZE instance Enum Word128 #-}
    {-# SPECIALIZE instance Enum Word160 #-}
    {-# SPECIALIZE instance Enum Word256 #-}

    toEnum i = BigWord 0 (toEnum i)
    fromEnum (BigWord _ l) = fromEnum l
    pred (BigWord h 0) = BigWord (pred h) maxBound
    pred (BigWord h l) = BigWord h (pred l)
    succ (BigWord h l) = if l == maxBound 
                             then BigWord (succ h) 0 
                             else BigWord h (succ l)

instance (Bits a, Real a, Bounded a, Integral a
         ,Bits b, Real b, Bounded b, Integral b) 
         => Real (BigWord a b) where

    {-# SPECIALIZE instance Real Word128 #-}
    {-# SPECIALIZE instance Real Word160 #-}
    {-# SPECIALIZE instance Real Word256 #-}

    toRational w = toRational (fromIntegral w :: Integer)

instance (Bounded a, Integral a, Bits a
         ,Bounded b, Integral b, Bits b) 
         => Integral (BigWord a b) where

    {-# SPECIALIZE instance Integral Word128 #-}
    {-# SPECIALIZE instance Integral Word160 #-}
    {-# SPECIALIZE instance Integral Word256 #-}

    toInteger (BigWord h l) = 
        (fromIntegral h `shiftL` bitSize l) + (fromIntegral l)

    quotRem a b = (fromIntegral q, fromIntegral r)
        where (q, r) = ma `quotRem` mb
              ma = fromIntegral a :: Integer
              mb = fromIntegral b :: Integer

-- Binary long division : Keeping for reference only
--
--    quotRem _ 0 = error "divide by zero"
--    quotRem a b = (q, r)
--        where r = a - q * b
--              q = go 0 (bitSize a) 0
--              go t 0 v = if v >= b then t + 1 else t
--              go t i v
--                  | v >= b    = go (setBit t i) (i-1) v2
--                  | otherwise = go t (i-1) v1
--                  where newBit = if (testBit a (i-1)) then 1 else 0
--                        v1 = (v `shiftL` 1) .|. newBit
--                        v2 = ((v-b) `shiftL` 1) .|. newBit
              
instance (Bounded a, Bits a, Integral a
         ,Bounded b, Bits b, Integral b)
         => Show (BigWord a b) where

    {-# SPECIALIZE instance Show Word128 #-}
    {-# SPECIALIZE instance Show Word160 #-}
    {-# SPECIALIZE instance Show Word256 #-}

    show = show . (fromIntegral :: Integral a => a -> Integer)

instance (Integral a, Num a, Bits a, Ord a, Bounded a
         ,Integral b, Num b, Bits b, Ord b, Bounded b)
         => Read (BigWord a b) where

    {-# SPECIALIZE instance Read Word128 #-}
    {-# SPECIALIZE instance Read Word160 #-}
    {-# SPECIALIZE instance Read Word256 #-}

    readsPrec i s = [(fromIntegral x, str) | (x,str) <- readsPrecI i s]
        where readsPrecI = readsPrec :: Int -> ReadS Integer


