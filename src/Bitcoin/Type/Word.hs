module Bitcoin.Type.Word
( Word16be
, Word32be
, Word64be
, Word128be
, Word160be
, Word256be
) where

import Data.Word
import Data.Bits
import Control.Applicative

import Data.Binary.Get
import Data.Binary.Put

import qualified Bitcoin.Type as Bitcoin

type Word128be = BigWord Word64be Word64be
type Word160be = BigWord Word32be Word128be
type Word256be = BigWord Word128be Word128be

data BigWord a b = BigWord { bwFirst :: !a, bwSecond :: !b }
    deriving (Eq, Ord, Bounded)

instance (Ord a, Bits a, Integral a, Bounded a
         ,Ord b, Bits b, Integral b, Bounded b) 
         => Bits (BigWord a b) where

    {-# SPECIALIZE instance Bits Word128be #-}
    {-# SPECIALIZE instance Bits Word160be #-}
    {-# SPECIALIZE instance Bits Word256be #-}

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

instance (Ord a, Bits a, Integral a, Bounded a, Num a
         ,Ord b, Bits b, Integral b, Bounded b, Num b) 
         => Num (BigWord a b) where

    {-# SPECIALIZE instance Num Word128be #-}
    {-# SPECIALIZE instance Num Word160be #-}
    {-# SPECIALIZE instance Num Word256be #-}

    (BigWord ah al) + (BigWord bh bl) = BigWord hsum lsum
        where lsum = al + bl 
              carr = if lsum < al then 1 else 0
              hsum = ah + bh + carr

    (BigWord ah al) - (BigWord bh bl) = BigWord hsub lsub
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

    fromInteger i = BigWord first second
        where second = fromInteger i
              size   = bitSize second
              first  = fromInteger $ i `shiftR` size

instance ( Bounded a, Eq a, Num a, Enum a
         , Bounded b, Eq b, Num b, Enum b)
          => Enum (BigWord a b) where

    {-# SPECIALIZE instance Enum Word128be #-}
    {-# SPECIALIZE instance Enum Word160be #-}
    {-# SPECIALIZE instance Enum Word256be #-}

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

    {-# SPECIALIZE instance Real Word128be #-}
    {-# SPECIALIZE instance Real Word160be #-}
    {-# SPECIALIZE instance Real Word256be #-}

    toRational w = toRational (fromIntegral w :: Integer)

instance (Bounded a, Integral a, Bits a
         ,Bounded b, Integral b, Bits b) 
         => Integral (BigWord a b) where

    {-# SPECIALIZE instance Integral Word128be #-}
    {-# SPECIALIZE instance Integral Word160be #-}
    {-# SPECIALIZE instance Integral Word256be #-}

    toInteger (BigWord h l) = 
        (fromIntegral h `shiftL` bitSize l) + (fromIntegral l)

    -- Binary long division
    quotRem a b = (q, r)
        where r = a - q * b
              q = go 0 (bitSize a) 0
              go t 0 v = if v >= b then t + 1 else t
              go t i v
                  | v >= b    = go (setBit t i) (i-1) v2
                  | otherwise = go t (i-1) v1
                  where newBit = if (testBit a (i-1)) then 1 else 0
                        v1 = (v `shiftL` 1) .|. newBit
                        v2 = ((v-b) `shiftL` 1) .|. newBit
              
instance (Bounded a, Bits a, Integral a, Bounded b, Bits b, Integral b)
         => Show (BigWord a b) where

    {-# SPECIALIZE instance Show Word128be #-}
    {-# SPECIALIZE instance Show Word160be #-}
    {-# SPECIALIZE instance Show Word256be #-}

    show = show . fromIntegral

instance (Bitcoin.Type a, Bitcoin.Type b) 
         => Bitcoin.Type (BigWord a b) where

    {-# SPECIALIZE instance Bitcoin.Type Word128be #-}
    {-# SPECIALIZE instance Bitcoin.Type Word160be #-}
    {-# SPECIALIZE instance Bitcoin.Type Word256be #-}

    get = BigWord <$> Bitcoin.get <*> Bitcoin.get
    put (BigWord a b) = do
        Bitcoin.put a
        Bitcoin.put b

{-- Word16be --}

newtype Word16be = Word16be Word16
    deriving (Eq, Ord, Bounded)

instance Bits Word16be where
    (Word16be a) .&. (Word16be b) = Word16be (a .&. b)
    (Word16be a) .|. (Word16be b) = Word16be (a .|. b)
    (Word16be a) `xor` (Word16be b) = Word16be (a `xor` b)
    complement (Word16be a) = Word16be (complement a)
    (Word16be a) `shiftL` i = Word16be (a `shiftL` i)
    (Word16be a) `shiftR` i = Word16be (a `shiftR` i)
    bitSize (Word16be a) = bitSize a
    isSigned (Word16be a) = isSigned a

instance Num Word16be where
    (Word16be a) + (Word16be b) = Word16be (a + b)
    (Word16be a) - (Word16be b) = Word16be (a - b)
    (Word16be a) * (Word16be b) = Word16be (a * b)
    negate (Word16be a) = Word16be (negate a)
    abs (Word16be a) = Word16be (abs a)
    signum (Word16be a) = Word16be (signum a)
    fromInteger i = Word16be (fromInteger i)

instance Enum Word16be where
    toEnum i = Word16be (toEnum i)
    fromEnum (Word16be a) = fromEnum a
    pred (Word16be a) = Word16be (pred a)
    succ (Word16be a) = Word16be (succ a)

instance Real Word16be where
    toRational (Word16be a) = toRational a

instance Integral Word16be where
    toInteger (Word16be a) = toInteger a
    (Word16be a) `quotRem` (Word16be b) = go (a `quotRem` b)
        where go (x, y) = (Word16be x, Word16be y)

instance Show Word16be where
    show (Word16be a) = show a

instance Read Word16be where
    readsPrec i s = 
        let readsPrecW :: Int -> ReadS Word16
            readsPrecW = readsPrec
        in [(fromIntegral w, str) | (w,str) <- readsPrecW i s]

instance Bitcoin.Type Word16be where
    get = Word16be <$> getWord16be
    put (Word16be a) = putWord16be a

{-- Word32be --}

newtype Word32be = Word32be Word32
    deriving (Eq, Ord, Bounded)

instance Bits Word32be where
    (Word32be a) .&. (Word32be b) = Word32be (a .&. b)
    (Word32be a) .|. (Word32be b) = Word32be (a .|. b)
    (Word32be a) `xor` (Word32be b) = Word32be (a `xor` b)
    complement (Word32be a) = Word32be (complement a)
    (Word32be a) `shiftL` i = Word32be (a `shiftL` i)
    (Word32be a) `shiftR` i = Word32be (a `shiftR` i)
    bitSize (Word32be a) = bitSize a
    isSigned (Word32be a) = isSigned a

instance Num Word32be where
    (Word32be a) + (Word32be b) = Word32be (a + b)
    (Word32be a) - (Word32be b) = Word32be (a - b)
    (Word32be a) * (Word32be b) = Word32be (a * b)
    negate (Word32be a) = Word32be (negate a)
    abs (Word32be a) = Word32be (abs a)
    signum (Word32be a) = Word32be (signum a)
    fromInteger i = Word32be (fromInteger i)

instance Enum Word32be where
    toEnum i = Word32be (toEnum i)
    fromEnum (Word32be a) = fromEnum a
    pred (Word32be a) = Word32be (pred a)
    succ (Word32be a) = Word32be (succ a)

instance Real Word32be where
    toRational (Word32be a) = toRational a

instance Integral Word32be where
    toInteger (Word32be a) = toInteger a
    (Word32be a) `quotRem` (Word32be b) = go (a `quotRem` b)
        where go (x, y) = (Word32be x, Word32be y)

instance Show Word32be where
    show (Word32be a) = show a

instance Read Word32be where
    readsPrec i s = 
        let readsPrecW :: Int -> ReadS Word32
            readsPrecW = readsPrec
        in [(fromIntegral w, str) | (w,str) <- readsPrecW i s]

instance Bitcoin.Type Word32be where
    get = Word32be <$> getWord32be
    put (Word32be a) = putWord32be a

{-- Word64be --}

newtype Word64be = Word64be Word64
    deriving (Eq, Ord, Bounded)

instance Bits Word64be where
    (Word64be a) .&. (Word64be b) = Word64be (a .&. b)
    (Word64be a) .|. (Word64be b) = Word64be (a .|. b)
    (Word64be a) `xor` (Word64be b) = Word64be (a `xor` b)
    complement (Word64be a) = Word64be (complement a)
    (Word64be a) `shiftL` i = Word64be (a `shiftL` i)
    (Word64be a) `shiftR` i = Word64be (a `shiftR` i)
    bitSize (Word64be a) = bitSize a
    isSigned (Word64be a) = isSigned a

instance Num Word64be where
    (Word64be a) + (Word64be b) = Word64be (a + b)
    (Word64be a) - (Word64be b) = Word64be (a - b)
    (Word64be a) * (Word64be b) = Word64be (a * b)
    negate (Word64be a) = Word64be (negate a)
    abs (Word64be a) = Word64be (abs a)
    signum (Word64be a) = Word64be (signum a)
    fromInteger i = Word64be (fromInteger i)

instance Enum Word64be where
    toEnum i = Word64be (toEnum i)
    fromEnum (Word64be a) = fromEnum a
    pred (Word64be a) = Word64be (pred a)
    succ (Word64be a) = Word64be (succ a)

instance Real Word64be where
    toRational (Word64be a) = toRational a

instance Integral Word64be where
    toInteger (Word64be a) = toInteger a
    (Word64be a) `quotRem` (Word64be b) = go (a `quotRem` b)
        where go (x, y) = (Word64be x, Word64be y)

instance Show Word64be where
    show (Word64be a) = show a

instance Read Word64be where
    readsPrec i s = 
        let readsPrecW :: Int -> ReadS Word64
            readsPrecW = readsPrec
        in [(fromIntegral w, str) | (w,str) <- readsPrecW i s]

instance Bitcoin.Type Word64be where
    get = Word64be <$> getWord64be
    put (Word64be a) = putWord64be a

