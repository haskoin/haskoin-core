module Network.Haskoin.Crypto.Bloom 
( BloomFilter(..)
, BloomFlags(..)
, bloomCreate
, bloomInsert
, bloomContains
, isBloomValid
, isBloomEmpty
, isBloomFull
) where

import Data.Word
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.ByteString as BS

import Network.Haskoin.Crypto.Hash 
import Network.Haskoin.Protocol

-- 20,000 items with fp rate < 0.1% or 10,000 items and <0.0001%
maxBloomSize :: Int
maxBloomSize = 36000

maxHashFuncs :: Word32
maxHashFuncs = 50

ln2Squared :: Double
ln2Squared = 0.4804530139182014246671025263266649717305529515945455

ln2 :: Double
ln2 = 0.6931471805599453094172321214581765680755001343602552

bitMask :: [Word8]
bitMask = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80]

-- | Build a bloom filter that will provide the given false positive rate when
-- the given number of elements have been inserted. 
bloomCreate :: Int          -- ^ Number of elements
            -> Double       -- ^ False positive rate
            -> Word32       
             -- ^ A random nonce (tweak) for the hash function. It should be
             -- a random number but the secureness of the random value is not
             -- of geat consequence.
            -> BloomFlags   -- ^ Bloom filter flags
            -> BloomFilter  -- ^ Bloom filter
bloomCreate numElem fpRate tweak flags =
    BloomFilter (S.replicate bloomSize 0) numHashF tweak flags
  where
    bloomSize = truncate $ (min a b) / 8
    a         = -1 / ln2Squared * (fromIntegral numElem) * log fpRate
    b         = fromIntegral $ maxBloomSize * 8
    numHashF  = truncate $ min c (fromIntegral maxHashFuncs)
    c         = (fromIntegral bloomSize) * 8 / (fromIntegral numElem) * ln2

bloomHash :: BloomFilter -> Word32 -> BS.ByteString -> Word32
bloomHash bfilter hashNum bs =
    murmurHash3 seed bs `mod` (fromIntegral (S.length (bloomData bfilter)) * 8)
  where
    seed = hashNum * 0xfba4c795 + (bloomTweak bfilter)

-- | Insert arbitrary data into a bloom filter. Returns the new bloom filter
-- containing the new data.
bloomInsert :: BloomFilter    -- ^ Original bloom filter
            -> BS.ByteString  -- ^ New data to insert
            -> BloomFilter    -- ^ Bloom filter containing the new data
bloomInsert bfilter bs 
    | isBloomFull bfilter = bfilter
    | otherwise = bfilter { bloomData = newData }
  where
    idxs    = map (\i -> bloomHash bfilter i bs) [0..bloomHashFuncs bfilter - 1]
    upd s i = S.adjust (.|. bitMask !! fromIntegral (7 .&. i))
                       (fromIntegral $ i `shiftR` 3) s
    newData = foldl upd (bloomData bfilter) idxs

-- | Tests if some arbitrary data matches the filter. This can be either because
-- the data was inserted into the filter or because it is a false positive.
bloomContains :: BloomFilter    -- ^ Bloom filter 
              -> BS.ByteString  
              -- ^ Data that will be checked against the given bloom filter
              -> Bool
              -- ^ Returns True if the data matches the filter
bloomContains bfilter bs
    | isBloomFull bfilter  = True
    | isBloomEmpty bfilter = False
    | otherwise            = and $ map isSet idxs
  where
    s       = bloomData bfilter
    idxs    = map (\i -> bloomHash bfilter i bs) [0..bloomHashFuncs bfilter - 1]
    isSet i = (S.index s (fromIntegral $ i `shiftR` 3))
          .&. (bitMask !! fromIntegral (7 .&. i)) /= 0

-- TODO: Write bloomRelevantUpdate
-- bloomRelevantUpdate :: BloomFilter -> Tx -> Hash256 -> Maybe BloomFilter

-- | Returns True if the filter is empty (all bytes set to 0x00)
isBloomEmpty :: BloomFilter -> Bool
isBloomEmpty bfilter = all (== 0x00) $ F.toList $ bloomData bfilter

-- | Returns True if the filter is full (all bytes set to 0xff)
isBloomFull :: BloomFilter -> Bool
isBloomFull bfilter = all (== 0xff) $ F.toList $ bloomData bfilter

-- | Tests if a given bloom filter is valid.
isBloomValid :: BloomFilter -- ^ Bloom filter to test
             -> Bool        -- ^ True if the given filter is valid
isBloomValid bfilter =
    (S.length $ bloomData bfilter) <= maxBloomSize &&
    (bloomHashFuncs bfilter) <= maxHashFuncs

