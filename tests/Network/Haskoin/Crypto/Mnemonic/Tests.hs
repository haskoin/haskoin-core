{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Mnemonic.Tests (tests) where

import Test.QuickCheck (Arbitrary, Property, arbitrary, choose, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Bits ((.&.), shiftR)
import Data.Binary (Binary)
import Data.Word (Word32)
import qualified Data.ByteString as BS 
    ( ByteString
    , empty
    , append
    , length
    , last
    )

import Network.Haskoin.Test
import Network.Haskoin.Crypto
import Network.Haskoin.Util 
import Network.Haskoin.Internals (fromMnemonic, getBits)


tests :: [Test]
tests = 
    [ testGroup "Encode mnemonic"
        [ testProperty "128-bit entropy -> 12 words" toMnemonic128
        , testProperty "160-bit entropy -> 18 words" toMnemonic160
        , testProperty "256-bit entropy -> 24 words" toMnemonic256
        , testProperty "512-bit entropy -> 48 words" toMnemonic512
        , testProperty "n-bit entropy -> m words" toMnemonicVar
        ]
    , testGroup "Encode/Decode Mnemonic"
        [ testProperty "128-bit entropy" fromToMnemonic128
        , testProperty "160-bit entropy" fromToMnemonic160
        , testProperty "256-bit entropy" fromToMnemonic256
        , testProperty "512-bit entropy" fromToMnemonic512
        , testProperty "n-bit entropy" fromToMnemonicVar
        ]
    , testGroup "Mnemonic to seed"
        [ testProperty "128-bit entropy" mnemonicToSeed128
        , testProperty "160-bit entropy" mnemonicToSeed160
        , testProperty "256-bit entropy" mnemonicToSeed256
        , testProperty "512-bit entropy" mnemonicToSeed512
        , testProperty "n-bit entropy" mnemonicToSeedVar
        ]
    , testGroup "Get bits from ByteString"
        [ testProperty "Byte count" getBitsByteCount
        , testProperty "End bits" getBitsEndBits
        ]
    ]

binWordsToBS :: Binary a => [a] -> BS.ByteString
binWordsToBS = foldr f BS.empty
  where
    f b a = a `BS.append` encode' b

{- Encode mnemonic -}

toMnemonic128 :: Word128 -> Bool
toMnemonic128 x = l == 12
  where
    bs = encode' x
    l = length . words . fromRight $ toMnemonic bs

toMnemonic160 :: Word160 -> Bool
toMnemonic160 x = l == 15
  where
    bs = encode' x
    l = length . words . fromRight $ toMnemonic bs

toMnemonic256 :: Word256 -> Bool
toMnemonic256 x = l == 24
  where
    bs = encode' x
    l = length . words . fromRight $ toMnemonic bs

toMnemonic512 :: Word512 -> Bool
toMnemonic512 x = l == 48
  where
    bs = encode' x
    l = length . words . fromRight $ toMnemonic bs

toMnemonicVar :: [Word32] -> Property
toMnemonicVar ls = not (length ls > 8) ==> l == wc
  where
    bs = binWordsToBS ls
    bl = BS.length bs
    cb = bl `div` 4
    wc = (cb + bl * 8) `div` 11
    l = length . words . fromRight $ toMnemonic bs

{- Encode/Decode -}

fromToMnemonic128 :: Word128 -> Bool
fromToMnemonic128 x = bs == bs'
  where
    bs = encode' x
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic160 :: Word160 -> Bool
fromToMnemonic160 x = bs == bs'
  where
    bs = encode' x
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic256 :: Word256 -> Bool
fromToMnemonic256 x = bs == bs'
  where
    bs = encode' x
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic512 :: Word512 -> Bool
fromToMnemonic512 x = bs == bs'
  where
    bs = encode' x
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonicVar :: [Word32] -> Property
fromToMnemonicVar ls = not (length ls > 8) ==> bs == bs'
  where
    bs = binWordsToBS ls
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

{- Mnemonic to seed -}

mnemonicToSeed128 :: Word128 -> Bool
mnemonicToSeed128 x = l == 64
  where
    bs = encode' x
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed160 :: Word160 -> Bool
mnemonicToSeed160 x = l == 64
  where
    bs = encode' x
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed256 :: Word256 -> Bool
mnemonicToSeed256 x = l == 64
  where
    bs = encode' x
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed512 :: Word512 -> Bool
mnemonicToSeed512 x = l == 64
  where
    bs = encode' x
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeedVar :: [Word32] -> Property
mnemonicToSeedVar ls = not (length ls > 16) ==> l == 64
  where
    bs = binWordsToBS ls
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

{- Get bits from ByteString -}

data ByteCountGen = ByteCountGen BS.ByteString Int deriving Show

instance Arbitrary ByteCountGen where
    arbitrary = do
        ArbitraryByteString bs <- arbitrary
        i <- choose (0, BS.length bs * 8)
        return $ ByteCountGen bs i

getBitsByteCount :: ByteCountGen -> Bool
getBitsByteCount (ByteCountGen bs i) = BS.length bits == l
  where
    (q, r) = i `quotRem` 8
    bits = getBits i bs
    l = if r == 0 then q else q + 1

getBitsEndBits :: ByteCountGen -> Bool
getBitsEndBits (ByteCountGen bs i) = mask
  where
    r = i `mod` 8
    bits = getBits i bs
    mask = if r == 0 then True else BS.last bits .&. (0xff `shiftR` r) == 0x00

