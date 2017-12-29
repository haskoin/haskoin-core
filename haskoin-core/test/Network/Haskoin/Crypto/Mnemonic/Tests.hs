{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Mnemonic.Tests (tests) where

import           Data.Bits                            (shiftR, (.&.))
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as C
import           Data.Serialize                       (Serialize)
import           Data.Word                            (Word32, Word64)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Internals            (fromMnemonic, getBits)
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck                      (Arbitrary, Property,
                                                       arbitrary, choose, (==>))

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

binWordsToBS :: Serialize a => [a] -> BS.ByteString
binWordsToBS = foldr f BS.empty
  where
    f b a = a `BS.append` encodeStrict b

{- Encode mnemonic -}

toMnemonic128 :: (Word64, Word64) -> Bool
toMnemonic128 (a, b) = l == 12
  where
    bs = encodeStrict a `BS.append` encodeStrict b
    l = length . C.words . fromRight $ toMnemonic bs

toMnemonic160 :: (Word32, Word64, Word64) -> Bool
toMnemonic160 (a, b, c) = l == 15
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c]
    l = length . C.words . fromRight $ toMnemonic bs

toMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
toMnemonic256 (a, b, c, d) = l == 24
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d]
    l = length . C.words . fromRight $ toMnemonic bs

toMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
toMnemonic512 ((a, b, c, d), (e, f, g, h)) = l == 48
  where
    bs = BS.concat
        [ encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d
        , encodeStrict e, encodeStrict f, encodeStrict g, encodeStrict h
        ]
    l = length . C.words . fromRight $ toMnemonic bs

toMnemonicVar :: [Word32] -> Property
toMnemonicVar ls = length ls > 0 && length ls <= 8 ==> l == wc
  where
    bs = binWordsToBS ls
    bl = BS.length bs
    cb = bl `div` 4
    wc = (cb + bl * 8) `div` 11
    l = length . C.words . fromRight $ toMnemonic bs

{- Encode/Decode -}

fromToMnemonic128 :: (Word64, Word64) -> Bool
fromToMnemonic128 (a, b) = bs == bs'
  where
    bs = encodeStrict a `BS.append` encodeStrict b
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic160 :: (Word32, Word64, Word64) -> Bool
fromToMnemonic160 (a, b, c) = bs == bs'
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c]
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
fromToMnemonic256 (a, b, c, d) = bs == bs'
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d]
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
fromToMnemonic512 ((a, b, c, d), (e, f, g, h)) = bs == bs'
  where
    bs = BS.concat
        [ encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d
        , encodeStrict e, encodeStrict f, encodeStrict g, encodeStrict h
        ]
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

fromToMnemonicVar :: [Word32] -> Property
fromToMnemonicVar ls = length ls > 0 && length ls <= 8 ==> bs == bs'
  where
    bs = binWordsToBS ls
    bs' = fromRight (fromMnemonic =<< toMnemonic bs)

{- Mnemonic to seed -}

mnemonicToSeed128 :: (Word64, Word64) -> Bool
mnemonicToSeed128 (a, b) = l == 64
  where
    bs = encodeStrict a `BS.append` encodeStrict b
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed160 :: (Word32, Word64, Word64) -> Bool
mnemonicToSeed160 (a, b, c) = l == 64
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c]
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed256 :: (Word64, Word64, Word64, Word64) -> Bool
mnemonicToSeed256 (a, b, c, d) = l == 64
  where
    bs = BS.concat [encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d]
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
mnemonicToSeed512 ((a, b, c, d), (e, f, g, h)) = l == 64
  where
    bs = BS.concat
        [ encodeStrict a, encodeStrict b, encodeStrict c, encodeStrict d
        , encodeStrict e, encodeStrict f, encodeStrict g, encodeStrict h
        ]
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeedVar :: [Word32] -> Property
mnemonicToSeedVar ls = length ls > 0 && length ls <= 16 ==> l == 64
  where
    bs = binWordsToBS ls
    seed = fromRight (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

{- Get bits from ByteString -}

data ByteCountGen = ByteCountGen BS.ByteString Int deriving Show

instance Arbitrary ByteCountGen where
    arbitrary = do
        bs <- arbitraryBS
        i <- choose (0, BS.length bs * 8)
        return $ ByteCountGen bs i

getBitsByteCount :: ByteCountGen -> Bool
getBitsByteCount (ByteCountGen bs i) = BS.length bits == l
  where
    (q, r) = i `quotRem` 8
    bits = getBits i bs
    l = if r == 0 then q else q + 1

getBitsEndBits :: ByteCountGen -> Bool
getBitsEndBits (ByteCountGen bs i) =
    (r == 0) || (BS.last bits .&. (0xff `shiftR` r) == 0x00)
  where
    r = i `mod` 8
    bits = getBits i bs
    mask = r == 0 || BS.last bits .&. (0xff `shiftR` r) == 0x00

