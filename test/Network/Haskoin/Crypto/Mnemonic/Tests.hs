{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Mnemonic.Tests (spec) where

import           Data.Bits              (shiftR, (.&.))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C
import           Data.Either            (fromRight)
import           Data.Serialize         (Serialize, encode)
import           Data.Word              (Word32, Word64)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Hspec
import           Test.QuickCheck        hiding ((.&.))

spec :: Spec
spec = do
    describe "mnemonic" $ do
        it "generate 12 words" $ property toMnemonic128
        it "generate 18 words" $ property toMnemonic160
        it "generate 24 words" $ property toMnemonic256
        it "generate 48 words" $ property toMnemonic512
        it "generate any number of words" $ property toMnemonicVar
        it "encode and decode 128-bit entropy" $ property fromToMnemonic128
        it "encode and decode 160-bit entropy" $ property fromToMnemonic160
        it "encode and decode 256-bit entropy" $ property fromToMnemonic256
        it "encode and decode 512-bit entropy" $ property fromToMnemonic512
        it "encode and decode n-bit entropy" $ property fromToMnemonicVar
        it "convert 128-bit mnemonic to seed" $ property mnemonicToSeed128
        it "convert 160-bit mnemonic to seed" $ property mnemonicToSeed160
        it "convert 256-bit mnemonic to seed" $ property mnemonicToSeed256
        it "convert 512-bit mnemonic to seed" $ property mnemonicToSeed512
        it "convert n-bit mnemonic to seed" $ property mnemonicToSeedVar
        it "get bits" $ property getBitsByteCount
        it "get end bits" $ property getBitsEndBits

binWordsToBS :: Serialize a => [a] -> BS.ByteString
binWordsToBS = foldr f BS.empty
  where
    f b a = a `BS.append` encode b

{- Encode mnemonic -}

toMnemonic128 :: (Word64, Word64) -> Bool
toMnemonic128 (a, b) = l == 12
  where
    bs = encode a `BS.append` encode b
    l =
        length .
        C.words . fromRight (error "Could not decode mnemonic senttence") $
        toMnemonic bs

toMnemonic160 :: (Word32, Word64, Word64) -> Bool
toMnemonic160 (a, b, c) = l == 15
  where
    bs = BS.concat [encode a, encode b, encode c]
    l =
        length .
        C.words . fromRight (error "Colud not decode mnemonic sentence") $
        toMnemonic bs

toMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
toMnemonic256 (a, b, c, d) = l == 24
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    l =
        length .
        C.words . fromRight (error "Could not decode mnemonic sentence") $
        toMnemonic bs

toMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
toMnemonic512 ((a, b, c, d), (e, f, g, h)) = l == 48
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    l =
        length .
        C.words . fromRight (error "Colud not decode mnemoonic sentence") $
        toMnemonic bs

toMnemonicVar :: [Word32] -> Property
toMnemonicVar ls = not (null ls) && length ls <= 8 ==> l == wc
  where
    bs = binWordsToBS ls
    bl = BS.length bs
    cb = bl `div` 4
    wc = (cb + bl * 8) `div` 11
    l =
        length . C.words .
        fromRight (error "Could not decode mnemonic sentence") $
        toMnemonic bs

{- Encode/Decode -}

fromToMnemonic128 :: (Word64, Word64) -> Bool
fromToMnemonic128 (a, b) = bs == bs'
  where
    bs = encode a `BS.append` encode b
    bs' =
        fromRight
            (error "Colud not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic160 :: (Word32, Word64, Word64) -> Bool
fromToMnemonic160 (a, b, c) = bs == bs'
  where
    bs = BS.concat [encode a, encode b, encode c]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
fromToMnemonic256 (a, b, c, d) = bs == bs'
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
fromToMnemonic512 ((a, b, c, d), (e, f, g, h)) = bs == bs'
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonicVar :: [Word32] -> Property
fromToMnemonicVar ls = not (null ls) && length ls <= 8 ==> bs == bs'
  where
    bs = binWordsToBS ls
    bs' =
        fromRight
            (error "Colud not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

{- Mnemonic to seed -}

mnemonicToSeed128 :: (Word64, Word64) -> Bool
mnemonicToSeed128 (a, b) = l == 64
  where
    bs = encode a `BS.append` encode b
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed160 :: (Word32, Word64, Word64) -> Bool
mnemonicToSeed160 (a, b, c) = l == 64
  where
    bs = BS.concat [encode a, encode b, encode c]
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed256 :: (Word64, Word64, Word64, Word64) -> Bool
mnemonicToSeed256 (a, b, c, d) = l == 64
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    seed =
        fromRight
            (error "Colud not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
mnemonicToSeed512 ((a, b, c, d), (e, f, g, h)) = l == 64
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeedVar :: [Word32] -> Property
mnemonicToSeedVar ls = not (null ls) && length ls <= 16 ==> l == 64
  where
    bs = binWordsToBS ls
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
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
