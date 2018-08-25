{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.CashAddr
    ( CashPrefix
    , CashVersion
    , cashAddrDecode
    , cashAddrEncode
    , cash32decode
    , cash32encode
    ) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Word
import           Debug.Trace
import           Network.Haskoin.Constants
import           Network.Haskoin.Util

type CashPrefix = ByteString
type CashVersion = Word8
type CashAddr = ByteString
type Cash32 = ByteString
type Word5 = Word8

charset :: [Char]
charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

base32char :: Char -> Maybe Word8
base32char = fmap fromIntegral . (`elemIndex` charset)

cashAddrDecode :: CashAddr -> Maybe (CashVersion, ByteString)
cashAddrDecode ca' = do
    epfx <- cashAddrPrefix
    guard (B.length ca <= 90)
    guard (C.map toUpper ca' == ca' || ca == ca')
    let (cpfx', cdat) = C.breakEnd (== ':') ca
        cpfx
            | B.null cpfx' = epfx
            | otherwise = B.init cpfx'
    (dpfx, bs) <- cash32decode (epfx <> ":" <> cdat)
    guard (not (B.null bs))
    guard (dpfx == epfx)
    let len = decodeCashLength (B.head bs)
        ver = decodeCashVersion (B.head bs)
    guard (B.length (B.tail bs) == len)
    return (ver, B.tail bs)
  where
    ca = C.map toLower ca'

cashAddrEncode :: CashVersion -> ByteString -> Maybe CashAddr
cashAddrEncode cv bs = do
    pfx <- cashAddrPrefix
    ver <- encodeCashVersion cv
    len <- encodeCashLength (B.length bs)
    let vb = ver .|. len
        pl = vb `B.cons` bs
    return (cash32encode pfx pl)

cash32decode :: Cash32 -> Maybe (CashPrefix, ByteString)
cash32decode bs' = do
    let bs = C.map toLower bs'
    guard (B.length bs' <= 90)
    guard (C.map toUpper bs' == bs' || bs == bs')
    let (pfx', dat) = C.breakEnd (== ':') bs
    pfx <-
        if B.null pfx' || pfx' == C.singleton ':'
            then Nothing
            else Just (B.init pfx')
    b32 <- B.pack <$> mapM base32char (C.unpack dat)
    let px = B.map (.&. 0x1f) pfx
        pd = px <> B.singleton 0 <> b32
        cs = cash32Polymod pd
        bb = B.take (B.length b32 - 8) b32
    guard (verifyCash32Polymod cs)
    let out = toBase256 (B.unpack bb)
    return (pfx, B.pack out)

cash32encode :: CashPrefix -> ByteString -> Cash32
cash32encode pfx bs =
    let b32 = B.pack (toBase32 (B.unpack bs))
        px = B.map (.&. 0x1f) pfx
        pd = px <> B.singleton 0 <> b32 <> B.replicate 8 0
        cs = cash32Polymod pd
        c32 = B.map f (b32 <> cs)
        f = fromIntegral . ord . (charset !!) . fromIntegral
    in pfx <> ":" <> c32

toBase32 :: [Word8] -> [Word5]
toBase32 = map fromIntegral . fst . convertBits True 8 5 . map fromIntegral

toBase256 :: [Word5] -> [Word8]
toBase256 = map fromIntegral . fst . convertBits False 5 8 . map fromIntegral

decodeCashVersion :: Word8 -> Word8
decodeCashVersion ver = ver `shiftR` 3

encodeCashVersion :: Word8 -> Maybe Word8
encodeCashVersion ver = do
    guard (ver == ver .&. 0x1f)
    return (ver `shiftL` 3)

decodeCashLength :: Word8 -> Int
decodeCashLength w8 = lengths !! fromIntegral (w8 .&. 0x07)
  where
    lengths = [20, 24, 28, 32, 40, 48, 56, 64]

encodeCashLength :: Int -> Maybe Word8
encodeCashLength len
    | len == 20 = Just 0
    | len == 24 = Just 1
    | len == 28 = Just 2
    | len == 32 = Just 3
    | len == 40 = Just 4
    | len == 48 = Just 5
    | len == 56 = Just 6
    | len == 64 = Just 7
    | otherwise = Nothing

cash32Polymod :: Cash32 -> Cash32
cash32Polymod v =
    B.pack
        [fromIntegral (polymod `shiftR` (5 * (7 - i))) .&. 0x1f | i <- [0 .. 7]]
  where
    polymod = B.foldl' outer (1 :: Word64) v `xor` 1
    outer c d =
        let c0 = (fromIntegral (c `shiftR` 35) :: Word8)
            c' = ((c .&. 0x07ffffffff) `shiftL` 5) `xor` fromIntegral d
        in foldl' (inner c0) c' (zip [0 ..] generator)
    generator =
        [0x98f2bc8e61, 0x79b76d99e2, 0xf33e5fb3c4, 0xae2eabe2a8, 0x1e4f43e470]
    inner c0 c (b, g)
        | c0 `testBit` b = c `xor` g
        | otherwise = c

verifyCash32Polymod :: ByteString -> Bool
verifyCash32Polymod = (== B.replicate 8 0)
