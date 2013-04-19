module Bitcoin.Protocol 
( BitcoinProtocol(bitcoinGet, bitcoinPut)

, BitcoinGet, BitcoinPut

, runGet, runPut

, Word8, Word16, Word32, Word64, Word128, Word160, Word256
, getBool, putBool
, getWord8, putWord8

, getWord16le,  getWord16be,  putWord16le,  putWord16be
, getWord32le,  getWord32be,  putWord32le,  putWord32be
, getWord64le,  getWord64be,  putWord64le,  putWord64be
, getWord128le, getWord128be, putWord128le, putWord128be
, getWord160le, getWord160be, putWord160le, putWord160be
, getWord256le, getWord256be, putWord256le, putWord256be

, getByteString, putByteString

, hasMore
) where

import Data.Word
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative

import Bitcoin.Protocol.BigWord

type BitcoinGet = Get
type BitcoinPut = Put

class BitcoinProtocol a where
    bitcoinGet :: BitcoinGet a
    bitcoinPut :: a -> BitcoinPut

getBool :: BitcoinGet Bool
getBool = go =<< getWord8
    where go 0 = return False
          go _ = return True

putBool :: Bool -> BitcoinPut 
putBool True  = putWord8 1
putBool False = putWord8 0

getWord128le :: BitcoinGet Word128
getWord128le = do
    littleEnd <- (fromIntegral <$> getWord64le)
    bigEnd    <- (fromIntegral <$> getWord64le)
    return $ (bigEnd `shiftL` 64) + littleEnd

putWord128le :: Word128 -> BitcoinPut
putWord128le w = do
    putWord64le $ fromIntegral w
    putWord64le $ fromIntegral (w `shiftR` 64)

getWord160le :: BitcoinGet Word160
getWord160le = do
    littleEnd <- (fromIntegral <$> getWord32le)
    bigEnd    <- (fromIntegral <$> getWord128le)
    return $ (bigEnd `shiftL` 32) + littleEnd

putWord160le :: Word160 -> BitcoinPut
putWord160le w = do
    putWord32le $ fromIntegral w
    putWord128le $ fromIntegral (w `shiftR` 32)

getWord256le :: BitcoinGet Word256
getWord256le = do
    littleEnd <- (fromIntegral <$> getWord128le)
    bigEnd    <- (fromIntegral <$> getWord128le)
    return $ (bigEnd `shiftL` 128) + littleEnd

putWord256le :: Word256 -> BitcoinPut
putWord256le w = do
    putWord128le $ fromIntegral w
    putWord128le $ fromIntegral (w `shiftR` 128)

getWord128be :: BitcoinGet Word128
getWord128be = do
    bigEnd    <- (fromIntegral <$> getWord64be)
    littleEnd <- (fromIntegral <$> getWord64be)
    return $ (bigEnd `shiftL` 64) + littleEnd

putWord128be :: Word128 -> BitcoinPut
putWord128be w = do
    putWord64be $ fromIntegral (w `shiftR` 64)
    putWord64be $ fromIntegral w

getWord160be :: BitcoinGet Word160
getWord160be = do
    bigEnd    <- (fromIntegral <$> getWord32be)
    littleEnd <- (fromIntegral <$> getWord128be)
    return $ (bigEnd `shiftL` 128) + littleEnd

putWord160be :: Word160 -> BitcoinPut
putWord160be w = do
    putWord32be $ fromIntegral (w `shiftR` 128)
    putWord128be $ fromIntegral w

getWord256be :: BitcoinGet Word256
getWord256be = do
    bigEnd    <- (fromIntegral <$> getWord128be)
    littleEnd <- (fromIntegral <$> getWord128be)
    return $ (bigEnd `shiftL` 128) + littleEnd

putWord256be :: Word256 -> BitcoinPut
putWord256be w = do
    putWord128be $ fromIntegral (w `shiftR` 128)
    putWord128be $ fromIntegral w

hasMore :: Get a -> Get a -> Get a
hasMore t f = isEmpty >>= (\e -> if e then f else t)


