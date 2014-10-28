module Network.Haskoin.Util.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (permutations)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (update, fromList)

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Util 

tests :: [Test]
tests = 
    [ testGroup "Utility functions"
        [ testProperty "toStrict( toLazy(bs) ) = bs" fromToLazy
        , testProperty "get( put(Integer) ) = Integer" getPutInteger
        , testProperty "stringToBS( bsToString(s) ) = s" fromToString
        , testProperty "decode'( encode'(s) ) = s" decEncBS
        , testProperty "decodeOrFail'( encode'(s) ) = s" decEncFailBS
        , testProperty "fromHex( toHex(bs) ) = bs" fromToHex
        , testProperty "testing decodeEither" testFromDecode
        , testProperty "compare updateIndex with Data.Sequence" testUpdateIndex
        , testProperty "testing matchTemplate" testMatchTemplate
        , testProperty 
            "testing matchTemplate with two lists" testMatchTemplateLen
        , testProperty "Testing either helper functions" testEither
        ]
    ]

{- Various utilities -}

fromToLazy :: ArbitraryByteString -> Bool
fromToLazy (ArbitraryByteString bs) = (toStrictBS $ toLazyBS bs) == bs

fromToString :: ArbitraryByteString  -> Bool
fromToString (ArbitraryByteString bs) = (stringToBS $ bsToString bs) == bs

decEncBS :: ArbitraryByteString -> Bool
decEncBS (ArbitraryByteString bs) = (decode' $ encode' bs) == bs

decEncFailBS :: ArbitraryByteString -> Bool
decEncFailBS (ArbitraryByteString bs) = case (decodeOrFail' $ encode' bs) of
    (Left _)            -> False
    (Right (_, _, res)) -> res == bs

getPutInteger :: Integer -> Bool
getPutInteger i = (bsToInteger $ integerToBS p) == p
  where 
    p = abs i

fromToHex :: ArbitraryByteString -> Bool
fromToHex (ArbitraryByteString bs) = (fromJust $ hexToBS $ bsToHex bs) == bs

testFromDecode :: ArbitraryByteString -> Integer -> Integer -> Bool
testFromDecode (ArbitraryByteString bs) def v = case decodeOrFail' bs of
    (Left _)          -> fromDecode bs def (*v) == def 
    (Right (_,_,res)) -> fromDecode bs def (*v) == res*v 

testUpdateIndex :: [Int] -> Int -> Int -> Bool 
testUpdateIndex xs v i = 
    (updateIndex i xs $ const v) == (toList $ Seq.update i v s)
  where 
    s = Seq.fromList xs

testMatchTemplate :: [Int] -> Int -> Bool
testMatchTemplate as i = catMaybes res == bs
  where 
    res = matchTemplate as bs (==)
    idx = if length as == 0 then 0 else i `mod` length as 
    bs  = (permutations as) !! idx

testMatchTemplateLen :: [Int] -> [Int] -> Bool
testMatchTemplateLen as bs = length bs == length res
  where 
    res = matchTemplate as bs (==)

testEither :: Either String Int -> Bool
testEither e = case e of
    (Right v) -> (isRight e)
              && (not $ isLeft e) 
              && (fromRight e == v)
              && (eitherToMaybe e == Just v)
    (Left v)  -> (isLeft e)
              && (not $ isRight e)
              && (fromLeft e == v)
              && (eitherToMaybe e == Nothing)

