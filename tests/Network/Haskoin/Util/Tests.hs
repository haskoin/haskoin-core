module Network.Haskoin.Util.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (permutations)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (update, fromList)
import qualified Data.ByteString as BS (ByteString)

import Network.Haskoin.Util 
import Network.Haskoin.Util.BuildMonad
import Network.Haskoin.Util.Arbitrary()

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
    , testGroup "Build Monad"
        [ testProperty "Build monadic composition" testBuildCompose
        , testProperty "Testing guardPartial" testGuardPartial
        ]
    ]

{- Various utilities -}

fromToLazy :: BS.ByteString -> Bool
fromToLazy bs = (toStrictBS $ toLazyBS bs) == bs

fromToString :: BS.ByteString -> Bool
fromToString bs = (stringToBS $ bsToString bs) == bs

decEncBS :: BS.ByteString -> Bool
decEncBS bs = (decode' $ encode' bs) == bs

decEncFailBS :: BS.ByteString -> Bool
decEncFailBS bs = case (decodeOrFail' $ encode' bs) of
    (Left _)            -> False
    (Right (_, _, res)) -> res == bs

getPutInteger :: Integer -> Bool
getPutInteger i = (bsToInteger $ integerToBS p) == p
  where 
    p = abs i

fromToHex :: BS.ByteString -> Bool
fromToHex bs = (fromJust $ hexToBS $ bsToHex bs) == bs

testFromDecode :: BS.ByteString -> Integer -> Integer -> Bool
testFromDecode bs def v = case decodeOrFail' bs of
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

{- Build Monad -}

testBuildCompose :: Build Int -> Build Int -> Bool
testBuildCompose ma mb
    | isBroken ma || isBroken mb   = isBroken res
    | isPartial ma || isPartial mb = isPartial res
    | otherwise                    = isComplete res
  where 
    res = ma >>= (\a -> mb >>= (\b -> return $ a + b ))

testGuardPartial :: Build Int -> Build Int -> Bool
testGuardPartial ma mb
    | isBroken ma || isBroken mb   = isBroken res
    | otherwise                    = isPartial res
  where 
    res = do
        a <- ma
        guardPartial False
        b <- mb
        return $ a + b

