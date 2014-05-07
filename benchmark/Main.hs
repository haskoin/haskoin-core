{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Control.Applicative

import System.Random

import Data.Maybe
import Data.Time.Clock

import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve

bench :: Int -> String -> IO a -> IO a
bench n s f = do
    start <- getCurrentTime
    !r <- f
    end <- getCurrentTime
    let t = (diffUTCTime end start)
    putStrLn $ "----------------------------"
    putStrLn $ s ++ " (" ++ (show n) ++ " samples)"
    putStrLn $ "Total time: " ++ (show t)
    putStrLn $ "Op/sec    : " ++ (show $ (fromIntegral n)/t)
    seq r $ return r

main :: IO ()
main = do

    _ <- bench (10^(7 :: Int)) "BigWord multiplication (mod n)" 
        (return $ testBigWord $ 10^(7 :: Int))
        
    _ <- bench (10^(5 :: Int)) "BigWord inversion (mod n)" 
        (return $ invBigWord $ 10^(5 :: Int))

    let elems = 2000
        msg   = fromInteger $ curveN - 10

    !priv <- replicateM elems $
                (fromJust . makePrvKey) <$> 
                getStdRandom (randomR (1, curveN))

    !pub <- bench elems "Point multiplications" $ forM priv $ \x -> 
        return $! derivePubKey x

    _ <- bench 100000 "Point additions" $ 
        forM (take 100000 $ cycle pub) $ \x -> do
            let !a = pubKeyPoint x
            return $! addPoint a a

    _ <- bench 100000 "Point doubling" $ 
        forM (take 100000 $ cycle pub) $ \x -> do
            let !a = pubKeyPoint x
            return $! doublePoint a

    _ <- bench elems "Shamirs trick" $ 
        forM (priv `zip` pub) $ \(d,q) -> do
            let !a = prvKeyFieldN d
                !b = pubKeyPoint q
            return $! shamirsTrick a b a b

    !sigs <- bench elems "Signature creations" $ 
        withSource devURandom $! forM priv (signMsg msg) 
        
    _ <- bench elems "Signature verifications" $ 
        forM (sigs `zip` pub) $ \(s,q) -> 
            return $! verifySig msg s q

    return ()

testBigWord :: Int -> FieldN
testBigWord maxVal = go 2 0
  where 
    go i n
        | n < maxVal = go (i*i) (n + 1)
        | otherwise = i

invBigWord :: Int -> FieldN
invBigWord maxVal = go 1 0
  where 
    go i n
        | n < maxVal = go ((inverseN i) + 1) (n + 1)
        | otherwise = i

