module Network.Haskoin.Crypto.NumberTheory
( extendedModGCD
, mulInverse
) where

-- Extended euclidean algorithm
-- Calculates the multiplicative inverse modulo p
extendedModGCD :: Integer -> Integer -> Integer -> (Integer, Integer)
extendedModGCD a b p
    | b == 0 = (1,0)
    | otherwise = (t, (s - q*t) `mod` p)
  where 
    (q,r) = quotRem a b
    (s,t) = extendedModGCD b r p

-- Find multiplicative inverse of a : a*s = 1 (mod p)
mulInverse :: Integer -> Integer -> Integer
mulInverse a p 
    | a*s `mod` p == 1 = s
    | otherwise = error "No multiplicative inverse (mod p) for a"
  where 
    (s,_) = extendedModGCD a p p

