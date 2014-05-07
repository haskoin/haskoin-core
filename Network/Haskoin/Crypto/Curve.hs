module Network.Haskoin.Crypto.Curve
( pairG
, curveP
, curveN
, integerB
, integerA
) where

-- SECP256k1 curve parameters

pairG :: (Integer, Integer)
pairG = ( 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        , 0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 
        )

curveP :: Integer
curveP = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f 

curveN :: Integer
curveN = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 

integerB :: Integer
integerB = 7

integerA :: Integer
integerA = 0

