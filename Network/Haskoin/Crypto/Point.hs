module Network.Haskoin.Crypto.Point
( Point( InfPoint )
, makePoint
, makeInfPoint
, getAffine, getX, getY
, validatePoint
, isInfPoint
, addPoint
, doublePoint
, mulPoint
, shamirsTrick
, curveB
, curveA
) where

import Data.Bits (shiftR)
import Control.Applicative ((<$>))

import Network.Haskoin.Crypto.Curve (integerA, integerB)
import Network.Haskoin.Crypto.BigWord (FieldP, FieldN)

curveA :: FieldP
curveA = fromInteger integerA

curveB :: FieldP
curveB = fromInteger integerB

{-| 
  Elliptic curves of the form y^2 = x^3 + 7 (mod p)
  Point on the elliptic curve in transformed Jacobian coordinates 
  (X,Y,Z) such that (x,y) = (X/Z^2, Y/Z^3)
  InfPoint is the point at infinity
-}
data Point = Point !FieldP !FieldP !FieldP | InfPoint
    deriving Show

instance Eq Point where
    InfPoint         == InfPoint         = True
    (Point x1 y1 z1) == (Point x2 y2 z2) = a == b && c == d
      where 
        a = x1*z2 ^ (2 :: Int)
        b = x2*z1 ^ (2 :: Int)
        c = y1*z2 ^ (3 :: Int)
        d = y2*z1 ^ (3 :: Int)
    _                == _                = False

-- Create a new point from (x,y) coordinates.
-- Returns Nothing if the point doesn't lie on the curve
makePoint :: FieldP -> FieldP -> Maybe Point
makePoint x y
    | validatePoint point = Just point
    | otherwise = Nothing
  where 
    point = Point x y 1

makeInfPoint :: Point
makeInfPoint = InfPoint

-- Get the original (x,y) coordinates from the Jacobian triple (X,Y,Z)
getAffine :: Point -> Maybe (FieldP, FieldP)
getAffine point = case point of
    InfPoint      -> Nothing
    (Point _ _ 0) -> Nothing
    (Point x y z) -> Just (x/z ^ (2 :: Int), y/z ^ (3 :: Int))

getX :: Point -> Maybe FieldP
getX point = fst <$> (getAffine point)

getY :: Point -> Maybe FieldP
getY point = snd <$> (getAffine point)

-- Section 3.2.2.1 http://www.secg.org/download/aid-780/sec1-v2.pdf
-- point 3.2.2.1.4 is not necessary as h=1
validatePoint :: Point -> Bool
validatePoint point = case getAffine point of
    -- 3.2.2.1.1 (check that point not equal to InfPoint)
    Nothing    -> False 
    -- 3.2.2.1.2 (check that the point lies on the curve)
    Just (x,y) -> y ^ (2 :: Int) == x ^ (3 :: Int) + (curveA * x) + curveB

isInfPoint :: Point -> Bool
isInfPoint InfPoint      = True
isInfPoint (Point _ _ 0) = True
isInfPoint _             = False

-- Elliptic curve point addition
-- http://en.wikibooks.org/wiki/Cryptography/Prime_Curve/Jacobian_Coordinates
addPoint :: Point -> Point -> Point
addPoint InfPoint point = point
addPoint point InfPoint = point
addPoint p1@(Point x1 y1 z1) (Point x2 y2 z2)
    | u1 == u2 = if s1 == s2 then doublePoint p1 else InfPoint
    | otherwise = Point x3 y3 z3
  where 
    u1 = x1*z2 ^ (2 :: Int)
    u2 = x2*z1 ^ (2 :: Int)
    s1 = y1*z2 ^ (3 :: Int)
    s2 = y2*z1 ^ (3 :: Int)
    h  = u2 - u1
    r  = s2 - s1
    x3 = r ^ (2 :: Int) - h ^ (3 :: Int) - 2*u1*h ^ (2 :: Int) 
    y3 = r*(u1 * h ^ (2 :: Int) - x3) - s1 * h ^ (3 :: Int)
    z3 = h * z1 * z2

-- Elliptic curve point doubling 
-- http://en.wikibooks.org/wiki/Cryptography/Prime_Curve/Jacobian_Coordinates
doublePoint :: Point -> Point
doublePoint InfPoint = InfPoint
doublePoint (Point x y z)
    | y == 0 = InfPoint
    | otherwise = Point x' y' z'
  where 
    s  = 4*x*y ^ (2 :: Int)
    m  = 3*x ^ (2 :: Int) + curveA * z ^ (4 :: Int)
    x' = m ^ (2 :: Int) - 2*s
    y' = m*(s - x') - 8*y ^ (4 :: Int)
    z' = 2*y*z

-- Elliptic curve point multiplication
-- Double and add method (weak to side channel attacks)
-- http://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication
mulPoint :: FieldN -> Point -> Point
mulPoint 0 _        = InfPoint
mulPoint 1 p        = p
mulPoint _ InfPoint = InfPoint
mulPoint n p 
    | n == 0    = InfPoint
    | odd n     = addPoint p (mulPoint (n-1) p)
    | otherwise = mulPoint (n `shiftR` 1) (doublePoint p)

-- Efficiently compute r1*p1 + r2*p2
shamirsTrick :: FieldN -> Point -> FieldN -> Point -> Point
shamirsTrick r1 p1 r2 p2 = go r1 r2
  where 
    q      = addPoint p1 p2
    go 0 0 = InfPoint
    go a b 
        | ea && eb  = b2
        | ea        = addPoint b2 p2
        | eb        = addPoint b2 p1
        | otherwise = addPoint b2 q
      where 
        b2 = doublePoint $ go (a `shiftR` 1) (b `shiftR` 1)
        ea = even a
        eb = even b


