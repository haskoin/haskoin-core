module Network.Haskoin.Crypto.Point.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust)

import Network.Haskoin.Test
import Network.Haskoin.Crypto
import Network.Haskoin.Internals
    ( Point(..)
    , getAffine
    , addPoint
    , mulPoint
    , shamirsTrick
    , makeInfPoint
    , makePoint
    , validatePoint
    , isInfPoint
    , doublePoint
    )

tests :: [Test]
tests = 
    [ testGroup "Elliptic curve point arithmetic"
        [ testProperty "P is on the curve" checkOnCurve
        , testProperty "P1 + P2 is on the curve" addOnCurve
        , testProperty "n*P is on the curve" mulOnCurve
        , testProperty "makePoint (getAffine P) = P" fromToAffine
        , testProperty "P + InfPoint = P" addInfPoint
        , testProperty "InfPoint + P = P" addInfPoint'
        , testProperty "P1 + P2 = P2 + P1" addCommutative
        , testProperty "(P1 + P2) + P3 = P1 + (P2 + P3)" addAssoc
        , testProperty "(x,y) + (x,-y) = InfPoint" addInverseY
        , testProperty "double P = P + P" doubleAddPoint
        , testProperty "double P = 2*P" doubleMulPoint
        , testProperty "n*P = P + (n-1)*P" mulPointInduction
        , testProperty "a*P + b*P = (a + b)*P" mulDistributivity
        , testProperty "shamirsTrick = n1*P1 + n2*P2" testShamirsTrick
        , testProperty "point equality" testPointEqual
        ]
    ]

{- Elliptic curve point arithmetic -}

checkOnCurve :: ArbitraryPoint -> Bool
checkOnCurve (ArbitraryPoint p) = validatePoint p

addOnCurve :: ArbitraryPoint -> ArbitraryPoint -> Bool
addOnCurve (ArbitraryPoint p1) (ArbitraryPoint p2) = 
    case addPoint p1 p2 of
        InfPoint -> True
        p        -> validatePoint p

mulOnCurve :: ArbitraryPoint -> FieldN -> Bool
mulOnCurve (ArbitraryPoint p1) n = case mulPoint n p1 of
    InfPoint -> True
    p        -> validatePoint p

fromToAffine :: ArbitraryPoint -> Property
fromToAffine (ArbitraryPoint p) = not (isInfPoint p) ==> 
    (makePoint x y) == Just p
  where 
    (x,y) = fromJust $ getAffine p

addInfPoint :: ArbitraryInfPoint -> Bool
addInfPoint (ArbitraryInfPoint p) = addPoint p makeInfPoint == p

addInfPoint' :: ArbitraryInfPoint -> Bool
addInfPoint' (ArbitraryInfPoint p) = addPoint makeInfPoint p == p

addCommutative :: ArbitraryInfPoint -> ArbitraryInfPoint -> Bool
addCommutative (ArbitraryInfPoint p1) (ArbitraryInfPoint p2) = 
    addPoint p1 p2 == addPoint p2 p1

addAssoc :: ArbitraryInfPoint -> ArbitraryInfPoint -> ArbitraryInfPoint -> Bool
addAssoc (ArbitraryInfPoint p1) (ArbitraryInfPoint p2) (ArbitraryInfPoint p3) =
    addPoint (addPoint p1 p2) p3 == addPoint p1 (addPoint p2 p3)

addInverseY :: ArbitraryInfPoint -> Bool
addInverseY (ArbitraryInfPoint p1) = case (getAffine p1) of
    (Just (x,y)) -> addPoint p1 (fromJust $ makePoint x (-y)) == makeInfPoint
    Nothing      -> True

doubleAddPoint :: ArbitraryInfPoint -> Bool
doubleAddPoint (ArbitraryInfPoint p) = doublePoint p == addPoint p p

doubleMulPoint :: ArbitraryInfPoint -> Bool
doubleMulPoint (ArbitraryInfPoint p) = doublePoint p == mulPoint 2 p

mulPointInduction :: FieldN -> ArbitraryInfPoint -> Property
mulPointInduction i (ArbitraryInfPoint p) = i > 2 ==> 
    mulPoint i p == addPoint p (mulPoint (i-1) p)

mulDistributivity :: FieldN -> FieldN -> ArbitraryInfPoint -> Bool
mulDistributivity a b (ArbitraryInfPoint p) = 
    (addPoint (mulPoint a p) (mulPoint b p)) == mulPoint (a + b) p

testShamirsTrick :: FieldN -> ArbitraryInfPoint 
                 -> FieldN -> ArbitraryInfPoint 
                 -> Bool
testShamirsTrick n1 (ArbitraryInfPoint p1) n2 (ArbitraryInfPoint p2) = 
    shamirRes == normalRes
  where 
    shamirRes = shamirsTrick n1 p1 n2 p2
    normalRes = addPoint (mulPoint n1 p1) (mulPoint n2 p2)  

testPointEqual :: ArbitraryInfPoint -> ArbitraryInfPoint -> Bool
testPointEqual (ArbitraryInfPoint p1) (ArbitraryInfPoint p2) = case (p1,p2) of
    (InfPoint, InfPoint) -> p1 == p2
    (_, InfPoint)        -> p1 /= p2
    (InfPoint, _)        -> p1 /= p2
    _                    -> go
  where 
    go | x1 == x2 && y1 == y2 = p1 == p2
       | otherwise            = p1 /= p2
    (x1,y1) = fromJust $ getAffine p1
    (x2,y2) = fromJust $ getAffine p2

