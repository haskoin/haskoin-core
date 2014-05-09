module Network.Haskoin.Crypto.Merkle.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Haskoin.Crypto.Arbitrary ()
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Merkle Trees"
        [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
        , testProperty "Width of tree at height 0 is # txns" testBaseWidth
        , testProperty "extract . build partial merkle tree" buildExtractTree
        ]
    ]

{- Merkle Trees -}

testTreeWidth :: Int -> Property
testTreeWidth i = i > 0 ==> calcTreeWidth i (calcTreeHeight i) == 1

testBaseWidth :: Int -> Property
testBaseWidth i = i > 0 ==> calcTreeWidth i 0 == i

buildExtractTree :: [(Hash256,Bool)] -> Property
buildExtractTree txs = not (null txs) ==>
    r == (buildMerkleRoot hashes) && m == (map fst $ filter snd txs)
  where
    (f,h)  = buildPartialMerkle txs
    (r,m)  = fromRight $ extractMatches f h (length txs)
    hashes = map fst txs


