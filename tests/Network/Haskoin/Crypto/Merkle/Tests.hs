module Network.Haskoin.Crypto.Merkle.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Network.Haskoin.Crypto.Arbitrary()
import Network.Haskoin.Block.Arbitrary()
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize merkle blocks"
        [ testProperty "MerkleBlock" (metaBinary :: MerkleBlock -> Bool)
        ]
    , testGroup "Merkle Trees"
        [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
        , testProperty "Width of tree at height 0 is # txns" testBaseWidth
        , testProperty "extract . build partial merkle tree" buildExtractTree
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

{- Merkle Trees -}

testTreeWidth :: Int -> Property
testTreeWidth i = i > 0 ==> calcTreeWidth i (calcTreeHeight i) == 1

testBaseWidth :: Int -> Property
testBaseWidth i = i > 0 ==> calcTreeWidth i 0 == i

buildExtractTree :: [(TxHash,Bool)] -> Property
buildExtractTree txs = not (null txs) ==>
    r == (buildMerkleRoot hashes) && m == (map fst $ filter snd txs)
  where
    (f,h)  = buildPartialMerkle txs
    (r,m)  = fromRight $ extractMatches f h (length txs)
    hashes = map fst txs


