module Network.Haskoin.Block.Tests (tests) where

import Test.QuickCheck (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust)

import Network.Haskoin.Network
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Util

type Net = Prodnet

net :: Net
net = undefined

tests :: [Test]
tests = 
    [ testGroup "Block tests"
        [ testProperty "decode . encode BlockHash id" decEncBlockHashid ]
    , testGroup "Merkle Trees"
        [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
        , testProperty "Width of tree at height 0 is # txns" testBaseWidth
        , testProperty "extract . build partial merkle tree" buildExtractTree
        ]
    ]

decEncBlockHashid :: BlockHash -> Bool
decEncBlockHashid h = (fromJust $ decodeBlockHashLE $ encodeBlockHashLE h) == h

{- Merkle Trees -}

testTreeWidth :: Int -> Property
testTreeWidth i = i /= 0 ==>
    calcTreeWidth i' (calcTreeHeight i') == 1
  where
    i' = abs i

testBaseWidth :: Int -> Property
testBaseWidth i = i /= 0 ==>
    calcTreeWidth i' 0 == i'
  where
    i' = abs i

buildExtractTree :: [(TxHash,Bool)] -> Property
buildExtractTree txs = not (null txs) ==>
    r == (buildMerkleRoot hashes) && m == (map fst $ filter snd txs)
  where
    (f,h)  = buildPartialMerkle txs
    (r,m)  = fromRight $ extractMatches net f h (length txs)
    hashes = map fst txs

