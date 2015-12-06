module Network.Haskoin.Block.Tests (tests) where

import Control.Arrow

import Data.String (fromString)
import Data.String.Conversions (cs)
import Test.QuickCheck (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust)

import Network.Haskoin.Block
import Network.Haskoin.Util
import Network.Haskoin.Test

tests :: [Test]
tests =
    [ testGroup "Block tests"
        [ testProperty "decode . encode BlockHash id" decEncBlockHashid ]
    , testGroup "Merkle trees"
        [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
        , testProperty "Width of tree at height 0 is # txns" testBaseWidth
        , testProperty "extract . build partial merkle tree" buildExtractTree
        ]
    , testGroup "Block hashes"
        [ testProperty "Read/Show block hash" testReadShowBlockHash
        , testProperty "From string block hash" fromStringBlockHash
        ]
    ]

decEncBlockHashid :: ArbitraryBlockHash -> Bool
decEncBlockHashid (ArbitraryBlockHash h) =
    fromJust (hexToBlockHash $ blockHashToHex h) == h

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

buildExtractTree :: [(ArbitraryTxHash, Bool)] -> Property
buildExtractTree txs = not (null txs) ==>
    r == (buildMerkleRoot hashes) && m == (map (txh . fst) $ filter snd txs)
  where
    (f, h) = buildPartialMerkle $ map (first txh) txs
    (r, m) = fromRight $ extractMatches f h (length txs)
    hashes = map (txh . fst) txs
    txh (ArbitraryTxHash t) = t

testReadShowBlockHash :: ArbitraryBlockHash -> Bool
testReadShowBlockHash (ArbitraryBlockHash h) = read (show h) == h

fromStringBlockHash :: ArbitraryBlockHash -> Bool
fromStringBlockHash (ArbitraryBlockHash h) = fromString (cs $ blockHashToHex h) == h
