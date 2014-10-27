module Network.Haskoin.Block.Tests (tests) where

import Test.QuickCheck 
    ( Arbitrary
    , listOf1
    , arbitrary
    , vectorOf
    , Property
    , (==>)
    )

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Binary (Binary)

import Network.Haskoin.Test.Block
import Network.Haskoin.Test.Crypto

import Network.Haskoin.Block.Types
import Network.Haskoin.Block.Merkle
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize block types"
        [ testProperty "Block" $ \(ArbitraryBlock x) -> metaBinary x
        , testProperty "BlockHeader" $ \(ArbitraryBlockHeader x) -> metaBinary x
        , testProperty "GetBlocks" $ \(ArbitraryGetBlocks x) -> metaBinary x
        , testProperty "GetHeaders" $ \(ArbitraryGetHeaders x) -> metaBinary x
        , testProperty "Headers" $ \(ArbitraryHeaders x) -> metaBinary x
        , testProperty "MerkleBlock" $ \(ArbitraryMerkleBlock x) -> metaBinary x
        ]
    , testGroup "Block tests"
        [ testProperty "decode . encode BlockHash id" decEncBlockHashid ]
    , testGroup "Merkle Trees"
        [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
        , testProperty "Width of tree at height 0 is # txns" testBaseWidth
        , testProperty "extract . build partial merkle tree" buildExtractTree
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

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
    (r,m)  = fromRight $ extractMatches f h (length txs)
    hashes = map fst txs

