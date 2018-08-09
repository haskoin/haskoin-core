module Network.Haskoin.Block.Tests (tests) where

import           Data.Either                          (fromRight)
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Network.Haskoin.Block
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup
          "Block hash tests"
          [ testProperty "decode . encode block hash" $
            forAll arbitraryBlockHash $ \h ->
                hexToBlockHash (blockHashToHex h) == Just h
          , testProperty "From string block hash" $
            forAll arbitraryBlockHash $ \h ->
                fromString (cs $ blockHashToHex h) == h
          ]
    , testGroup
          "Merkle trees"
          [ testProperty "Width of tree at maxmum height = 1" testTreeWidth
          , testProperty "Width of tree at height 0 is # txns" testBaseWidth
          , testProperty "extract . build partial merkle tree" $
            forAll
                (listOf1 ((,) <$> arbitraryTxHash <*> arbitrary))
                buildExtractTree
          ]
    ]

{- Merkle Trees -}

testTreeWidth :: Int -> Property
testTreeWidth i = i /= 0 ==> calcTreeWidth (abs i) (calcTreeHeight $ abs i) == 1

testBaseWidth :: Int -> Property
testBaseWidth i = i /= 0 ==> calcTreeWidth (abs i) 0 == abs i

buildExtractTree :: [(TxHash, Bool)] -> Bool
buildExtractTree txs =
    r == buildMerkleRoot (map fst txs) && m == map fst (filter snd txs)
  where
    (f, h) = buildPartialMerkle txs
    (r, m) =
        fromRight (error "Could not extract matches from Merkle tree") $
        extractMatches f h (length txs)
