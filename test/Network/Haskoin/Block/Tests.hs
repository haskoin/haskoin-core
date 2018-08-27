module Network.Haskoin.Block.Tests (spec) where

import           Data.Either                          (fromRight)
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net = do
    describe "block hash" $ do
        it "encodes and decodes block hash" $
            property $
            forAll arbitraryBlockHash $ \h ->
                hexToBlockHash (blockHashToHex h) == Just h
        it "From string block hash" $
            property $
            forAll arbitraryBlockHash $ \h ->
                fromString (cs $ blockHashToHex h) == h
    describe "merkle trees" $ do
        it "builds tree of right width at height 1" $ property testTreeWidth
        it "builds tree of right width at height 0" $ property testBaseWidth
        it "builds and extracts partial merkle tree" $
            property $
            forAll
                (listOf1 ((,) <$> arbitraryTxHash <*> arbitrary))
                (buildExtractTree net)

{- Merkle Trees -}

testTreeWidth :: Int -> Property
testTreeWidth i = i /= 0 ==> calcTreeWidth (abs i) (calcTreeHeight $ abs i) == 1

testBaseWidth :: Int -> Property
testBaseWidth i = i /= 0 ==> calcTreeWidth (abs i) 0 == abs i

buildExtractTree :: Network -> [(TxHash, Bool)] -> Bool
buildExtractTree net txs =
    r == buildMerkleRoot (map fst txs) && m == map fst (filter snd txs)
  where
    (f, h) = buildPartialMerkle txs
    (r, m) =
        fromRight (error "Could not extract matches from Merkle tree") $
        extractMatches net f h (length txs)
