{-|
  Arbitrary types for Network.Haskoin.Block
-}
module Network.Haskoin.Test.Block where

import           Network.Haskoin.Block.Merkle
import           Network.Haskoin.Block.Common
import           Network.Haskoin.Constants
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Network
import           Network.Haskoin.Test.Transaction
import           Test.QuickCheck

arbitraryBlock :: Network -> Gen Block
arbitraryBlock net = do
    h   <- arbitraryBlockHeader
    c   <- choose (0,10)
    txs <- vectorOf c (arbitraryTx net)
    return $ Block h txs

arbitraryBlockHeader :: Gen BlockHeader
arbitraryBlockHeader =
    BlockHeader <$> arbitrary
                      <*> arbitraryBlockHash
                      <*> arbitraryHash256
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

arbitraryBlockHash :: Gen BlockHash
arbitraryBlockHash = BlockHash <$> arbitraryHash256

arbitraryGetBlocks :: Gen GetBlocks
arbitraryGetBlocks =
    GetBlocks <$> arbitrary
              <*> listOf1 arbitraryBlockHash
              <*> arbitraryBlockHash

arbitraryGetHeaders :: Gen GetHeaders
arbitraryGetHeaders =
    GetHeaders <$> arbitrary
               <*> listOf1 arbitraryBlockHash
               <*> arbitraryBlockHash

arbitraryHeaders :: Gen Headers
arbitraryHeaders =
    Headers <$> listOf1 ((,) <$> arbitraryBlockHeader <*> arbitraryVarInt)

arbitraryMerkleBlock :: Gen MerkleBlock
arbitraryMerkleBlock = do
    bh     <- arbitraryBlockHeader
    ntx    <- arbitrary
    hashes <- listOf1 arbitraryHash256
    c      <- choose (1,10)
    flags  <- vectorOf (c*8) arbitrary
    return $ MerkleBlock bh ntx hashes flags

