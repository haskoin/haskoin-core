{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Function to deal with Merkle trees inside blocks.
module Bitcoin.Block.Merkle (
    -- * Merkle Blocks
    MerkleBlock (..),
    MerkleRoot,
    FlagBits,
    PartialMerkleTree,
    buildMerkleRoot,
    merkleBlockTxs,
    testMerkleRoot,
    buildPartialMerkle,
    decodeMerkleFlags,
    encodeMerkleFlags,
    calcTreeHeight,
    calcTreeWidth,
    hash2,
    calcHash,
    traverseAndBuild,
    traverseAndExtract,
    extractMatches,
    splitIn,
    boolsToWord8,
) where

import Control.DeepSeq
import Control.Monad (forM_, replicateM, when)
import Data.Binary (Binary (..))
import Data.Bits
import qualified Data.ByteString as BS
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Either (isRight)
import Data.Hashable
import Data.Maybe
import Data.Serialize (Serialize (..))
import Data.Word (Word32, Word8)
import GHC.Generics
import Bitcoin.Block.Common
import Bitcoin.Crypto.Hash
import Bitcoin.Data
import Bitcoin.Network.Common
import Bitcoin.Transaction.Common


-- | Hash of the block's Merkle root.
type MerkleRoot = Hash256


-- | Bits that are used to rebuild partial merkle tree transaction hash list.
type FlagBits = [Bool]


-- | Partial Merkle tree for a filtered block.
type PartialMerkleTree = [Hash256]


-- | Filtered block: a block with a partial Merkle tree that only includes the
-- transactions that pass a bloom filter that was negotiated.
data MerkleBlock = MerkleBlock
    { merkleHeader :: !BlockHeader
    -- ^ block header
    , merkleTotalTxns :: !Word32
    -- ^ total number of transactions in block
    , mHashes :: !PartialMerkleTree
    -- ^ hashes in depth-first order
    , mFlags :: !FlagBits
    -- ^ bits to rebuild partial merkle tree
    }
    deriving (Eq, Show, Read, Generic, Hashable, NFData)


instance Serial MerkleBlock where
    deserialize = do
        header <- deserialize
        ntx <- getWord32le
        (VarInt matchLen) <- deserialize
        hashes <- replicateM (fromIntegral matchLen) deserialize
        (VarInt flagLen) <- deserialize
        ws <- replicateM (fromIntegral flagLen) getWord8
        return $ MerkleBlock header ntx hashes (decodeMerkleFlags ws)


    serialize (MerkleBlock h ntx hashes flags) = do
        serialize h
        putWord32le ntx
        putVarInt $ length hashes
        forM_ hashes serialize
        let ws = encodeMerkleFlags flags
        putVarInt $ length ws
        forM_ ws putWord8


instance Binary MerkleBlock where
    put = serialize
    get = deserialize


instance Serialize MerkleBlock where
    put = serialize
    get = deserialize


-- | Unpack Merkle flags into 'FlagBits' structure.
decodeMerkleFlags :: [Word8] -> FlagBits
decodeMerkleFlags ws =
    [ b | p <- [0 .. length ws * 8 - 1], b <- [testBit (ws !! (p `div` 8)) (p `mod` 8)]
    ]


-- | Pack Merkle flags from 'FlagBits'.
encodeMerkleFlags :: FlagBits -> [Word8]
encodeMerkleFlags bs = map boolsToWord8 $ splitIn 8 bs


-- | Computes the height of a Merkle tree.
calcTreeHeight ::
    -- | number of transactions (leaf nodes)
    Int ->
    -- | height of the merkle tree
    Int
calcTreeHeight ntx
    | ntx < 2 = 0
    | even ntx = 1 + calcTreeHeight (ntx `div` 2)
    | otherwise = calcTreeHeight $ ntx + 1


-- | Computes the width of a Merkle tree at a specific height. The transactions
-- are at height 0.
calcTreeWidth ::
    -- | number of transactions (leaf nodes)
    Int ->
    -- | height at which we want to compute the width
    Int ->
    -- | width of the Merkle tree
    Int
calcTreeWidth ntx h = (ntx + (1 `shiftL` h) - 1) `shiftR` h


-- | Computes the root of a Merkle tree from a list of leaf node hashes.
buildMerkleRoot ::
    -- | transaction hashes (leaf nodes)
    [TxHash] ->
    -- | root of the Merkle tree
    MerkleRoot
buildMerkleRoot txs = calcHash (calcTreeHeight $ length txs) 0 txs


-- | Concatenate and compute double SHA256.
hash2 :: Hash256 -> Hash256 -> Hash256
hash2 a b = doubleSHA256 $ runPutS (serialize a) <> runPutS (serialize b)


-- | Computes the hash of a specific node in a Merkle tree.
calcHash ::
    -- | height of the node
    Int ->
    -- | position of the node (0 for the leftmost node)
    Int ->
    -- | transaction hashes (leaf nodes)
    [TxHash] ->
    -- | hash of the node at the specified position
    Hash256
calcHash height pos txs
    | height < 0 || pos < 0 = error "calcHash: Invalid parameters"
    | height == 0 = getTxHash $ txs !! pos
    | otherwise = hash2 left right
  where
    left = calcHash (height - 1) (pos * 2) txs
    right
        | pos * 2 + 1 < calcTreeWidth (length txs) (height - 1) =
            calcHash (height - 1) (pos * 2 + 1) txs
        | otherwise = left


-- | Build a partial Merkle tree. Provide a list of tuples with all transaction
-- hashes in the block, and whether the transaction is to be included in the
-- partial tree. Returns a flag bits structure and the computed partial Merkle
-- tree.
buildPartialMerkle ::
    -- | transaction hash and whether to include
    [(TxHash, Bool)] ->
    -- | flag bits and partial Merkle tree
    (FlagBits, PartialMerkleTree)
buildPartialMerkle hs = traverseAndBuild (calcTreeHeight $ length hs) 0 hs


-- | Helper function to build partial Merkle tree. Used by 'buildPartialMerkle'
-- above.
traverseAndBuild ::
    Int -> Int -> [(TxHash, Bool)] -> (FlagBits, PartialMerkleTree)
traverseAndBuild height pos txs
    | height < 0 || pos < 0 = error "traverseAndBuild: Invalid parameters"
    | height == 0 || not match = ([match], [calcHash height pos t])
    | otherwise = (match : lb ++ rb, lh ++ rh)
  where
    t = map fst txs
    s = pos `shiftL` height
    e = min (length txs) $ (pos + 1) `shiftL` height
    match = any snd $ take (e - s) $ drop s txs
    (lb, lh) = traverseAndBuild (height - 1) (pos * 2) txs
    (rb, rh)
        | (pos * 2 + 1) < calcTreeWidth (length txs) (height - 1) =
            traverseAndBuild (height - 1) (pos * 2 + 1) txs
        | otherwise = ([], [])


-- | Helper function to extract transaction hashes from partial Merkle tree.
traverseAndExtract ::
    Int ->
    Int ->
    Int ->
    FlagBits ->
    PartialMerkleTree ->
    Maybe (MerkleRoot, [TxHash], Int, Int)
traverseAndExtract height pos ntx flags hashes
    | null flags = Nothing
    | height == 0 || not match = leafResult
    | isNothing leftM = Nothing
    | (pos * 2 + 1) >= calcTreeWidth ntx (height - 1) =
        Just (hash2 lh lh, lm, lcf + 1, lch)
    | isNothing rightM = Nothing
    | otherwise =
        Just (hash2 lh rh, lm ++ rm, lcf + rcf + 1, lch + rch)
  where
    leafResult
        | null hashes = Nothing
        | otherwise = Just (h, [TxHash h | height == 0 && match], 1, 1)
    (match : fs) = flags
    (h : _) = hashes
    leftM = traverseAndExtract (height - 1) (pos * 2) ntx fs hashes
    (lh, lm, lcf, lch) = fromMaybe e leftM
    rightM =
        traverseAndExtract
            (height - 1)
            (pos * 2 + 1)
            ntx
            (drop lcf fs)
            (drop lch hashes)
    (rh, rm, rcf, rch) = fromMaybe e rightM
    e = error "traverseAndExtract: unexpected error extracting a Maybe value"


-- | Extracts the matching hashes from a partial merkle tree. This will return
-- the list of transaction hashes that have been included (set to true) in
-- a call to 'buildPartialMerkle'.
extractMatches ::
    Network ->
    FlagBits ->
    PartialMerkleTree ->
    -- | number of transaction at height 0 (leaf nodes)
    Int ->
    -- | Merkle root and list of matching transaction hashes
    Either String (MerkleRoot, [TxHash])
extractMatches net flags hashes ntx
    | ntx == 0 =
        Left
            "extractMatches: number of transactions can not be 0"
    | ntx > getMaxBlockSize net `div` 60 =
        Left
            "extractMatches: number of transactions excessively high"
    | length hashes > ntx =
        Left
            "extractMatches: More hashes provided than the number of transactions"
    | length flags < length hashes =
        Left
            "extractMatches: At least one bit per node and one bit per hash"
    | isNothing resM =
        Left
            "extractMatches: traverseAndExtract failed"
    | (nBitsUsed + 7) `div` 8 /= (length flags + 7) `div` 8 =
        Left
            "extractMatches: All bits were not consumed"
    | nHashUsed /= length hashes =
        Left $
            "extractMatches: All hashes were not consumed: " ++ show nHashUsed
    | otherwise = return (merkRoot, matches)
  where
    resM = traverseAndExtract (calcTreeHeight ntx) 0 ntx flags hashes
    (merkRoot, matches, nBitsUsed, nHashUsed) = fromMaybe e resM
    e = error "extractMatches: unexpected error extracting a Maybe value"


-- | Helper function to split a list in chunks 'Int' length. Last chunk may be
-- smaller.
splitIn :: Int -> [a] -> [[a]]
splitIn _ [] = []
splitIn c xs = xs1 : splitIn c xs2
  where
    (xs1, xs2) = splitAt c xs


-- | Pack up to eight bools in a byte.
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs = foldl setBit 0 (map snd $ filter fst $ zip xs [0 .. 7])


-- | Get matching transactions from Merkle block.
merkleBlockTxs :: Network -> MerkleBlock -> Either String [TxHash]
merkleBlockTxs net b =
    let flags = mFlags b
        hs = mHashes b
        n = fromIntegral $ merkleTotalTxns b
        merkle = merkleRoot $ merkleHeader b
     in do
            (root, ths) <- extractMatches net flags hs n
            when (root /= merkle) $ Left "merkleBlockTxs: Merkle root incorrect"
            return ths


-- | Check if Merkle block root is valid against the block header.
testMerkleRoot :: Network -> MerkleBlock -> Bool
testMerkleRoot net = isRight . merkleBlockTxs net
