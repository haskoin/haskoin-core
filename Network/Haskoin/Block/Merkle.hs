module Network.Haskoin.Block.Merkle 
( MerkleBlock(..)
, calcTreeHeight
, calcTreeWidth
, buildMerkleRoot
, calcHash
, buildPartialMerkle
, extractMatches
) where

import Control.Monad (forM_, replicateM)
import Control.DeepSeq (NFData, rnf)

import Data.Bits
import Data.Maybe
import Data.Word (Word8, Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord8, getWord32le)
import Data.Binary.Put (putWord8, putWord32le)
import qualified Data.ByteString as BS 

import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Block.Types
import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Protocol.Types

type MerkleRoot        = Word256
type FlagBits          = [Bool]
type PartialMerkleTree = [Word256]

data MerkleBlock = 
    MerkleBlock {
                -- | Header information for this merkle block.
                  merkleHeader :: !BlockHeader
                -- | Number of transactions in the block (including
                -- unmatched transactions).
                , merkleTotalTxns :: !Word32
                -- | Hashes in depth-first order. They are used to rebuild a
                -- partial merkle tree.
                , mHashes :: ![Word256]
                -- | Flag bits, packed per 8 in a byte. Least significant bit
                -- first. Flag bits are used to rebuild a partial merkle
                -- tree.
                , mFlags :: ![Bool]
                } deriving (Eq, Show, Read)

instance NFData MerkleBlock where
    rnf (MerkleBlock m t h f) = rnf m `seq` rnf t `seq` rnf h `seq` rnf f

instance Binary MerkleBlock where

    get = do
        header <- get
        ntx    <- getWord32le
        (VarInt matchLen) <- get
        hashes <- replicateM (fromIntegral matchLen) get
        (VarInt flagLen)  <- get
        ws <- replicateM (fromIntegral flagLen) getWord8
        return $ MerkleBlock header ntx hashes (decodeMerkleFlags ws)

    put (MerkleBlock h ntx hashes flags) = do
        put h
        putWord32le ntx
        put $ VarInt $ fromIntegral $ length hashes
        forM_ hashes put
        let ws = encodeMerkleFlags flags
        put $ VarInt $ fromIntegral $ length ws
        forM_ ws putWord8

decodeMerkleFlags :: [Word8] -> [Bool]
decodeMerkleFlags ws = 
    [ b | p <- [0..(length ws)*8-1]
    , b <- [testBit (ws !! (p `div` 8)) (p `mod` 8)]
    ]

encodeMerkleFlags :: [Bool] -> [Word8]
encodeMerkleFlags bs = map boolsToWord8 $ splitIn 8 bs

-- | Computes the height of a merkle tree.
calcTreeHeight :: Int -- ^ Number of transactions (leaf nodes).
               -> Int -- ^ Height of the merkle tree.
calcTreeHeight ntx | ntx < 2 = 0
                   | even ntx  = 1 + calcTreeHeight (ntx `div` 2)
                   | otherwise = calcTreeHeight $ ntx + 1

-- | Computes the width of a merkle tree at a specific height. The transactions
-- are at height 0.
calcTreeWidth :: Int -- ^ Number of transactions (leaf nodes).
              -> Int -- ^ Height at which we want to compute the width.
              -> Int -- ^ Width of the merkle tree.
calcTreeWidth ntx h = (ntx + (1 `shiftL` h) - 1) `shiftR` h

-- | Computes the root of a merkle tree from a list of leaf node hashes.
buildMerkleRoot :: [TxHash]   -- ^ List of transaction hashes (leaf nodes).
                -> MerkleRoot -- ^ Root of the merkle tree.
buildMerkleRoot txs = calcHash (calcTreeHeight $ length txs) 0 txs

hash2 :: Word256 -> Word256 -> Word256
hash2 a b = doubleHash256 $ encode' a `BS.append` encode' b

-- | Computes the hash of a specific node in a merkle tree.
calcHash :: Int       -- ^ Height of the node in the merkle tree.
         -> Int       -- ^ Position of the node (0 for the leftmost node).
         -> [TxHash]  -- ^ Transaction hashes of the merkle tree (leaf nodes).
         -> Word256   -- ^ Hash of the node at the specified position.
calcHash height pos txs
    | height < 0 || pos < 0 = error "calcHash: Invalid parameters"
    | height == 0 = fromIntegral $ txs !! pos
    | otherwise = hash2 left right
  where
    left = calcHash (height-1) (pos*2) txs
    right | pos*2+1 < calcTreeWidth (length txs) (height-1) = 
                calcHash (height-1) (pos*2+1) txs
          | otherwise = left

-- | Build a partial merkle tree.
buildPartialMerkle 
    :: [(TxHash,Bool)] 
    -- ^ List of transactions hashes forming the leaves of the merkle tree
    -- and a bool indicating if that transaction should be included in the 
    -- partial merkle tree.
    -> (FlagBits, PartialMerkleTree) 
    -- ^ Flag bits (used to parse the partial merkle tree) and the 
    -- partial merkle tree.
buildPartialMerkle hs = traverseAndBuild (calcTreeHeight $ length hs) 0 hs

traverseAndBuild :: Int -> Int -> [(TxHash,Bool)] 
                 -> (FlagBits, PartialMerkleTree)
traverseAndBuild height pos txs
    | height < 0 || pos < 0 = error "traverseAndBuild: Invalid parameters"
    | height == 0 || not match = ([match],[calcHash height pos t])
    | otherwise = (match : lb ++ rb, lh ++ rh)
  where
    t = map fst txs
    s = pos `shiftL` height
    e = min (length txs) $ (pos+1) `shiftL` height
    match = or $ map snd $ take (e-s) $ drop s txs
    (lb,lh) = traverseAndBuild (height-1) (pos*2) txs
    (rb,rh) | (pos*2+1) < calcTreeWidth (length txs) (height-1)
                = traverseAndBuild (height-1) (pos*2+1) txs
            | otherwise = ([],[])

traverseAndExtract :: Int -> Int -> Int -> FlagBits -> PartialMerkleTree
                   -> Maybe (MerkleRoot, [TxHash], Int, Int)
traverseAndExtract height pos ntx flags hashes
    | length flags == 0        = Nothing
    | height == 0 || not match = leafResult
    | isNothing leftM          = Nothing
    | (pos*2+1) >= calcTreeWidth ntx (height-1) = 
        Just (hash2 lh lh, lm, lcf+1, lch)
    | isNothing rightM         = Nothing
    | otherwise = 
        Just (hash2 lh rh, lm ++ rm, lcf+rcf+1, lch+rch)
  where
    leafResult
        | null hashes = Nothing
        | otherwise = Just 
            (h,if height == 0 && match then [fromIntegral h] else [],1,1)
    (match:fs) = flags
    (h:_)     = hashes
    leftM  = traverseAndExtract (height-1) (pos*2) ntx fs hashes
    (lh,lm,lcf,lch) = fromJust leftM
    rightM = traverseAndExtract (height-1) (pos*2+1) ntx 
                (drop lcf fs) (drop lch hashes)
    (rh,rm,rcf,rch) = fromJust rightM

-- | Extracts the matching hashes from a partial merkle tree. This will return
-- the list of transaction hashes that have been included (set to True) in
-- a call to 'buildPartialMerkle'.
extractMatches :: FlagBits -- ^ Flag bits (produced by buildPartialMerkle).
               -> PartialMerkleTree -- ^ Partial merkle tree.
               -> Int -- ^ Number of transaction at height 0 (leaf nodes).
               -> Either String (MerkleRoot, [TxHash])
               -- ^ Merkle root and the list of matching transaction hashes.
extractMatches flags hashes ntx
    | ntx == 0 = Left $
        "extractMatches: number of transactions can not be 0"
    | ntx > maxBlockSize `div` 60 = Left $
        "extractMatches: number of transactions excessively high"
    | length hashes > ntx = Left $
        "extractMatches: More hashes provided than the number of transactions"
    | length flags < length hashes = Left $
        "extractMatches: At least one bit per node and one bit per hash"
    | isNothing resM = Left $
        "extractMatches: traverseAndExtract failed"
    | (nBitsUsed+7) `div` 8 /= (length flags+7) `div` 8 = Left $
        "extractMatches: All bits were not consumed"
    | nHashUsed /= length hashes = Left $
        "extractMatches: All hashes were not consumed: " ++ (show nHashUsed)
    | otherwise = return (merkRoot, matches)
  where
    resM = traverseAndExtract (calcTreeHeight ntx) 0 ntx flags hashes
    (merkRoot, matches, nBitsUsed, nHashUsed) = fromJust resM

splitIn :: Int -> [a] -> [[a]]
splitIn _ [] = []
splitIn c xs = take c xs : (splitIn c $ drop c xs)
 
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs = foldl setBit 0 (map snd $ filter fst $ zip xs [0..7])

