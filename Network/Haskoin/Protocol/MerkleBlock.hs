module Network.Haskoin.Protocol.MerkleBlock (MerkleBlock(..)) where

import Control.Monad (replicateM, forM_)

import Data.Word (Word8, Word32)
import Data.Bits (testBit, setBit)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord8, getWord32le)
import Data.Binary.Put (putWord8, putWord32le)

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Protocol.BlockHeader
import Network.Haskoin.Crypto (Hash256)

data MerkleBlock = 
    MerkleBlock {
                -- | Header information for this merkle block.
                  merkleHeader :: !BlockHeader
                -- | Number of transactions in the block (including
                -- unmatched transactions).
                , merkleTotalTxns :: !Word32
                  -- | Hashes in depth-first order. They are used to rebuild a
                  -- partial merkle tree.
                , mHashes     :: [Hash256]
                  -- | Flag bits, packed per 8 in a byte. Least significant bit
                  -- first. Flag bits are used to rebuild a partial merkle
                  -- tree.
                , mFlags      :: [Bool]
                } deriving (Eq, Show)

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
    
splitIn :: Int -> [a] -> [[a]]
splitIn _ [] = []
splitIn c xs = take c xs : (splitIn c $ drop c xs)
 
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs = foldl setBit 0 (map snd $ filter fst $ zip xs [0..7])

