module Bitcoin.Protocol.Block 
( Block(..)
, genesisBlock
, genesisBlockHash
, blockHash
) where

import Control.Monad
import Control.Applicative

import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.BlockHeader
import Bitcoin.Protocol.Script

import qualified Data.ByteString as BS

data Block = Block {
    blockHeader     :: BlockHeader,
    blockCoinbaseTx :: CoinbaseTx,
    blockTxns       :: [Tx]
} deriving (Eq, Read, Show)

instance BitcoinProtocol Block where

    bitcoinGet = do
        head       <- bitcoinGet
        (VarInt c) <- bitcoinGet
        cb         <- bitcoinGet
        txs        <- replicateM (fromIntegral (c-1)) bitcoinGet
        return $ Block head cb txs

    bitcoinPut (Block h cb txs) = do
        bitcoinPut h
        bitcoinPut $ VarInt $ fromIntegral $ (length txs) + 1
        bitcoinPut cb
        forM_ txs bitcoinPut

blockHash :: Block -> Word256
blockHash = blockHeaderHash . blockHeader

genesisMessage :: String
genesisMessage = 
    "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks";

genesisMerkle :: Word256
genesisMerkle = 
    fromIntegral 
        0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a


genesisBlockHash :: Word256
genesisBlockHash = 
    fromIntegral
        0x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f

genesisBlock :: Block
genesisBlock = 
    Block
        (BlockHeader
            (fromIntegral 0x01)        -- version
            (fromIntegral 0x00)        -- previous block
            genesisMerkle              -- merkle root
            (fromIntegral 1231006505)  -- timestamp
            (fromIntegral 0x1d00ffff)  -- bits 
            (fromIntegral 2083236893)) -- nonce
        (CoinbaseTx
            txCurrentVersion
            (BS.append
                (BS.pack [0x04,0xff,0xff,0x00,0x1d,0x01,0x04,0x45])
                (stringToBS genesisMessage))
            [ TxOut
                (fromIntegral 5000000000)
                (Script
                    [ OP_PUSHDATA $ BS.pack [0x67,0x8a,0xfd,0xb0]
                    , OP_PUBKEY $ 
                        BS.pack [0x55,0x48,0x27,0x19,0x67,0xf1,0xa6,0x71,0x30
                                ,0xb7,0x10,0x5c,0xd6,0xa8,0x28,0xe0,0x39,0x09
                                ,0xa6,0x79,0x62,0xe0,0xea,0x1f,0x61,0xde,0xb6
                                ,0x49,0xf6,0xbc,0x3f,0x4c,0xef,0x38,0xc4,0xf3
                                ,0x55,0x04,0xe5,0x1e,0xc1,0x12,0xde,0x5c,0x38
                                ,0x4d,0xf7,0xba,0x0b,0x8d,0x57,0x8a,0x4c,0x70
                                ,0x2b,0x6b,0xf1,0x1d,0x5f
                                ]
                    , OP_CHECKSIG
                    ])
            ]
            0) --txLockTime
        [] --txns

--        if (fTestNet)
--        {
--            block.nTime = 1296688602;
--            block.nNonce = 414098458;
--        }
        
