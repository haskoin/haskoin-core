module Bitcoin.BlockChain.BlockIndex
( BlockIndex(..)
, buildBlockIndex
) where

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.Util

import Control.Monad
import Control.Applicative

data BlockIndex = BlockIndex 
    { biHash      :: !Word256
    , biPrev      :: !Word256
    , biHeight    :: !Word32
    , biTx        :: !Word32
    } deriving (Eq, Read, Show)

instance BitcoinProtocol BlockIndex where

    bitcoinGet = BlockIndex <$> getWord256be
                            <*> getWord256be
                            <*> getWord32le
                            <*> getWord32le

    bitcoinPut (BlockIndex h p he tx) = do
        putWord256be h
        putWord256be p
        putWord32le  he
        putWord32le  tx

buildBlockIndex :: Block -> (Maybe BlockIndex) -> BlockIndex
buildBlockIndex block prev = 
    let nTxns = (length (blockTxns block)) + 1
        hash = blockHash block
        in case prev of
            (Just prevBlockIndex) -> 
                BlockIndex
                    hash
                    (biHash prevBlockIndex)
                    ((biHeight prevBlockIndex) + 1)
                    (fromIntegral nTxns)
            Nothing ->
                BlockIndex
                    hash
                    (fromIntegral 0)    -- prev hash
                    (fromIntegral 0)    -- height
                    (fromIntegral nTxns)

