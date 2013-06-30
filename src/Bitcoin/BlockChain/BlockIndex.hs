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
    { biHash      :: Word256
    , biPrev      :: Word256
    , biHeight    :: Word32
    , biTx        :: Word32
    , biChainWork :: Word256
    , biChainTx   :: Word32
    , biFile      :: Word32
    , biDataPos   :: Word32
    , biUndoPos   :: Word32
    , biStatus    :: Word32
    } deriving (Eq, Read, Show)

instance BitcoinProtocol BlockIndex where

    bitcoinGet = BlockIndex <$> getWord256be
                            <*> getWord256be
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord256be
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le

    bitcoinPut (BlockIndex h p he tx cw ct f d u s) = do
        putWord256be h
        putWord256be p
        putWord32le  he
        putWord32le  tx
        putWord256be cw
        putWord32le  ct
        putWord32le  f
        putWord32le  d
        putWord32le  u
        putWord32le  s

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
                    ((biChainWork prevBlockIndex) + 0) -- todo
                    ((biChainTx prevBlockIndex) + (fromIntegral nTxns))
                    (fromIntegral 0)
                    (fromIntegral 0)
                    (fromIntegral 0)
                    (fromIntegral 0)
            Nothing ->
                BlockIndex
                    hash
                    (fromIntegral 0)    -- prev hash
                    (fromIntegral 0)    -- height
                    (fromIntegral nTxns)
                    (fromIntegral 0)    -- chain work
                    (fromIntegral nTxns) -- chain tx
                    (fromIntegral 0)
                    (fromIntegral 0)
                    (fromIntegral 0)
                    (fromIntegral 0)

