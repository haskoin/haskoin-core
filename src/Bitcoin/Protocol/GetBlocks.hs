module Bitcoin.Protocol.GetBlocks 
( GetBlocks(..) 
, requestMaxBlocks
, BlockLocator
) where

import Control.Monad
import Control.Applicative

import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt

type BlockLocator = [Word256]

data GetBlocks = GetBlocks {
    getBlocksVersion :: Word32,
    blockLocatorHash :: BlockLocator,
    hashStop         :: Word256
} deriving (Eq, Read, Show)

instance BitcoinProtocol GetBlocks where

    bitcoinGet = GetBlocks <$> getWord32le
                           <*> (repList =<< bitcoinGet)
                           <*> getWord256be
        where repList (VarInt c) = replicateM (fromIntegral c) getWord256be

    bitcoinPut (GetBlocks v xs h) = do
        putWord32le v
        bitcoinPut $ lengthFromList xs
        forM_ xs putWord256be
        putWord256be h

requestMaxBlocks :: Word256
requestMaxBlocks = fromIntegral 0
                    
