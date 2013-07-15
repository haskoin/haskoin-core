module Bitcoin.BlockStore
( BlockStore(..)
) where

import Control.Monad
import Control.Monad.IO.Class

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.BlockChain.BlockIndex

class MonadIO m => BlockStore m where
    blockStoreGet :: Word256 -> m (Maybe BlockIndex)
    blockStorePut :: BlockIndex -> m ()
    runDB         :: m a -> IO a



