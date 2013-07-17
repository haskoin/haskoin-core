module Bitcoin.BlockStore
( BlockStore(..)
) where

import Control.Monad
import Control.Monad.IO.Class

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.BlockChain.BlockIndex

import qualified Data.Conduit as C

class MonadIO m => BlockStore m where
    blockStoreGet    :: Word256 -> m (Maybe BlockIndex)
    blockStorePut    :: BlockIndex -> m ()
    blockStoreStream :: C.Source m BlockIndex
    blockStoreRun    :: m a -> IO a



