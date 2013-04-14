module Bitcoin.Type.Block ( Block(..) ) where

import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Monad

import Bitcoin.Type.BlockHeader
import Bitcoin.Type.Tx
import Bitcoin.Type.VarInt
import qualified Bitcoin.Type as Bitcoin

data Block = Block {
    blockHeader :: BlockHeader,
    blockTxns   :: [Tx]
} deriving (Read, Show)

instance Bitcoin.Type Block where
    get = Block <$> Bitcoin.get 
                <*> (getTxns =<< Bitcoin.get)
        where getTxns (VarInt c) = replicateM (fromIntegral c) Bitcoin.get

    put (Block h xs) = do
        Bitcoin.put h
        Bitcoin.put $ (VarInt . fromIntegral . length) xs
        forM_ xs Bitcoin.put
        
