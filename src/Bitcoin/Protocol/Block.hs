module Bitcoin.Protocol.Block ( Block(..) ) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.BlockHeader

data Block = Block {
    blockHeader :: BlockHeader,
    blockTxns   :: [Tx]
} deriving (Eq, Read, Show)

instance BitcoinProtocol Block where

    bitcoinGet = Block <$> bitcoinGet
                       <*> (readList =<< bitcoinGet)
        where readList (VarInt c) = replicateM (fromIntegral c) bitcoinGet

    bitcoinPut (Block h xs) = do
        bitcoinPut h
        bitcoinPut $ lengthFromList xs
        forM_ xs bitcoinPut
        
