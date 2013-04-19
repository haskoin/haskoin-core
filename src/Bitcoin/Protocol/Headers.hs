module Bitcoin.Protocol.Headers 
( Headers(..)
, BlockHeaderCount
) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.BlockHeader

type BlockHeaderCount = (BlockHeader, VarInt)

data Headers = Headers {
    headersList :: [BlockHeaderCount]
} deriving (Eq, Read, Show)

instance BitcoinProtocol Headers where

    bitcoinGet = Headers <$> (readList =<< bitcoinGet)
        where readList (VarInt c) = replicateM (fromIntegral c) action
              action = liftM2 (,) bitcoinGet bitcoinGet

    bitcoinPut (Headers xs) = do
        bitcoinPut $ lengthFromList xs
        forM_ xs $ \(a,b) -> bitcoinPut a >> bitcoinPut b

