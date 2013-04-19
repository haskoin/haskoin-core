module Bitcoin.Protocol.InvVector 
( InvVector(..) 
, InvType(..)
) where

import Bitcoin.Protocol
import Control.Applicative

data InvType = Error | Tx | Block
    deriving (Show, Read)

instance BitcoinProtocol InvType where

    bitcoinGet = go =<< getWord32le
        where go x = case x of
                          0 -> return Error
                          1 -> return Tx
                          2 -> return Block

    bitcoinPut x = 
        putWord32le $ case x of
                           Error -> 0
                           Tx    -> 1
                           Block -> 2

data InvVector = InvVector {
    invType :: InvType,
    invHash :: Word256
} deriving (Show, Read)

instance BitcoinProtocol InvVector where

    bitcoinGet = InvVector <$> bitcoinGet
                           <*> getWord256be

    bitcoinPut (InvVector t h) = do
        bitcoinPut t
        putWord256be h


