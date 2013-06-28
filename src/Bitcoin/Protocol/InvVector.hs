module Bitcoin.Protocol.InvVector 
( InvVector(..) 
, InvType(..)
) where

import Bitcoin.Protocol
import Control.Applicative

data InvType = InvError | InvTx | InvBlock
    deriving (Eq, Show, Read)

instance BitcoinProtocol InvType where

    bitcoinGet = go =<< getWord32le
        where go x = case x of
                          0 -> return InvError
                          1 -> return InvTx
                          2 -> return InvBlock
                          _ -> error "bitcoinGet InvType: Invalid Type"

    bitcoinPut x = 
        putWord32le $ case x of
                           InvError -> 0
                           InvTx    -> 1
                           InvBlock -> 2

data InvVector = InvVector {
    invType :: InvType,
    invHash :: Word256
} deriving (Eq, Show, Read)

instance BitcoinProtocol InvVector where

    bitcoinGet = InvVector <$> bitcoinGet
                           <*> getWord256be

    bitcoinPut (InvVector t h) = do
        bitcoinPut t
        putWord256be h


