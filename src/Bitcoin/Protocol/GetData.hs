module Bitcoin.Protocol.GetData ( GetData(..) ) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.InvVector
import Bitcoin.Protocol.VarInt

data GetData = GetData {
   getDataList :: [InvVector] 
} deriving (Eq, Read, Show)

instance BitcoinProtocol GetData where

    bitcoinGet = GetData <$> (readList =<< bitcoinGet)
        where readList (VarInt c) = replicateM (fromIntegral c) bitcoinGet

    bitcoinPut (GetData xs) = do
        bitcoinPut $ lengthFromList xs
        forM_ xs bitcoinPut

