module Bitcoin.Protocol.Inv ( Inv(..) ) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.InvVector
import Bitcoin.Protocol.VarInt

data Inv = Inv {
   invList :: [InvVector] 
} deriving (Eq, Read, Show)

instance BitcoinProtocol Inv where

    bitcoinGet = Inv <$> (repList =<< bitcoinGet)
        where repList (VarInt c) = replicateM (fromIntegral c) bitcoinGet

    bitcoinPut (Inv xs) = do
        bitcoinPut $ lengthFromList xs
        forM_ xs bitcoinPut

