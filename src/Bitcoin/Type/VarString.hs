module Bitcoin.Type.VarString ( VarString(..) ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Bitcoin.Type.VarInt
import qualified Bitcoin.Type.UInt as B

newtype VarString = VarString [Char]
    deriving (Eq, Show, Read)

instance Binary VarString where

    get = do
        size <- (get :: Get VarInt)
        chars <- sequence $ replicate (fromIntegral size) (get :: Get Char)
        return $ VarString chars

    put (VarString x) = do
        let size = B.UInt64 . fromIntegral . length $ x
        put $ VarInt size
        putChars x

putChars :: [Char] -> Put 
putChars [] = return ()
putChars (x:xs) = put x >> putChars xs


