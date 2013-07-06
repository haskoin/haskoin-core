module Bitcoin.Util
( toStrictBS
, toLazyBS
, stringToBS
, bsToString
, bscToBS
, bsToBSC
, partitionM
) where

import Data.Char

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

stringToBS :: String -> BS.ByteString
stringToBS s = BS.pack $ map (fromIntegral . ord) s

bsToString :: BS.ByteString -> String
bsToString bs = map (chr . fromIntegral) (BS.unpack bs)

bscToBS :: BSC.ByteString -> BS.ByteString
bscToBS = stringToBS . BSC.unpack

bsToBSC :: BS.ByteString -> BSC.ByteString
bsToBSC = BSC.pack . bsToString

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f d [] = return d
foldrM f d (x:xs) = (f x) =<< foldrM f d xs

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f xs = foldrM go ([],[]) xs
    where go x (a, b) = do
              flag <- f x
              return $ if flag then (x:a,b) else (a,x:b)

