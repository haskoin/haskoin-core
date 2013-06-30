module Bitcoin.Util
( toStrictBS
, toLazyBS
, stringToBS
, bsToString
, bscToBS
, bsToBSC
, partitionM
, conditionM
, conditionM_
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

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f xs = foldM go ([],[]) xs
    where go (a, b) x = do
              flag <- f x
              return $ if flag then (x:a,b) else (a,x:b)

conditionM :: Monad m => m Bool -> m a -> m a -> m a
conditionM condition a b = go =<< condition
    where go True  = a
          go False = b

conditionM_ :: Monad m => m Bool -> m () -> m () -> m ()
conditionM_ condition a b = go =<< condition
    where go True  = a
          go False = b

