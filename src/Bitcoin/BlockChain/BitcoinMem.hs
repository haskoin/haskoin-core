module Bitcoin.BlockChain.BitcoinMem
( BitcoinMem
, MemState(..)
, withOrphanMap
, withIndexMap
, getBestIndex
, saveBlock
, newMemState
, alreadyHave
) where

import Data.Maybe

import Control.Concurrent.STM

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Resource

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.GetBlocks
import Bitcoin.Protocol.BlockHeader
import Bitcoin.BlockChain.BlockIndex

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

import Bitcoin.Store
import Bitcoin.Store.STM

type BitcoinMem = ReaderT MemState STM

data MemState = MemState
    { stateBlockIndex   :: TVar (STMState BlockIndex)
    , stateOrphanBlocks :: TVar (STMState Block)
    } 

newMemState :: IO MemState
newMemState = liftM2 MemState newSTMState newSTMState

withOrphanMap :: Mem Block a -> BitcoinMem a
withOrphanMap m = do
    tvar <- stateOrphanBlocks <$> ask 
    lift $ runReaderT (runMem m) tvar

withIndexMap :: Mem BlockIndex a -> BitcoinMem a
withIndexMap m = do
    tvar <- stateBlockIndex <$> ask 
    lift $ runReaderT (runMem m) tvar 

getBestIndex :: BitcoinMem (Maybe BlockIndex)
getBestIndex = withIndexMap dbBest

alreadyHave :: Word256 -> BitcoinMem Bool
alreadyHave w = liftM2 (||) (withOrphanMap $ existsBlock w)
                            (withIndexMap  $ existsIndex w)

toBlockIndex :: Block -> BitcoinMem (Maybe BlockIndex)
toBlockIndex block = withIndexMap $ go =<< dbGet (prevBlock $ blockHeader block)
    where go p@(Just _) = return $ Just $ buildBlockIndex block p   
          go _          = return $ Nothing

saveBlock :: Block -> BitcoinMem [BlockIndex]
saveBlock block = do
    have <- alreadyHave (blockHash block)
    if have then return []
            else go =<< toBlockIndex block
    where go (Just bi) = do
              rehomed <- withOrphanMap $ getAllChildrenOf block
              bis <- catMaybes <$> mapM toBlockIndex rehomed
              withOrphanMap $ forM_ bis (delBlock . biHash)
              withIndexMap  $ forM_ (bi:bis) dbPut 
              return $ bi:bis
          go Nothing = do
              withOrphanMap $ dbPut block
              return []

