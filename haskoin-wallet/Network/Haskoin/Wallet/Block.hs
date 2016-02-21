module Network.Haskoin.Wallet.Block where

import           Control.Exception                  (throw)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.Trans                (MonadIO)
import           Data.Maybe                         (fromMaybe)
import           Database.Persist.Sql               (SqlPersistT)
import           Network.Haskoin.Block
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Wallet.Model
import           Network.Haskoin.Wallet.Transaction
import           Network.Haskoin.Wallet.Types

mainChain :: (MonadIO m, MonadThrow m)
          => Either BlockHeight BlockHash
          -> ListRequest
          -> SqlPersistT m (ListResult NodeBlock)
mainChain blockE ListRequest{..} = do
    bestHash <- fst <$> walletBestBlock
    bestNodeM <- getBlockByHash bestHash
    bestNode <-
        maybe (error "Could not find wallet best block") return bestNodeM
    remoteNode <- case blockE of
        Right h -> do
            remoteNodeM <- getBlockByHash h
            maybe (throwM $ WalletException "Colud not get remote node")
                return remoteNodeM
        Left h -> do
            heightNodeM <- getBlockByHeight bestNode h
            maybe (throwM $ WalletException "Could not find bock height")
                return heightNodeM
    let split = fst $ splitBlock bestNode remoteNode
        cnt = nodeBlockHeight bestNode - split
        limit = min listLimit (cnt - listOffset)
        offset =
            if listReverse
            then cnt - listOffset - limit
            else listOffset
    nodes <- getBlocksFromHeight bestNode limit (split + offset)
    return $ ListResult nodes cnt

blockTxs :: [NodeBlock] -> [WalletTx] -> [JsonBlock]
blockTxs blocks transactions = reverse $ go [] blocks transactions
  where
    go bs [] _ = bs
    go bs (n : ns) [] = go (toJsonBlock n [] : bs) ns []
    go [] (n : ns) xs = go [toJsonBlock n []] ns xs
    go (b : bs) (n : ns) (x : xs)
       | jsonBlockHash b == blockHashOf x = go
           (b{ jsonBlockTxs = toJsonTx x Nothing : jsonBlockTxs b } : bs)
           (n : ns)
           xs
       | getNodeHash (nodeBlockHash n) == blockHashOf x = go
           (toJsonBlock n [toJsonTx x Nothing] : b : bs)
           ns
           xs
       | otherwise = go
           (toJsonBlock n [] : b : bs)
           ns
           (x : xs)
    blockHashOf t = fromMaybe
        (throw $ WalletException "Unexpected unconfirmed transaction")
        (walletTxConfirmedBy t)
    toJsonBlock NodeBlock{..} ts = JsonBlock
        { jsonBlockHash   = getNodeHash nodeBlockHash
        , jsonBlockHeight = nodeBlockHeight
        , jsonBlockPrev   = getNodeHash nodeBlockPrev
        , jsonBlockTxs    = ts
        }

