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
    bestM <- getBlockByHash bestHash
    best <- maybe (error "Could not find wallet best block") return bestM
    remoteNode <- case blockE of
        Right h -> do
            remoteNodeM <- getBlockByHash h
            maybe (throwM $ WalletException "Colud not get remote node")
                return remoteNodeM
        Left h -> do
            heightNodeM <- getBlockByHeight best h
            maybe (throwM $ WalletException "Could not find bock height")
                return heightNodeM
    frst <- (+1) . nodeBlockHeight <$> splitBlock best remoteNode
    let cnt = nodeBlockHeight best - frst
        limit = min listLimit (cnt - listOffset)
        offset =
            if listReverse
            then cnt - listOffset - limit
            else listOffset
    nodes <- getBlocksFromHeight best limit (frst + offset)
    return $ ListResult nodes cnt

blockTxs :: [NodeBlock] -> [WalletTx] -> [(NodeBlock, [WalletTx])]
blockTxs blocks transactions = reverse $ go [] blocks transactions
  where
    go bs [] _ = bs
    go bs (n:ns) [] = go ((n,[]):bs) ns []
    go [] (n:ns) xs = go [(n,[])] ns xs
    go (b:bs) (n:ns) (x:xs)
       | nodeHash (fst b) == blockHashOf x =
           go ((fst b, x : snd b) : bs) (n:ns) xs
       | nodeHash n == blockHashOf x =
           go ((n, [x]) : b : bs) ns xs
       | otherwise = go ((n, []) : b : bs) ns (x:xs)
    blockHashOf t = fromMaybe
        (throw $ WalletException "Unexpected unconfirmed transaction")
        (walletTxConfirmedBy t)
