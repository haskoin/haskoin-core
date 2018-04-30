{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Network.Haskoin.Wallet.Server.Handler where

import           Control.Arrow                      (first)
import           Control.Concurrent.STM.TBMChan     (TBMChan)
import           Control.Exception                  (SomeException (..),
                                                     tryJust)
import           Control.Monad                      (liftM, forM, unless, when)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.IO.Unlift            (MonadUnliftIO)
import           Control.Monad.Logger               (MonadLoggerIO, logError,
                                                     logInfo)
import           Control.Monad.Reader               (ReaderT, asks, runReaderT)
import           Control.Monad.Trans                (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Resource       (MonadResource)
import           Data.Aeson                         (Value (..), toJSON)
import qualified Data.Map.Strict                    as M (elems, fromList,
                                                          intersectionWith)
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text, pack, unpack)
import           Data.Word                          (Word32)
import           Data.Maybe                         (catMaybes)
import           Database.Esqueleto                 (Entity (..), SqlPersistT)
import           Database.Persist.Sql               (ConnectionPool,
                                                     SqlPersistM,
                                                     runSqlPersistMPool,
                                                     runSqlPool)
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.BlockChain
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Node.Peer
import           Network.Haskoin.Node.STM
import           Network.Haskoin.Transaction
import           Network.Haskoin.Wallet.Accounts
import           Network.Haskoin.Wallet.Block
import           Network.Haskoin.Wallet.Model
import           Network.Haskoin.Wallet.Settings
import           Network.Haskoin.Wallet.Transaction
import           Network.Haskoin.Wallet.Types
import           Network.Haskoin.Wallet.Types.BlockInfo (fromNodeBlock)

type Handler m = ReaderT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig    :: !Config
    , handlerPool      :: !ConnectionPool
    , handlerNodeState :: !(Maybe SharedNodeState)
    , handlerNotifChan :: !(TBMChan Notif)
    }

runHandler :: Monad m => Handler m a -> HandlerSession -> m a
runHandler = runReaderT

runDB :: MonadUnliftIO m => SqlPersistT m a -> Handler m a
runDB action = asks handlerPool >>= lift . runDBPool action

runDBPool :: MonadUnliftIO m => SqlPersistT m a -> ConnectionPool -> m a
runDBPool = runSqlPool

tryDBPool :: MonadLoggerIO m => ConnectionPool -> SqlPersistM a -> m (Maybe a)
tryDBPool pool action = do
    resE <- liftIO $ tryJust f $ runSqlPersistMPool action pool
    case resE of
        Right res -> return $ Just res
        Left err -> do
            $(logError) $ pack $ unwords [ "A database error occured:", err]
            return Nothing
  where
    f (SomeException e) = Just $ show e

runNode :: MonadIO m => NodeT m a -> Handler m a
runNode action = do
    nodeStateM <- asks handlerNodeState
    case nodeStateM of
        Just nodeState -> lift $ runNodeT action nodeState
        _ -> error "runNode: No node state available"

{- Server Handlers -}

accountReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
           => AccountName -> Handler m (Maybe Value)
accountReq name = do
    $(logInfo) $ format $ unlines
        [ "Account"
        , "  Account name: " ++ unpack name
        ]
    Entity _ acc <- runDB $ getAccount name
    return $ Just $ toJSON $ toJsonAccount Nothing acc

accountsReq :: ( MonadLoggerIO m
               , MonadUnliftIO m
               , MonadThrow m
               , MonadResource m
               )
             => ListRequest
             -> Handler m (Maybe Value)
accountsReq lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "Accounts"
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]
    (accs, cnt) <- runDB $ accounts lq
    return $ Just $ toJSON $ ListResult (map (toJsonAccount Nothing) accs) cnt

newAccountReq
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadUnliftIO m)
    => NewAccount -> Handler m (Maybe Value)
newAccountReq newAcc@NewAccount{..} = do
    $(logInfo) $ format $ unlines
        [ "NewAccount"
        , "  Account name: " ++ unpack newAccountName
        , "  Account type: " ++ show newAccountType
        ]
    (Entity _ newAcc', mnemonicM) <- runDB $ newAccount newAcc
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc') updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount mnemonicM newAcc'

renameAccountReq
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadUnliftIO m)
    => AccountName -> AccountName -> Handler m (Maybe Value)
renameAccountReq oldName newName = do
    $(logInfo) $ format $ unlines
        [ "RenameAccount"
        , "  Account name: " ++ unpack oldName
        , "  New name    : " ++ unpack newName
        ]
    newAcc <- runDB $ do
        accE <- getAccount oldName
        renameAccount accE newName
    return $ Just $ toJSON $ toJsonAccount Nothing newAcc

addPubKeysReq
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadUnliftIO m)
    => AccountName -> [XPubKey] -> Handler m (Maybe Value)
addPubKeysReq name keys = do
    $(logInfo) $ format $ unlines
        [ "AddPubKeys"
        , "  Account name: " ++ unpack name
        , "  Key count   : " ++ show (length keys)
        ]
    newAcc <- runDB $ do
        accE <- getAccount name
        addAccountKeys accE keys
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc) updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount Nothing newAcc

setAccountGapReq :: ( MonadLoggerIO m
                    , MonadUnliftIO m
                    , MonadThrow m
                    , MonadResource m
                    )
                 => AccountName -> Word32
                 -> Handler m (Maybe Value)
setAccountGapReq name gap = do
    $(logInfo) $ format $ unlines
        [ "SetAccountGap"
        , "  Account name: " ++ unpack name
        , "  New gap size: " ++ show gap
        ]
    -- Update the gap
    Entity _ newAcc <- runDB $ do
        accE <- getAccount name
        setAccountGap accE gap
    -- Update the bloom filter
    whenOnline updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount Nothing newAcc

addrsReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
         => AccountName
         -> AddressType
         -> Word32
         -> Bool
         -> ListRequest
         -> Handler m (Maybe Value)
addrsReq name addrType minConf offline listReq = do
    $(logInfo) $ format $ unlines
        [ "Addresses"
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        , "  Start index : " ++ show (listOffset listReq)
        , "  Reversed    : " ++ show (listReverse listReq)
        , "  MinConf     : " ++ show minConf
        , "  Offline     : " ++ show offline
        ]

    (res, bals, cnt) <- runDB $ do
        accE <- getAccount name
        (res, cnt) <- addressList accE addrType listReq
        case res of
            [] -> return (res, [], cnt)
            _ -> do
                let is = map walletAddrIndex res
                    (iMin, iMax) = (minimum is, maximum is)
                bals <- addressBalances accE iMin iMax addrType minConf offline
                return (res, bals, cnt)

    -- Join addresses and balances together
    let g (addr, bal) = toJsonAddr addr (Just bal)
        addrBals = map g $ M.elems $ joinAddrs res bals
    return $ Just $ toJSON $ ListResult addrBals cnt
  where
    joinAddrs addrs bals =
        let f addr = (walletAddrIndex addr, addr)
        in  M.intersectionWith (,) (M.fromList $ map f addrs) (M.fromList bals)

unusedAddrsReq
    :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
    => AccountName -> AddressType -> ListRequest -> Handler m (Maybe Value)
unusedAddrsReq name addrType lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "UnusedAddrs"
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]

    (addrs, cnt) <- runDB $ do
        accE <- getAccount name
        unusedAddresses accE addrType lq

    return $ Just $ toJSON $ ListResult (map (`toJsonAddr` Nothing) addrs) cnt

addressReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
           => AccountName -> KeyIndex -> AddressType
           -> Word32 -> Bool
           -> Handler m (Maybe Value)
addressReq name i addrType minConf offline = do
    $(logInfo) $ format $ unlines
        [ "Address"
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Address type: " ++ show addrType
        ]

    (addr, balM) <- runDB $ do
        accE <- getAccount name
        addrE <- getAddress accE addrType i
        bals <- addressBalances accE i i addrType minConf offline
        return $ case bals of
            ((_,bal):_) -> (entityVal addrE, Just bal)
            _           -> (entityVal addrE, Nothing)
    return $ Just $ toJSON $ toJsonAddr addr balM

-- TODO: How can we generalize this? Perhaps as part of wallet searching?
pubKeyIndexReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
               => AccountName
               -> PubKeyC
               -> AddressType
               -> Handler m (Maybe Value)
pubKeyIndexReq name key addrType = do
    $(logInfo) $ format $ unlines
        [ "PubKeyIndex"
        , "  Account name: " ++ unpack name
        , "  Key         : " ++ show key
        , "  Address type: " ++ show addrType
        ]
    addrLst <- runDB $ do
        accE <- getAccount name
        lookupByPubKey accE key addrType
    -- TODO: We don't return the balance for now. Maybe add it? Or not?
    return $ Just $ toJSON $ map (`toJsonAddr` Nothing) addrLst

setAddrLabelReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
                => AccountName
                -> KeyIndex
                -> AddressType
                -> Text
                -> Handler m (Maybe Value)
setAddrLabelReq name i addrType label = do
    $(logInfo) $ format $ unlines
        [ "SetAddrLabel"
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Label       : " ++ unpack label
        ]

    newAddr <- runDB $ do
        accE <- getAccount name
        setAddrLabel accE i addrType label

    return $ Just $ toJSON $ toJsonAddr newAddr Nothing

generateAddrsReq :: ( MonadLoggerIO m
                    , MonadUnliftIO m
                    , MonadThrow m
                    , MonadResource m
                    )
                 => AccountName
                 -> KeyIndex
                 -> AddressType
                 -> Handler m (Maybe Value)
generateAddrsReq name i addrType = do
    $(logInfo) $ format $ unlines
        [ "GenerateAddrs"
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        ]

    cnt <- runDB $ do
        accE <- getAccount name
        generateAddrs accE addrType i

    -- Update the bloom filter
    whenOnline updateNodeFilter

    return $ Just $ toJSON cnt

-- This is a generic function (see specifics below)
getTxs :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
       => AccountName
       -> ListRequest
       -> String
       -> (AccountId -> ListRequest -> SqlPersistT m ([WalletTx], Word32))
       -> Handler m (Maybe Value)
getTxs name lq@ListRequest{..} cmd f = do
    $(logInfo) $ format $ unlines
        [ cmd
        , "  Account name: " ++ unpack name
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]

    (res, cnt, bb) <- runDB $ do
        Entity ai _ <- getAccount name
        bb <- walletBestBlock
        (res, cnt) <- f ai lq
        return (res, cnt, bb)

    return $ Just $ toJSON $ ListResult (map (g bb) res) cnt
  where
    g bb = toJsonTx name (Just bb)

txsReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
       => AccountName -> ListRequest -> Handler m (Maybe Value)
txsReq name lq = getTxs name lq "Txs" (txs Nothing)

pendingTxsReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
              => AccountName -> ListRequest -> Handler m (Maybe Value)
pendingTxsReq name lq = getTxs name lq "PendingTxs" (txs (Just TxPending))

deadTxsReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
         => AccountName -> ListRequest -> Handler m (Maybe Value)
deadTxsReq name lq = getTxs name lq "DeadTxs" (txs (Just TxDead))

addrTxsReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
           => AccountName -> KeyIndex -> AddressType -> ListRequest
           -> Handler m (Maybe Value)
addrTxsReq name index addrType lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "AddrTxs"
        , "  Account name : " ++ unpack name
        , "  Address index: " ++ show index
        , "  Address type : " ++ show addrType
        , "  Offset       : " ++ show listOffset
        , "  Limit        : " ++ show listLimit
        , "  Reversed     : " ++ show listReverse
        ]

    (res, cnt, bb) <- runDB $ do
        accE <- getAccount name
        addrE <- getAddress accE addrType index
        bb <- walletBestBlock
        (res, cnt) <- addrTxs accE addrE lq
        return (res, cnt, bb)

    return $ Just $ toJSON $ ListResult (map (f bb) res) cnt
  where
    f bb = toJsonTx name (Just bb)

createTxReq :: ( MonadLoggerIO m, MonadUnliftIO m
               , MonadThrow m, MonadResource m
               )
            => AccountName
            -> CreateTx
            -> Handler m (Maybe Value)
createTxReq name (CreateTx rs fee minconf rcptFee sign masterM) = do
    $(logInfo) $ format $ unlines
        [ "CreateTx"
        , "  Account name: " ++ unpack name
        , "  Recipients  : " ++ show (map (first addrToBase58) rs)
        , "  Fee         : " ++ show fee
        , "  Minconf     : " ++ show minconf
        , "  Rcpt. Fee   : " ++ show rcptFee
        , "  Sign        : " ++ show sign
        ]

    notif <- asks handlerNotifChan

    (bb, txRes, newAddrs) <- runDB $ do
        accE <- getAccount name
        bb   <- walletBestBlock
        (txRes, newAddrs) <- createWalletTx
            accE (Just notif) masterM rs fee minconf rcptFee sign
        return (bb, txRes, newAddrs)

    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (walletTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [walletTxHash txRes]
    return $ Just $ toJSON $ toJsonTx name (Just bb) txRes

importTxReq :: ( MonadLoggerIO m, MonadUnliftIO m
               , MonadThrow m, MonadResource m
               )
            => AccountName -> Tx -> Handler m (Maybe Value)
importTxReq name tx = do
    $(logInfo) $ format $ unlines
        [ "ImportTx"
        , "  Account name: " ++ unpack name
        , "  TxId        : " ++ cs (txHashToHex (txHash tx))
        ]

    notif <- asks handlerNotifChan

    (bb, txRes, newAddrs) <- runDB $ do
        Entity ai _ <- getAccount name
        bb <- walletBestBlock
        (res, newAddrs) <- importTx tx (Just notif) ai
        case filter ((== ai) . walletTxAccount) res of
            (txRes:_) -> return (bb, txRes, newAddrs)
            _ -> throwM $ WalletException "Could not import the transaction"

    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (walletTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [walletTxHash txRes]
    return $ Just $ toJSON $ toJsonTx name (Just bb) txRes

signTxReq :: ( MonadLoggerIO m, MonadUnliftIO m
             , MonadThrow m, MonadResource m
             )
          => AccountName -> SignTx -> Handler m (Maybe Value)
signTxReq name (SignTx txid masterM) = do
    $(logInfo) $ format $ unlines
        [ "SignTx"
        , "  Account name: " ++ unpack name
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]

    notif <- asks handlerNotifChan

    (bb, txRes, newAddrs) <- runDB $ do
        accE@(Entity ai _) <- getAccount name
        bb <- walletBestBlock
        (res, newAddrs) <- signAccountTx accE (Just notif) masterM txid
        case filter ((== ai) . walletTxAccount) res of
            (txRes:_) -> return (bb, txRes, newAddrs)
            _ -> throwM $ WalletException "Could not sign the transaction"

    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (walletTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [walletTxHash txRes]
    return $ Just $ toJSON $ toJsonTx name (Just bb) txRes

txReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
      => AccountName -> TxHash -> Handler m (Maybe Value)
txReq name txid = do
    $(logInfo) $ format $ unlines
        [ "Tx"
        , "  Account name: " ++ unpack name
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]
    (res, bb) <- runDB $ do
        Entity ai _ <- getAccount name
        bb <- walletBestBlock
        res <- getAccountTx ai txid
        return (res, bb)
    return $ Just $ toJSON $ toJsonTx name (Just bb) res

-- TODO: This should be limited to a single account
deleteTxReq :: (MonadLoggerIO m, MonadThrow m, MonadUnliftIO m)
            => TxHash -> Handler m (Maybe Value)
deleteTxReq txid = do
    $(logInfo) $ format $ unlines
        [ "DeleteTx"
        , "  TxId: " ++ cs (txHashToHex txid)
        ]
    runDB $ deleteTx txid
    return Nothing

offlineTxReq :: ( MonadLoggerIO m, MonadUnliftIO m
                , MonadThrow m, MonadResource m
                )
             => AccountName -> TxHash -> Handler m (Maybe Value)
offlineTxReq accountName txid = do
    $(logInfo) $ format $ unlines
        [ "OfflineTx"
        , "  Account name: " ++ unpack accountName
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]
    (dat, _) <- runDB $ do
        Entity ai _ <- getAccount accountName
        getOfflineTxData ai txid
    return $ Just $ toJSON dat

signOfflineTxReq :: ( MonadLoggerIO m, MonadUnliftIO m
                    , MonadThrow m, MonadResource m
                    )
                 => AccountName
                 -> Maybe XPrvKey
                 -> Tx
                 -> [CoinSignData]
                 -> Handler m (Maybe Value)
signOfflineTxReq accountName masterM tx signData = do
    $(logInfo) $ format $ unlines
        [ "SignOfflineTx"
        , "  Account name: " ++ unpack accountName
        , "  TxId        : " ++ cs (txHashToHex (txHash tx))
        ]
    Entity _ acc <- runDB $ getAccount accountName
    let signedTx = signOfflineTx acc masterM tx signData
        complete = verifyStdTx signedTx $ map toDat signData
        toDat CoinSignData{..} = (coinSignScriptOutput, coinSignOutPoint)
    return $ Just $ toJSON $ TxCompleteRes signedTx complete

balanceReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
           => AccountName -> Word32 -> Bool
           -> Handler m (Maybe Value)
balanceReq name minconf offline = do
    $(logInfo) $ format $ unlines
        [ "Balance"
        , "  Account name: " ++ unpack name
        , "  Minconf     : " ++ show minconf
        , "  Offline     : " ++ show offline
        ]
    bal <- runDB $ do
        Entity ai _ <- getAccount name
        accountBalance ai minconf offline
    return $ Just $ toJSON bal

nodeActionReq :: (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m)
              => NodeAction -> Handler m (Maybe Value)
nodeActionReq action = case action of
    NodeActionRescan tM -> do
        t <- case tM of
            Just t  -> return $ adjustFCTime t
            Nothing -> do
                timeM <- runDB firstAddrTime
                maybe err (return . adjustFCTime) timeM
        $(logInfo) $ format $ unlines
            [ "Node Rescan"
            , "  Timestamp: " ++ show t
            ]
        whenOnline $ do
            runDB resetRescan
            runNode $ atomicallyNodeT $ rescanTs t
        return $ Just $ toJSON $ RescanRes t
    NodeActionStatus -> do
        $(logInfo) $ format "Node Status"
        status <- runNode $ atomicallyNodeT nodeStatus
        return $ Just $ toJSON status
  where
    err = throwM $ WalletException
        "No keys have been generated in the wallet"

syncReq :: (MonadThrow m, MonadLoggerIO m, MonadUnliftIO m)
        => AccountName
        -> Either BlockHeight BlockHash
        -> ListRequest
        -> Handler m (Maybe Value)
syncReq acc blockE lq@ListRequest{..} = runDB $ do
    $(logInfo) $ format $ unlines
        [ "Sync"
        , "  Account name: " ++ cs acc
        , "  Block       : " ++ showBlock
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]
    ListResult nodes cnt <- mainChain blockE lq
    case nodes of
        [] -> return $ Just $ toJSON $ ListResult ([] :: [()]) cnt
        b:_ -> do
            Entity ai _ <- getAccount acc
            ts <- accTxsFromBlock ai (nodeBlockHeight b)
                (fromIntegral $ length nodes)
            let bts = blockTxs nodes ts
            return $ Just $ toJSON $ ListResult (map f bts) cnt
  where
    f (block, txs') = JsonSyncBlock
        { jsonSyncBlockHash   = nodeHash          block
        , jsonSyncBlockHeight = nodeBlockHeight   block
        , jsonSyncBlockPrev   = nodePrev          block
        , jsonSyncBlockTxs    = map (toJsonTx acc Nothing) txs'
        }
    showBlock = case blockE of
        Left  e -> show e
        Right b -> cs $ blockHashToHex b

blockInfoReq :: (MonadThrow m, MonadLoggerIO m, MonadUnliftIO m)
             => [BlockHash] -> Handler m (Maybe Value)
blockInfoReq headerLst = do
    $(logInfo) $ format "Received BlockInfo request"
    lstMaybeBlk <- forM headerLst (runNode . runSqlNodeT . getBlockByHash)
    return $ toJSON <$> Just (handleRes lstMaybeBlk)
  where
    handleRes :: [Maybe NodeBlock] -> [BlockInfo]
    handleRes = map fromNodeBlock . catMaybes

stopServerReq :: MonadLoggerIO m => Handler m (Maybe Value)
stopServerReq = do
    $(logInfo) $ format "Received StopServer request"
    return Nothing

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- configMode `liftM` asks handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter
    :: (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m)
    => Handler m ()
updateNodeFilter = do
    $(logInfo) $ format "Sending a new bloom filter"
    (bloom, elems, _) <- runDB getBloomFilter
    runNode $ atomicallyNodeT $ sendBloomFilter bloom elems

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

