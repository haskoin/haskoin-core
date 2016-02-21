{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Network.Haskoin.Wallet.Server.Handler where

import           Control.Arrow                      (first)
import           Control.Exception                  (SomeException (..),
                                                     tryJust)
import           Control.Monad                      (liftM, unless, when)
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.Logger               (MonadLoggerIO, logError,
                                                     logInfo)
import           Control.Monad.Reader               (ReaderT, asks, runReaderT)
import           Control.Monad.Trans                (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Trans.Resource       (MonadResource)
import           Data.Aeson                         (Value (..), toJSON)
import qualified Data.Map.Strict                    as M (elems, fromList,
                                                          intersectionWith)
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text, pack, unpack)
import           Data.Word                          (Word32)
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

type Handler m = ReaderT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig    :: !Config
    , handlerPool      :: !ConnectionPool
    , handlerNodeState :: !(Maybe SharedNodeState)
    }

runHandler :: Monad m => Handler m a -> HandlerSession -> m a
runHandler = runReaderT

runDB :: MonadBaseControl IO m => SqlPersistT m a -> Handler m a
runDB action = asks handlerPool >>= lift . runDBPool action

runDBPool :: MonadBaseControl IO m => SqlPersistT m a -> ConnectionPool -> m a
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

getAccountsR :: ( MonadLoggerIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                )
             => ListRequest
             -> Handler m (Maybe Value)
getAccountsR lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "GetAccountsR"
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]
    (accs, cnt) <- runDB $ accounts lq
    return $ Just $ toJSON $ ListResult (map (toJsonAccount Nothing) accs) cnt

postAccountsR
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadBaseControl IO m)
    => NewAccount -> Handler m (Maybe Value)
postAccountsR newAcc@NewAccount{..} = do
    $(logInfo) $ format $ unlines
        [ "PostAccountsR"
        , "  Account name: " ++ unpack newAccountName
        , "  Account type: " ++ show newAccountType
        ]
    (Entity _ newAcc', mnemonicM) <- runDB $ newAccount newAcc
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc') updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount mnemonicM newAcc'

postAccountRenameR
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadBaseControl IO m)
    => AccountName -> AccountName -> Handler m (Maybe Value)
postAccountRenameR oldName newName = do
    $(logInfo) $ format $ unlines
        [ "PostAccountRenameR"
        , "  Account name: " ++ unpack oldName
        , "  New name    : " ++ unpack newName
        ]
    newAcc <- runDB $ do
        accE <- getAccount oldName
        renameAccount accE newName
    return $ Just $ toJSON $ toJsonAccount Nothing newAcc

getAccountR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
            => AccountName -> Handler m (Maybe Value)
getAccountR name = do
    $(logInfo) $ format $ unlines
        [ "GetAccountR"
        , "  Account name: " ++ unpack name
        ]
    Entity _ acc <- runDB $ getAccount name
    return $ Just $ toJSON $ toJsonAccount Nothing acc

postAccountKeysR
    :: (MonadResource m, MonadThrow m, MonadLoggerIO m, MonadBaseControl IO m)
    => AccountName -> [XPubKey] -> Handler m (Maybe Value)
postAccountKeysR name keys = do
    $(logInfo) $ format $ unlines
        [ "PostAccountKeysR"
        , "  Account name: " ++ unpack name
        , "  Key count   : " ++ show (length keys)
        ]
    newAcc <- runDB $ do
        accE <- getAccount name
        addAccountKeys accE keys
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc) updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount Nothing newAcc

postAccountGapR :: ( MonadLoggerIO m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadThrow m
                   , MonadResource m
                   )
                => AccountName -> SetAccountGap
                -> Handler m (Maybe Value)
postAccountGapR name (SetAccountGap gap) = do
    $(logInfo) $ format $ unlines
        [ "PostAccountGapR"
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

getAddressesR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
              => AccountName
              -> AddressType
              -> Word32
              -> Bool
              -> ListRequest
              -> Handler m (Maybe Value)
getAddressesR name addrType minConf offline listReq = do
    $(logInfo) $ format $ unlines
        [ "GetAddressesR"
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

getAddressesUnusedR
    :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
    => AccountName -> AddressType -> ListRequest -> Handler m (Maybe Value)
getAddressesUnusedR name addrType lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "GetAddressesUnusedR"
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

getAddressR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
            => AccountName -> KeyIndex -> AddressType
            -> Word32 -> Bool
            -> Handler m (Maybe Value)
getAddressR name i addrType minConf offline = do
    $(logInfo) $ format $ unlines
        [ "GetAddressR"
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

putAddressR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
            => AccountName
            -> KeyIndex
            -> AddressType
            -> AddressLabel
            -> Handler m (Maybe Value)
putAddressR name i addrType (AddressLabel label) = do
    $(logInfo) $ format $ unlines
        [ "PutAddressR"
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Label       : " ++ unpack label
        ]

    newAddr <- runDB $ do
        accE <- getAccount name
        setAddrLabel accE i addrType label

    return $ Just $ toJSON $ toJsonAddr newAddr Nothing

postAddressesR :: ( MonadLoggerIO m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadBase IO m
                  , MonadResource m
                  )
               => AccountName
               -> KeyIndex
               -> AddressType
               -> Handler m (Maybe Value)
postAddressesR name i addrType = do
    $(logInfo) $ format $ unlines
        [ "PostAddressesR"
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        ]

    cnt <- runDB $ do
        accE <- getAccount name
        generateAddrs accE addrType i

    -- Update the bloom filter
    whenOnline updateNodeFilter

    return $ Just $ toJSON cnt

getTxsR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
        => AccountName -> ListRequest -> Handler m (Maybe Value)
getTxsR name lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "GetTxsR"
        , "  Account name: " ++ unpack name
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]

    (res, cnt, bb) <- runDB $ do
        Entity ai _ <- getAccount name
        bb <- walletBestBlock
        (res, cnt) <- txs ai lq
        return (res, cnt, bb)

    return $ Just $ toJSON $ ListResult (map (`toJsonTx` Just bb) res) cnt

getAddrTxsR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
            => AccountName -> KeyIndex -> AddressType -> ListRequest
            -> Handler m (Maybe Value)
getAddrTxsR name index addrType lq@ListRequest{..} = do
    $(logInfo) $ format $ unlines
        [ "GetAddrTxsR"
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

    return $ Just $ toJSON $ ListResult (map (`toJsonTx` Just bb) res) cnt

postTxsR :: ( MonadLoggerIO m, MonadBaseControl IO m, MonadBase IO m
            , MonadThrow m, MonadResource m
            )
         => AccountName -> Maybe XPrvKey -> TxAction -> Handler m (Maybe Value)
postTxsR name masterM action = do
    (accE@(Entity ai _), bb) <- runDB $ do
        accE <- getAccount name
        bb <- walletBestBlock
        return (accE, bb)

    (txRes, newAddrs) <- case action of
        CreateTx rs fee minconf rcptFee sign -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR CreateTx"
                , "  Account name: " ++ unpack name
                , "  Recipients  : " ++ show (map (first addrToBase58) rs)
                , "  Fee         : " ++ show fee
                , "  Minconf     : " ++ show minconf
                , "  Rcpt. Fee   : " ++ show rcptFee
                , "  Sign        : " ++ show sign
                ]
            runDB $ createTx accE masterM rs fee minconf rcptFee sign
        ImportTx tx -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR ImportTx"
                , "  Account name: " ++ unpack name
                , "  TxId        : " ++ cs (txHashToHex (txHash tx))
                ]
            runDB $ do
                (res, newAddrs) <- importTx tx ai
                case filter ((== ai) . walletTxAccount) res of
                    (txRes:_) -> return (txRes, newAddrs)
                    _ -> throwM $ WalletException
                        "Could not import the transaction"
        SignTx txid -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR SignTx"
                , "  Account name: " ++ unpack name
                , "  TxId        : " ++ cs (txHashToHex txid)
                ]
            runDB $ do
                (res, newAddrs) <- signAccountTx accE masterM txid
                case filter ((== ai) . walletTxAccount) res of
                    (txRes:_) -> return (txRes, newAddrs)
                    _ -> throwM $ WalletException
                        "Could not import the transaction"
    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (walletTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [walletTxHash txRes]
    return $ Just $ toJSON $ toJsonTx txRes (Just bb)

getTxR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
       => AccountName -> TxHash -> Handler m (Maybe Value)
getTxR name txid = do
    $(logInfo) $ format $ unlines
        [ "GetTxR"
        , "  Account name: " ++ unpack name
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]
    (res, bb) <- runDB $ do
        Entity ai _ <- getAccount name
        bb <- walletBestBlock
        res <- getAccountTx ai txid
        return (res, bb)
    return $ Just $ toJSON $ toJsonTx res (Just bb)

deleteTxIdR :: (MonadLoggerIO m, MonadThrow m, MonadBaseControl IO m)
            => TxHash -> Handler m (Maybe Value)
deleteTxIdR txid = do
    $(logInfo) $ format $ unlines
        [ "DeleteTxR"
        , "  TxId: " ++ cs (txHashToHex txid)
        ]
    runDB $ deleteTx txid
    return Nothing

getBalanceR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
            => AccountName -> Word32 -> Bool
            -> Handler m (Maybe Value)
getBalanceR name minconf offline = do
    $(logInfo) $ format $ unlines
        [ "GetBalanceR"
        , "  Account name: " ++ unpack name
        , "  Minconf     : " ++ show minconf
        , "  Offline     : " ++ show offline
        ]
    bal <- runDB $ do
        Entity ai _ <- getAccount name
        accountBalance ai minconf offline
    return $ Just $ toJSON bal

getOfflineTxR :: ( MonadLoggerIO m, MonadBaseControl IO m
                 , MonadBase IO m, MonadThrow m, MonadResource m
                 )
              => AccountName -> TxHash -> Handler m (Maybe Value)
getOfflineTxR accountName txid = do
    $(logInfo) $ format $ unlines
        [ "GetOfflineTxR"
        , "  Account name: " ++ unpack accountName
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]
    (dat, _) <- runDB $ do
        Entity ai _ <- getAccount accountName
        getOfflineTxData ai txid
    return $ Just $ toJSON dat

postOfflineTxR :: ( MonadLoggerIO m, MonadBaseControl IO m
                  , MonadBase IO m, MonadThrow m, MonadResource m
                  )
               => AccountName
               -> Maybe XPrvKey
               -> Tx
               -> [CoinSignData]
               -> Handler m (Maybe Value)
postOfflineTxR accountName masterM tx signData = do
    $(logInfo) $ format $ unlines
        [ "PostTxsR SignOfflineTx"
        , "  Account name: " ++ unpack accountName
        , "  TxId        : " ++ cs (txHashToHex (txHash tx))
        ]
    Entity _ acc <- runDB $ getAccount accountName
    let signedTx = signOfflineTx acc masterM tx signData
        complete = verifyStdTx signedTx $ map toDat signData
        toDat CoinSignData{..} = (coinSignScriptOutput, coinSignOutPoint)
    return $ Just $ toJSON $ TxCompleteRes signedTx complete

postNodeR :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
          => NodeAction -> Handler m (Maybe Value)
postNodeR action = case action of
    NodeActionRescan tM -> do
        t <- case tM of
            Just t  -> return $ adjustFCTime t
            Nothing -> do
                timeM <- runDB firstAddrTime
                maybe err (return . adjustFCTime) timeM
        $(logInfo) $ format $ unlines
            [ "NodeR Rescan"
            , "  Timestamp: " ++ show t
            ]
        whenOnline $ do
            runDB resetRescan
            runNode $ atomicallyNodeT $ rescanTs t
        return $ Just $ toJSON $ RescanRes t
    NodeActionStatus -> do
        status <- runNode $ atomicallyNodeT nodeStatus
        return $ Just $ toJSON status
  where
    err = throwM $ WalletException
        "No keys have been generated in the wallet"

getSyncR :: (MonadThrow m, MonadLoggerIO m, MonadBaseControl IO m)
         => AccountName
         -> Either BlockHeight BlockHash
         -> ListRequest
         -> Handler m (Maybe Value)
getSyncR acc blockE lq@ListRequest{..} = runDB $ do
    $(logInfo) $ format $ unlines
        [ "GetSyncR"
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
            return $ Just $ toJSON $ ListResult (blockTxs nodes ts) cnt
  where
    showBlock = case blockE of
        Left  e -> show e
        Right b -> cs $ blockHashToHex b

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- configMode `liftM` asks handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter
    :: (MonadBaseControl IO m, MonadLoggerIO m, MonadThrow m)
    => Handler m ()
updateNodeFilter = do
    $(logInfo) $ format "Sending a new bloom filter"
    (bloom, elems, _) <- runDB getBloomFilter
    runNode $ atomicallyNodeT $ sendBloomFilter bloom elems

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

