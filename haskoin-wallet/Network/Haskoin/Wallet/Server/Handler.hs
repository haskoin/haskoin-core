module Network.Haskoin.Wallet.Server.Handler where

import Control.Arrow (first)
import Control.Monad (when, unless, liftM)
import Control.Exception (SomeException(..), throwIO, tryJust)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Control.Monad.Logger (MonadLogger, logInfo, logError)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import qualified Control.Concurrent.MSem as Sem (MSem, with)
import qualified Control.Monad.State as S (StateT, evalStateT, gets)

import Data.Aeson (Value(..), toJSON)
import Data.Word (Word32)
import Data.Text (Text, pack, unpack)
import qualified Data.Map.Strict as M (intersectionWith, fromList, elems)
import Data.String.Conversions (cs)

import Database.Esqueleto (SqlPersistT, Entity(..))

import Database.Persist.Sql
    ( SqlPersistM
    , ConnectionPool
    , runSqlPool
    , runSqlPersistMPool
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Node.BlockChain
import Network.Haskoin.Node.Peer

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Accounts
import Network.Haskoin.Wallet.Transaction
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types

type Handler m = S.StateT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig    :: !Config
    , handlerPool      :: !ConnectionPool
    , handlerNodeState :: !(Maybe SharedNodeState)
    , handlerSem       :: !(Sem.MSem Int)
    }

runHandler :: Monad m => HandlerSession -> Handler m a -> m a
runHandler = flip S.evalStateT

runDB :: MonadBaseControl IO m => SqlPersistT m a -> Handler m a
runDB action = do
    sem  <- S.gets handlerSem
    pool <- S.gets handlerPool
    lift $ runDBPool sem pool action

runDBPool :: MonadBaseControl IO m
          => Sem.MSem Int -> ConnectionPool -> SqlPersistT m a -> m a
runDBPool sem pool action = liftBaseOp_ (Sem.with sem) $ runSqlPool action pool

tryDBPool :: (MonadIO m, MonadLogger m)
          => Sem.MSem Int -> ConnectionPool -> SqlPersistM a -> m (Maybe a)
tryDBPool sem pool action = do
    resE <- liftIO $ Sem.with sem $ tryJust f $ runSqlPersistMPool action pool
    case resE of
        Right res -> return $ Just res
        Left err -> do
            $(logError) $ pack $ unwords [ "A database error occured:", err]
            return Nothing
  where
    f (SomeException e) = Just $ show e

runNode :: MonadIO m => NodeT m a -> Handler m a
runNode action = do
    nodeStateM <- S.gets handlerNodeState
    case nodeStateM of
        Just nodeState -> lift $ runNodeT nodeState action
        _ -> error "runNode: No node state available"

{- Server Handlers -}

getAccountsR :: ( MonadLogger m
                , MonadIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                )
             => Handler m (Maybe Value)
getAccountsR = do
    $(logInfo) $ format "GetAccountsR"
    accs <- runDB accounts
    return $ Just $ toJSON $ map (toJsonAccount Nothing) accs

postAccountsR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m
       )
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
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m
       )
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

getAccountR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
            => AccountName -> Handler m (Maybe Value)
getAccountR name = do
    $(logInfo) $ format $ unlines
        [ "GetAccountR"
        , "  Account name: " ++ unpack name
        ]
    Entity _ acc <- runDB $ getAccount name
    return $ Just $ toJSON $ toJsonAccount Nothing acc

postAccountKeysR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m
       )
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

postAccountGapR :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadIO m
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

getAddressesR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
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
    :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
    => AccountName -> AddressType -> Handler m (Maybe Value)
getAddressesUnusedR name addrType = do
    $(logInfo) $ format $ unlines
        [ "GetAddressesUnusedR"
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        ]

    addrs <- runDB $ do
        accE <- getAccount name
        unusedAddresses accE addrType

    return $ Just $ toJSON $ map (`toJsonAddr` Nothing) addrs

getAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
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

putAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
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

postAddressesR :: ( MonadLogger m
                  , MonadBaseControl IO m
                  , MonadIO m
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

getTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
        => AccountName -> ListRequest -> Handler m (Maybe Value)
getTxsR name listReq = do
    $(logInfo) $ format $ unlines
        [ "GetTxsR"
        , "  Account name: " ++ unpack name
        , "  Start index : " ++ show (listOffset listReq)
        , "  Reversed    : " ++ show (listReverse listReq)
        ]

    (res, cnt, height) <- runDB $ do
        Entity ai _ <- getAccount name
        (_, height) <- getBestBlock
        (res, cnt) <- txs ai listReq
        return (res, cnt, height)

    return $ Just $ toJSON $ ListResult (map (`toJsonTx` Just height) res) cnt

getAddrTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
            => AccountName -> KeyIndex -> AddressType -> ListRequest
            -> Handler m (Maybe Value)
getAddrTxsR name index addrType listReq = do
    $(logInfo) $ format $ unlines
        [ "GetAddrTxsR"
        , "  Account name : " ++ unpack name
        , "  Address index: " ++ show index
        , "  Address type : " ++ show addrType
        , "  Start index  : " ++ show (listOffset listReq)
        , "  Reversed     : " ++ show (listReverse listReq)
        ]

    (res, cnt, height) <- runDB $ do
        accE <- getAccount name
        addrE <- getAddress accE addrType index
        (_, height) <- getBestBlock
        (res, cnt) <- addrTxs accE addrE listReq
        return (res, cnt, height)

    return $ Just $ toJSON $ ListResult (map (`toJsonTx` Just height) res) cnt

postTxsR :: ( MonadLogger m, MonadBaseControl IO m, MonadBase IO m
            , MonadIO m, MonadThrow m, MonadResource m
            )
         => AccountName -> Maybe XPrvKey -> TxAction -> Handler m (Maybe Value)
postTxsR name masterM action = do
    (accE@(Entity ai _), height) <- runDB $ do
        accE <- getAccount name
        (_, height) <- getBestBlock
        return (accE, height)

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
                    _ -> liftIO . throwIO $ WalletException
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
                    _ -> liftIO . throwIO $ WalletException
                        "Could not import the transaction"
    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (walletTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [walletTxHash txRes]
    return $ Just $ toJSON $ toJsonTx txRes (Just height)

getTxR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
       => AccountName -> TxHash -> Handler m (Maybe Value)
getTxR name txid = do
    $(logInfo) $ format $ unlines
        [ "GetTxR"
        , "  Account name: " ++ unpack name
        , "  TxId        : " ++ cs (txHashToHex txid)
        ]
    (res, height) <- runDB $ do
        Entity ai _ <- getAccount name
        (_, height) <- getBestBlock
        res <- getAccountTx ai txid
        return (res, height)
    return $ Just $ toJSON $ toJsonTx res (Just height)

deleteTxIdR :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
            => TxHash -> Handler m (Maybe Value)
deleteTxIdR txid = do
    $(logInfo) $ format $ unlines
        [ "DeleteTxR"
        , "  TxId: " ++ cs (txHashToHex txid)
        ]
    runDB $ deleteTx txid
    return Nothing

getBalanceR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
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

getOfflineTxR :: ( MonadLogger m, MonadIO m, MonadBaseControl IO m
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

postOfflineTxR :: ( MonadLogger m, MonadIO m, MonadBaseControl IO m
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

postNodeR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
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
    err = liftIO . throwIO $ WalletException
        "No keys have been generated in the wallet"

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- configMode `liftM` S.gets handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter
    :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
    => Handler m ()
updateNodeFilter = do
    $(logInfo) $ format "Sending a new bloom filter"
    (bloom, elems, _) <- runDB getBloomFilter
    runNode $ atomicallyNodeT $ sendBloomFilter bloom elems

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

