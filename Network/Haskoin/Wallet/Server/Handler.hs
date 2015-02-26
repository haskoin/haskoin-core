module Network.Haskoin.Wallet.Server.Handler where

import Control.Monad (when, liftM)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Monad.Logger (MonadLogger, logInfo, logWarn, logDebug, logError)
import qualified Control.Monad.State as S (StateT, evalStateT, gets)

import Data.Aeson (Value(..), toJSON)
import Data.Word (Word32)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import qualified Data.Text as T (pack, unpack, unwords)

import Database.Persist.Sql (SqlPersistT, ConnectionPool, runSqlPool)

import Network.Haskoin.Crypto
import Network.Haskoin.Node
import Network.Haskoin.Transaction
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Root

type Handler m = S.StateT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig :: SPVConfig
    , handlerPool   :: ConnectionPool
    , handlerChan   :: Maybe (TBMChan NodeRequest)
    }

runHandler :: Monad m => HandlerSession -> Handler m a -> m a
runHandler session x = S.evalStateT x session

runDB :: MonadIO m => SqlPersistT IO a -> Handler m a
runDB action = do
    pool <- S.gets handlerPool
    liftIO $ runSqlPool action pool

sendSPV :: MonadIO m => NodeRequest -> Handler m ()
sendSPV request = do
    chanM <- S.gets handlerChan 
    case chanM of
        Just chan -> liftIO $ atomically $ writeTBMChan chan request
        Nothing   -> return ()

{- Server Handlers -}

getWalletsR :: (MonadLogger m, MonadIO m) => Handler m Value
getWalletsR = do
    $(logInfo) "[ZeroMQ] GetWallets"
    liftM toJSON $ runDB walletList

getWalletR :: (MonadLogger m, MonadIO m) 
           => WalletName -> Handler m Value
getWalletR w = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] GetWallet", w ]
    liftM toJSON $ runDB (getWallet w)

postWalletsR :: (MonadLogger m, MonadIO m) => NewWallet -> Handler m Value
postWalletsR (NewWallet name passM msM) = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] PostWallets", name ]
    (ms, seed) <- case msM of
        Just ms -> case mnemonicToSeed pass (T.unpack ms) of
            Left err -> liftIO $ throwIO $ WalletException err
            Right seed -> return (T.unpack ms, seed)
        Nothing -> do
            ent <- liftIO $ devURandom 16
            let msSeedE = do
                ms <- toMnemonic ent
                seed <- mnemonicToSeed pass ms
                return (ms, seed)
            case msSeedE of
                Left err -> liftIO $ throwIO $ WalletException err
                Right msSeed -> return msSeed
    _ <- runDB $ newWallet name seed
    return $ toJSON $ MnemonicRes ms
  where
    pass = T.unpack $ fromMaybe "" $ passM

getAccountsR :: (MonadLogger m, MonadIO m) 
             => WalletName -> Handler m Value
getAccountsR wallet = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] GetAccounts", wallet ]
    liftM toJSON $ runDB (accountList wallet)

postAccountsR :: (MonadLogger m, MonadIO m)
              => WalletName -> NewAccount -> Handler m Value
postAccountsR wallet newAcc = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] PostAccounts", wallet, newAccountAccountName newAcc ]
    gap <- liftM spvGap $ S.gets handlerConfig
    liftM toJSON $ case newAcc of
        NewAccountRegular name -> do
            acc <- runDB $ newAccount wallet name
            runDB $ addLookAhead wallet name gap
            whenOnline updateNodeFilter
            return acc
        NewAccountMultisig name m n keys -> do
            acc <- runDB $ newAccountMultisig wallet name m n keys
            when (length (accountKeys acc) == n) $ do
                runDB $ addLookAhead wallet name gap
                whenOnline updateNodeFilter
            return acc
        NewAccountRead name key -> do
            acc <- runDB $ newAccountRead wallet name key
            runDB $ addLookAhead wallet name gap
            whenOnline updateNodeFilter
            return acc
        NewAccountReadMultisig name m n keys -> do
            acc <- runDB $ newAccountReadMultisig wallet name m n keys
            when (length (accountKeys acc) == n) $ do
                runDB $ addLookAhead wallet name gap
                whenOnline updateNodeFilter
            return acc

getAccountR :: (MonadLogger m, MonadIO m) 
            => WalletName -> AccountName -> Handler m Value
getAccountR wallet name = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] GetAccount", wallet, name ]
    liftM toJSON $ runDB (getAccount wallet name)

postAccountKeysR :: (MonadLogger m, MonadIO m)
                 => WalletName -> AccountName -> [XPubKey] -> Handler m Value
postAccountKeysR wallet name keys = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] PostAccountKeys", wallet, name ]
    acc <- runDB $ addAccountKeys wallet name keys
    when (length (accountKeys acc) == accountTotal acc) $ do
        gap <- liftM spvGap $ S.gets handlerConfig
        runDB $ addLookAhead wallet name gap
        whenOnline updateNodeFilter
    return $ toJSON acc

getAddressesR :: (MonadLogger m, MonadIO m) 
              => WalletName -> AccountName -> (Maybe PagedResult) 
              -> Word32 -> Bool -> Bool -> Bool -> Handler m Value
getAddressesR wallet name pageM minconf internal unlabeled unused 
    | internal && unlabeled = goUnused -- There are no labels on internal addrs
    | unlabeled = goUnlabeled
    | unused = goUnused
    | otherwise = case pageM of
        Just (PagedResult page elemPerPage) -> do
            $(logInfo) $ T.unwords 
                [ "[ZeroMQ] GetAddresses", wallet, name
                , "( Page:", T.pack $ show page
                , "elemPerPage:", T.pack $ show elemPerPage
                , if internal then ", internal )" else ")"
                ]
            runDB $ do
                (xs, m) <- addressPage wallet name page elemPerPage internal
                ba      <- mapM (flip addressBalance minconf) xs
                return $ toJSON $ AddressPageRes ba m
        Nothing -> do
            $(logInfo) $ T.unwords 
                [ "[ZeroMQ] GetAddresses", wallet, name 
                , if internal then "( internal )" else ""
                ]
            runDB $ do
                xs <- addressList wallet name internal
                liftM toJSON $ mapM (flip addressBalance minconf) xs
  where
    goUnlabeled = do
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] GetAddresses", wallet, name, "(unlabeled)" ]
        runDB $ do
            pa <- unlabeledAddrs wallet name
            liftM toJSON $ mapM (flip addressBalance minconf) pa
    goUnused = do
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] GetAddresses", wallet, name
            , "( unused", if internal then ", internal" else "", ")"
            ]
        runDB $ do
            pa <- unusedAddrs wallet name internal
            liftM toJSON $ mapM (flip addressBalance minconf) pa

postAddressesR :: (MonadLogger m, MonadIO m) 
               => WalletName -> AccountName -> AddressData -> Handler m Value
postAddressesR wallet name (AddressData label) = do
    $(logInfo) $ T.unwords [ "[ZeroMQ] PostAddresses", wallet, name, label ]
    runDB $ do
        unlabeled <- unlabeledAddrs wallet name
        liftM toJSON $ case unlabeled of
            [] -> liftIO $ throwIO $ 
                WalletException "No more available addresses"
            (newAddr:_) -> setAddrLabel wallet name (laIndex newAddr) label

getAddressR :: (MonadLogger m, MonadIO m) 
            => WalletName -> AccountName -> KeyIndex -> Word32 -> Bool 
            -> Handler m Value
getAddressR wallet name i minconf internal = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] GetAddress", wallet, name, T.pack $ show i 
        , if internal then "( internal )" else ""
        ]
    runDB $ do
        pa <- getAddress wallet name i internal
        liftM toJSON $ addressBalance pa minconf

putAddressR :: (MonadLogger m, MonadIO m)
            => WalletName -> AccountName -> KeyIndex -> AddressData
            -> Handler m Value
putAddressR wallet name i (AddressData label) = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] PutAddress", wallet, name, T.pack $ show i, label ]
    liftM toJSON $ runDB (setAddrLabel wallet name i label)

getTxsR :: (MonadLogger m, MonadIO m)
        => WalletName -> AccountName -> (Maybe PagedResult) 
        -> Handler m Value
getTxsR wallet name pageM = case pageM of
    Just (PagedResult page elemPerPage) -> do
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] GetTxs", wallet, name
            , "( Page:", T.pack $ show page
            , "elemPerPage:", T.pack $ show elemPerPage, ")"
            ]
        runDB $ do
            (xs, m) <- txPage wallet name page elemPerPage
            return $ toJSON $ TxPageRes xs m
    Nothing -> do
        $(logInfo) $ T.unwords [ "[ZeroMQ] GetTxs", wallet, name ]
        runDB $ liftM toJSON $ txList wallet name

postTxsR :: (MonadLogger m, MonadIO m)
         => WalletName -> AccountName -> AccTxAction -> Handler m Value
postTxsR wallet name action = case action of
    CreateTx rs fee minconf sign -> do
        (tid, complete, genA) <- runDB $ 
            createTx wallet name minconf rs fee sign
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] CreateTx", wallet, name, T.pack $ encodeTxHashLE tid ]
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . NodePublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete
    SignTx tx finalize -> do
        (tid, complete, genA) <- runDB $ signWalletTx wallet name tx finalize
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] SignTx", wallet, name, T.pack $ encodeTxHashLE tid
            , "( Complete:", T.pack $ show complete, ")"
            ]
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . NodePublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete
    SignOfflineTxData otd finalize -> do
        (tx, complete) <- runDB $ signOfflineTxData wallet name finalize otd
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] SignOfflineTx", wallet, name
            , T.pack $ encodeTxHashLE $ txHash tx
            , "( Complete:", T.pack $ show complete, ")"
            ]
        return $ toJSON $ TxStatusRes tx complete
    ImportTx tx -> do
        resM <- runDB $ importTx tx SourceUnknown $ Just (wallet, name)
        let (tid, conf, genA) = fromJust resM
            complete = conf `elem` [ TxPending, TxBuilding ]
        when (isNothing resM) $ liftIO $ throwIO $
            WalletException "Transaction could not be imported"
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] ImportTx", wallet, name, T.pack $ encodeTxHashLE tid
            , "( Complete:", T.pack $ show complete, ")"
            ]
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . NodePublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete

getTxR :: (MonadLogger m, MonadIO m)
       => WalletName -> AccountName -> TxHash -> Bool -> Handler m Value
getTxR wallet name hash prop = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] GetTx", wallet, name, T.pack $ encodeTxHashLE hash
        , if prop then "( proposition )" else ""
        ]
    runDB $ do
        aTx <- getAccTx wallet name hash
        if prop
            then do
                p <- getProposition wallet name hash
                return $ toJSON aTx{ accTxTx = p }
            else return $ toJSON aTx

getOfflineTxDataR :: (MonadLogger m, MonadIO m) 
                  => WalletName -> AccountName -> TxHash -> Handler m Value
getOfflineTxDataR wallet name hash = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] GetOfflineTxData", wallet, name
        , T.pack $ encodeTxHashLE hash 
        ]
    liftM toJSON $ runDB (getOfflineTxData wallet name hash)

getBalanceR :: (MonadLogger m, MonadIO m)
            => WalletName -> AccountName -> Word32 -> Handler m Value
getBalanceR wallet name minconf = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] GetBalance", wallet, name
        , "MinConf:", T.pack $ show minconf
        ]
    (balance, cs) <- runDB $ accountBalance wallet name minconf
    return $ toJSON $ BalanceRes balance cs

getSpendableR :: (MonadLogger m, MonadIO m)
              => WalletName -> AccountName -> Word32 -> Handler m Value
getSpendableR wallet name minconf = do
    $(logInfo) $ T.unwords 
        [ "[ZeroMQ] GetSpendable", wallet, name
        , "MinConf:", T.pack $ show minconf
        ]
    balance <- runDB $ spendableAccountBalance wallet name minconf
    return $ toJSON $ SpendableRes balance

postNodeR :: (MonadLogger m, MonadIO m)
          => NodeAction -> Handler m Value
postNodeR action = liftM toJSON $ case action of
    Rescan (Just t) -> do
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] Rescan", "( Timestamp:", T.pack $ show t, " )" ]
        whenOnline $ do
            runDB resetRescan
            sendSPV $ NodeStartDownload $ Left t
        return $ RescanRes t
    Rescan Nothing -> do
        fstKeyTimeM <- runDB firstKeyTime
        when (isNothing fstKeyTimeM) $ liftIO $ throwIO $
            WalletException "No keys have been generated in the wallet"
        let fstKeyTime = fromJust fstKeyTimeM       
        $(logInfo) $ T.unwords 
            [ "[ZeroMQ] Rescan", "( Timestamp:", T.pack $ show fstKeyTime, " )" ]
        whenOnline $ do
            runDB resetRescan
            sendSPV $ NodeStartDownload $ Left fstKeyTime
        return $ RescanRes fstKeyTime

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- liftM spvMode $ S.gets handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter :: MonadIO m => Handler m ()
updateNodeFilter = do
    bloomFP <- liftM spvBloomFP $ S.gets handlerConfig
    sendSPV . NodeBloomFilter =<< runDB (walletBloomFilter bloomFP)

