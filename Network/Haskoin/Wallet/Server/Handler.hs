module Network.Haskoin.Wallet.Server.Handler where

import Control.Arrow (first)
import Control.Monad (when, liftM)
import Control.Exception (SomeException(..), throwIO, throw, tryJust)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Monad.Logger (MonadLogger, logInfo, logError)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import qualified Control.Concurrent.MSem as Sem (MSem, with)
import qualified Control.Monad.State as S (StateT, evalStateT, gets)

import Data.Aeson (Value(..), toJSON)
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL (consume)

import Database.Persist (Entity(..), entityVal, getBy)
import Database.Persist.Sql 
    ( SqlPersistT
    , SqlPersistM
    , ConnectionPool
    , runSqlPool
    , runSqlPersistMPool
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Node
import Network.Haskoin.Transaction
import Network.Haskoin.Block
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Transaction
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types

type Handler m = S.StateT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig :: Config
    , handlerPool   :: ConnectionPool
    , handlerChan   :: Maybe (TBMChan NodeRequest)
    , handlerSem    :: Sem.MSem Int
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

sendSPV :: MonadIO m => NodeRequest -> Handler m ()
sendSPV request = do
    chanM <- S.gets handlerChan 
    case chanM of
        Just chan -> liftIO $ atomically $ writeTBMChan chan request
        Nothing   -> return ()

{- Server Handlers -}

getKeyRingsR :: ( MonadLogger m
                , MonadIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => Handler m Value
getKeyRingsR = do
    $(logInfo) $ format "GetKeyRingsR"
    res <- runDB $ keyRingSource $$ CL.consume
    return $ toJSON $ map toJsonKeyRing res

getKeyRingR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> Handler m Value
getKeyRingR name = do
    $(logInfo) $ format $ unwords [ "GetKeyRingR", unpack name ]
    res <- runDB $ getKeyRing name
    return $ toJSON $ toJsonKeyRing $ entityVal res

postKeyRingsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
              => NewKeyRing -> Handler m Value
postKeyRingsR (NewKeyRing name passM msM) = do
    $(logInfo) $ format $ unwords [ "PostKeyRingsR", unpack name ]
    (ms, seed) <- case msM of
        Just ms -> case mnemonicToSeed pass (unpack ms) of
            Left err   -> liftIO $ throwIO $ WalletException err
            Right seed -> return (unpack ms, seed)
        Nothing -> do
            ent <- liftIO $ devURandom 16
            either (liftIO . throwIO . WalletException) return $ do
                ms   <- toMnemonic ent
                seed <- mnemonicToSeed pass ms
                return (ms, seed)
    _ <- runDB $ newKeyRing name seed
    return $ toJSON $ MnemonicRes ms
  where
    pass = unpack $ fromMaybe "" passM

getAccountsR :: ( MonadLogger m
                , MonadIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => KeyRingName -> Handler m Value
getAccountsR keyRingName = do
    $(logInfo) $ format $ unwords [ "GetAccountsR", unpack keyRingName ]
    res <- runDB $ accountSource keyRingName $$ CL.consume
    return $ toJSON $ map toJsonAccount res

postAccountsR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m )
    => KeyRingName -> NewAccount -> Handler m Value
postAccountsR keyRingName NewAccount{..} = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountsR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account type: " ++ show newAccountType
        , "  Account name: " ++ unpack newAccountAccountName 
        ]
    acc <- case newAccountType of
        AccountRegular -> runDB $ newAccount keyRingName newAccountAccountName
        AccountMultisig -> do
            let m = fromMaybe err newAccountRequiredSigs
                n = fromMaybe err newAccountTotalKeys
            runDB $ newAccountMultisig keyRingName 
                                       newAccountAccountName 
                                       newAccountKeys m n
        AccountRead -> case newAccountKeys of
            [] -> liftIO . throwIO $ WalletException 
                "A key is required for creating a read-only account"
            (key:_) -> 
                runDB $ newAccountRead keyRingName newAccountAccountName key
        AccountReadMultisig -> do
            let m = fromMaybe err newAccountRequiredSigs
                n = fromMaybe err newAccountTotalKeys
            runDB $ newAccountReadMultisig keyRingName 
                                           newAccountAccountName 
                                           newAccountKeys m n
    whenOnline $
        if isMultisigAccount acc
            then when (completeMultisig acc) updateNodeFilter
            else updateNodeFilter
    return $ toJSON $ toJsonAccount acc
  where
    err = throw $ WalletException $ unwords
        [ "Could not create account", unpack newAccountAccountName
        , "due to invalid multisig parameters."
        ]

getAccountR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> AccountName -> Handler m Value
getAccountR keyRingName name = do
    $(logInfo) $ format $ unlines 
        [ "GetAccountR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        ]
    res <- runDB $ getAccount keyRingName name
    return $ toJSON $ toJsonAccount $ entityVal res

postAccountKeysR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m )
    => KeyRingName -> AccountName -> [XPubKey] -> Handler m Value
postAccountKeysR keyRingName name keys = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountKeysR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        , "  Key count   : " ++ show (length keys)
        ]
    ms <- runDB $ do
        accE <- getAccount keyRingName name
        addAccountKeys accE keys
    when (completeMultisig ms) $ whenOnline updateNodeFilter
    return $ toJSON $ toJsonAccount ms

postAccountGapR :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadIO m
                   , MonadThrow m
                   , MonadResource m
                   ) 
                => KeyRingName -> AccountName -> SetAccountGap 
                -> Handler m Value
postAccountGapR keyRingName name (SetAccountGap gap) = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountGapR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        , "  New gap size: " ++ show gap
        ]
    acc <- runDB $ do
        accE <- getAccount keyRingName name
        setAccountGap accE gap
    whenOnline updateNodeFilter
    return $ toJSON $ toJsonAccount acc

getAddressesR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
              => KeyRingName -> AccountName -> AddressType -> PageRequest 
              -> Handler m Value
getAddressesR keyRingName name addrType page = do
    $(logInfo) $ format $ unlines 
        [ "GetAddressesR" 
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        , "  Page number : " ++ show (pageNum page)
        , "  Page size   : " ++ show (pageLen page)
        , "  Page reverse: " ++ show (pageReverse page)
        ]
    (as, m) <- runDB $ addressPage keyRingName name addrType page
    return $ toJSON $ PageRes (map toJsonAddr as) m

getAddressesUnusedR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
                    => KeyRingName -> AccountName -> AddressType 
                    -> Handler m Value
getAddressesUnusedR keyRingName name addrType = do
    $(logInfo) $ format $ unlines 
        [ "GetAddressesUnusedR" 
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        ]
    res <- runDB $ addressUnused keyRingName name addrType
    return $ toJSON $ map toJsonAddr res

getAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> AccountName -> KeyIndex -> AddressType
            -> Handler m Value
getAddressR keyRingName name i addrType = do
    $(logInfo) $ format $ unlines
        [ "GetAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Address type: " ++ show addrType
        ]
    res <- runDB $ getAddress keyRingName name i addrType
    return $ toJSON $ toJsonAddr $ entityVal res

putAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName 
            -> KeyIndex -> AddressType -> AddressLabel
            -> Handler m Value
putAddressR keyRingName name i addrType (AddressLabel label) = do
    $(logInfo) $ format $ unlines
        [ "PutAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Label       : " ++ unpack label
        ]
    res <- runDB $ setAddrLabel keyRingName name i addrType label
    return $ toJSON $ toJsonAddr res

getTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
        => KeyRingName -> AccountName -> PageRequest -> Handler m Value
getTxsR keyRingName name page = do
    $(logInfo) $ format $ unlines
        [ "GetTxsR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Page number : " ++ show (pageNum page)
        , "  Page size   : " ++ show (pageLen page)
        , "  Page reverse: " ++ show (pageReverse page)
        ]
    liftM toJSON $ runDB $ do
        (_, height) <- getBestBlock
        (txs, m) <- txPage keyRingName name page
        return $ PageRes (map (toJsonTx height) txs) m

getAddrTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName 
            -> KeyIndex -> AddressType -> PageRequest 
            -> Handler m Value
getAddrTxsR keyRingName name index addrType page = do
    $(logInfo) $ format $ unlines
        [ "GetAddrTxsR"
        , "  KeyRing name : " ++ unpack keyRingName
        , "  Account name : " ++ unpack name
        , "  Address index: " ++ show index
        , "  Address type : " ++ show addrType
        , "  Page number  : " ++ show (pageNum page)
        , "  Page size    : " ++ show (pageLen page)
        , "  Page reverse : " ++ show (pageReverse page)
        ]
    liftM toJSON $ runDB $ do
        (_, height) <- getBestBlock
        (txs, m) <- addrTxPage keyRingName name index addrType page
        return $ PageRes (map (toJsonAddrTx height) txs) m

postTxsR :: ( MonadLogger m
            , MonadBaseControl IO m
            , MonadBase IO m
            , MonadIO m
            , MonadThrow m
            , MonadResource m
            )
         => KeyRingName -> AccountName -> TxAction -> Handler m Value
postTxsR keyRingName name action = case action of
    CreateTx rs fee rcptFee minconf sign -> do
        $(logInfo) $ format $ unlines
            [ "PostTxsR CreateTx"
            , "  KeyRing name: " ++ unpack keyRingName
            , "  Account name: " ++ unpack name
            , "  Recipients  : " ++ show (map (first addrToBase58) rs)
            , "  Fee         : " ++ show fee
            , "  Rcpt. Fee   : " ++ show rcptFee
            , "  Minconf     : " ++ show minconf
            , "  Sign        : " ++ show sign
            ]
        (txid, confidence, before) <- runDB $ do
            -- Get the number of elements before creating the new transaction
            (_, before, _)  <- getBloomFilter
            (txid, confidence) <- createTx
                keyRingName name minconf rs fee rcptFee sign
            return (txid, confidence, before)
        onlineAction before confidence txid
        return $ toJSON $ TxHashConfidenceRes txid confidence
    ImportTx tx -> do
        $(logInfo) $ format $ unlines
            [ "PostTxsR ImportTx"
            , "  KeyRing name: " ++ unpack keyRingName
            , "  Account name: " ++ unpack name
            , "  Txid        : " ++ encodeTxHashLE (txHash tx)
            ]
        (txid, confidence, before) <- runDB $ do
            -- Get the number of elements before signing the transaction
            (_, before, _)  <- getBloomFilter
            Entity ai _ <- getAccount keyRingName name
            (txid, confidence) <- importTx tx ai
            return (txid, confidence, before)
        onlineAction before confidence txid
        return $ toJSON $ TxHashConfidenceRes txid confidence
    SignTx initTxid -> do
        $(logInfo) $ format $ unlines
            [ "PostTxsR SignTx"
            , "  KeyRing name: " ++ unpack keyRingName
            , "  Account name: " ++ unpack name
            , "  Txid        : " ++ encodeTxHashLE initTxid
            ]
        (txid, confidence, before) <- runDB $ do
            -- Get the number of elements before signing the transaction
            (_, before, _)  <- getBloomFilter
            Entity _ keyRing <- getKeyRing keyRingName
            accE <- getAccount keyRingName name
            (txid, confidence) <- signKeyRingTx keyRing accE initTxid
            return (txid, confidence, before)
        onlineAction before confidence txid
        return $ toJSON $ TxHashConfidenceRes txid confidence
    SignOfflineTx tx signData -> do
        $(logInfo) $ format $ unlines
            [ "PostTxsR SignOfflineTx"
            , "  KeyRing name: " ++ unpack keyRingName
            , "  Account name: " ++ unpack name
            , "  Txid        : " ++ encodeTxHashLE (txHash tx)
            ]
        signedTx <- runDB $ do
            Entity _ keyRing <- getKeyRing keyRingName
            Entity _ acc     <- getAccount keyRingName name
            return $ signOfflineTx keyRing acc tx signData
        let toDat CoinSignData{..} = (coinSignScriptOutput, coinSignOutPoint)
            complete = verifyStdTx signedTx $ map toDat signData
        return $ toJSON $ TxCompleteRes signedTx complete
  where
    onlineAction before confidence txid = whenOnline $ do
        (_, after, _) <- runDB getBloomFilter
        -- Publish the new bloom filter to our peers only if it changed
        when (after > before) updateNodeFilter
        -- Publish the transaction to the network only wen it is complete
        when (confidence == TxPending) $ 
            sendSPV $ NodePublishTxs [txid]

getTxR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
       => KeyRingName -> AccountName -> TxHash -> Handler m Value
getTxR keyRingName accountName txid = do
    $(logInfo) $ format $ unlines
        [ "GetTxR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack accountName
        , "  Txid        : " ++ encodeTxHashLE txid
        ]
    runDB $ do
        Entity ai _ <- getAccount keyRingName accountName
        txM         <- getBy $ UniqueAccTx ai txid
        case txM of
            Just (Entity _ res) -> do
                (_, height) <- getBestBlock
                return $ toJSON $ toJsonTx height res
            _ -> liftIO . throwIO $ WalletException $ unwords
                [ "Transaction", encodeTxHashLE txid, "does not exist." ]

getOfflineTxR :: ( MonadLogger m
                 , MonadIO m
                 , MonadBaseControl IO m
                 , MonadBase IO m
                 , MonadThrow m
                 , MonadResource m
                 ) 
              => KeyRingName -> AccountName -> TxHash -> Handler m Value
getOfflineTxR keyRingName accountName txid = do
    $(logInfo) $ format $ unlines
        [ "GetOfflineTxR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack accountName
        , "  Txid        : " ++ encodeTxHashLE txid
        ]
    (dat, _) <- runDB $ do
        Entity ai _ <- getAccount keyRingName accountName
        getOfflineTxData ai txid
    return $ toJSON dat

getBalanceR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName -> Word32 -> Handler m Value
getBalanceR keyRingName name minconf = do
    $(logInfo) $ format $ unlines
        [ "GetBalanceR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Minconf     : " ++ show minconf
        ]
    balance <- runDB $ do
        Entity ai _ <- getAccount keyRingName name
        accountBalance ai minconf
    return $ toJSON $ BalanceRes balance

getOfflineBalanceR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
                   => KeyRingName -> AccountName -> Handler m Value
getOfflineBalanceR keyRingName name = do
    $(logInfo) $ format $ unlines
        [ "GetOfflineBalanceR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        ]
    balance <- runDB $ do
        Entity ai _ <- getAccount keyRingName name
        offlineBalance ai
    return $ toJSON $ BalanceRes balance

postNodeR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => NodeAction -> Handler m Value
postNodeR action = do
    t <- case action of
        Rescan (Just t) -> return $ adjustFCTime t
        Rescan Nothing  -> do
            timeM <- runDB firstAddrTime
            maybe err (return . adjustFCTime) timeM
    $(logInfo) $ format $ unlines
        [ "NodeR Rescan"
        , "  Timestamp: " ++ show t
        ]
    whenOnline $ do
        runDB resetRescan
        sendSPV $ NodeStartMerkleDownload $ Left t
    return $ toJSON $ RescanRes t
  where
    err = liftIO . throwIO $ WalletException
        "No keys have been generated in the wallet"

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- configMode `liftM` S.gets handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter :: (MonadBaseControl IO m, MonadIO m) => Handler m ()
updateNodeFilter = sendSPV . NodeBloomFilter . fst3 =<< runDB getBloomFilter

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

