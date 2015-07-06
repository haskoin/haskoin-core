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

import qualified Database.Persist as P
    ( Filter, SelectOpt( Asc, Desc, OffsetBy, LimitTo )
    , selectFirst, updateWhere, selectSource, count, update
    , deleteWhere, insertBy, insertMany_
    , (=.), (==.), (<.), (>.), (<-.)
    )
import Database.Esqueleto 
    ( Esqueleto, SqlQuery, SqlExpr, SqlBackend
    , InnerJoin(..), LeftOuterJoin(..), OrderBy, update
    , select, from, where_, val, valList, sub_select, countRows, count
    , orderBy, limit, asc, desc, set, offset, selectSource, updateCount
    , subList_select, in_, unValue, max_, not_, coalesceDefault, just, on
    , (^.), (=.), (==.), (&&.), (||.), (<.)
    , (<=.), (>.), (>=.), (-.), (*.), (?.), (!=.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , getBy, insertUnique, updateGet, replace, get, insertMany_, insert_
    )
import qualified Database.Esqueleto as E (isNothing, Value(..))

import Database.Persist.Sql 
    ( SqlPersistM
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
             => Handler m (Maybe Value)
getKeyRingsR = do
    $(logInfo) $ format "GetKeyRingsR"
    res <- runDB $ keyRingSource $$ CL.consume
    return $ Just $ toJSON $ map toJsonKeyRing res

getKeyRingR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> Handler m (Maybe Value)
getKeyRingR name = do
    $(logInfo) $ format $ unwords [ "GetKeyRingR", unpack name ]
    Entity _ keyRing <- runDB $ getKeyRing name
    return $ Just $ toJSON $ toJsonKeyRing keyRing

postKeyRingsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
              => NewKeyRing -> Handler m (Maybe Value)
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
    return $ Just $ toJSON $ MnemonicRes ms
  where
    pass = unpack $ fromMaybe "" passM

getAccountsR :: ( MonadLogger m
                , MonadIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => KeyRingName -> Handler m (Maybe Value)
getAccountsR keyRingName = do
    $(logInfo) $ format $ unwords [ "GetAccountsR", unpack keyRingName ]
    res <- runDB $ accountSource keyRingName $$ CL.consume
    return $ Just $ toJSON $ map buildJson res
  where
    buildJson (k, a) = toJsonAccount (Just $ toJsonKeyRing k) a

postAccountsR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m )
    => KeyRingName -> NewAccount -> Handler m (Maybe Value)
postAccountsR keyRingName NewAccount{..} = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountsR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account type: " ++ show newAccountType
        , "  Account name: " ++ unpack newAccountAccountName 
        ]
    (k, Entity _ a) <- runDB $ newAccount 
        keyRingName newAccountAccountName newAccountType newAccountKeys
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount a) $ updateNodeFilter
    return $ Just $ toJSON $ toJsonAccount (Just $ toJsonKeyRing k) a

getAccountR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> AccountName -> Handler m (Maybe Value)
getAccountR keyRingName name = do
    $(logInfo) $ format $ unlines 
        [ "GetAccountR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        ]
    (k, Entity _ a) <- runDB $ getAccount keyRingName name
    return $ Just $ toJSON $ toJsonAccount (Just $ toJsonKeyRing k) a

postAccountKeysR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m 
       )
    => KeyRingName -> AccountName -> [XPubKey] -> Handler m (Maybe Value)
postAccountKeysR keyRingName name keys = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountKeysR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        , "  Key count   : " ++ show (length keys)
        ]
    newAcc <- runDB $ do
        (_, accE) <- getAccount keyRingName name
        addAccountKeys accE keys
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc) updateNodeFilter
    return Nothing

postAccountGapR :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadIO m
                   , MonadThrow m
                   , MonadResource m
                   ) 
                => KeyRingName -> AccountName -> SetAccountGap 
                -> Handler m (Maybe Value)
postAccountGapR keyRingName name (SetAccountGap gap) = do
    $(logInfo) $ format $ unlines 
        [ "PostAccountGapR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name 
        , "  New gap size: " ++ show gap
        ]
    -- Update the gap
    runDB $ do
        (_, accE) <- getAccount keyRingName name
        setAccountGap accE gap
    -- Update the bloom filter
    whenOnline updateNodeFilter
    return Nothing

getAddressesR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
              => KeyRingName -> AccountName -> AddressType -> PageRequest 
              -> Handler m (Maybe Value)
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
    (res, maxPage) <- runDB $ addressPage keyRingName name addrType page
    return $ Just $ toJSON $ PageRes (map buildJsonAddr res) maxPage

getAddressesUnusedR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
                    => KeyRingName -> AccountName -> AddressType 
                    -> Handler m (Maybe Value)
getAddressesUnusedR keyRingName name addrType = do
    $(logInfo) $ format $ unlines 
        [ "GetAddressesUnusedR" 
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        ]
    res <- runDB $ unusedAddresses keyRingName name addrType
    return $ Just $ toJSON $ map buildJsonAddr res

getAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) 
            => KeyRingName -> AccountName -> KeyIndex -> AddressType
            -> Handler m (Maybe Value)
getAddressR keyRingName name i addrType = do
    $(logInfo) $ format $ unlines
        [ "GetAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Address type: " ++ show addrType
        ]
    (k, a, Entity _ x) <- runDB $ getAddress keyRingName name i addrType
    return $ Just $ toJSON $ buildJsonAddr (k, a, x)

buildJsonAddr :: (KeyRing, KeyRingAccount, KeyRingAddr) -> JsonAddr
buildJsonAddr (k, a, x) =
    toJsonAddr (Just accJson) Nothing Nothing x
  where
    accJson = toJsonAccount (Just $ toJsonKeyRing k) a

putAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName 
            -> KeyIndex -> AddressType -> AddressLabel
            -> Handler m (Maybe Value)
putAddressR keyRingName name i addrType (AddressLabel label) = do
    $(logInfo) $ format $ unlines
        [ "PutAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Label       : " ++ unpack label
        ]
    runDB $ setAddrLabel keyRingName name i addrType label
    return Nothing

getTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
        => KeyRingName -> AccountName -> PageRequest -> Handler m (Maybe Value)
getTxsR keyRingName name page = do
    $(logInfo) $ format $ unlines
        [ "GetTxsR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Page number : " ++ show (pageNum page)
        , "  Page size   : " ++ show (pageLen page)
        , "  Page reverse: " ++ show (pageReverse page)
        ]
    runDB $ do
        (_, height) <- getBestBlock
        (res, m) <- txPage keyRingName name page
        return $ Just $ toJSON $ PageRes (map (buildJson height) res) m
  where
    buildJson height (k, a, t) =
        let accJson  = toJsonAccount (Just $ toJsonKeyRing k) a
        in  toJsonTx (Just accJson) (Just height) t

getAddrTxsR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName 
            -> KeyIndex -> AddressType -> PageRequest 
            -> Handler m (Maybe Value)
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
    runDB $ do
        (_, height) <- getBestBlock
        (res, m) <- addrTxPage keyRingName name addrType index page
        return $ Just $ toJSON $ PageRes (map (buildJson height) res) m
  where
    buildJson height (k, a, x, xt, t) =
        let accJson  = toJsonAccount (Just $ toJsonKeyRing k) a
            addrJson = toJsonAddr (Just accJson) Nothing Nothing x
            txJson   = toJsonTx (Just accJson) (Just height) t
        in  toJsonAddrTx (Just addrJson) (Just txJson) xt

postTxsR :: ( MonadLogger m
            , MonadBaseControl IO m
            , MonadBase IO m
            , MonadIO m
            , MonadThrow m
            , MonadResource m
            )
         => KeyRingName -> AccountName -> TxAction -> Handler m (Maybe Value)
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
        return $ Just $ toJSON $ TxHashConfidenceRes txid confidence
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
            (_, Entity ai _) <- getAccount keyRingName name
            (txid, confidence) <- importTx tx ai
            return (txid, confidence, before)
        onlineAction before confidence txid
        return $ Just $ toJSON $ TxHashConfidenceRes txid confidence
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
            (keyRing, accE) <- getAccount keyRingName name
            (txid, confidence) <- signKeyRingTx keyRing accE initTxid
            return (txid, confidence, before)
        onlineAction before confidence txid
        return $ Just $ toJSON $ TxHashConfidenceRes txid confidence
    SignOfflineTx tx signData -> do
        $(logInfo) $ format $ unlines
            [ "PostTxsR SignOfflineTx"
            , "  KeyRing name: " ++ unpack keyRingName
            , "  Account name: " ++ unpack name
            , "  Txid        : " ++ encodeTxHashLE (txHash tx)
            ]
        signedTx <- runDB $ do
            (keyRing, Entity _ acc) <- getAccount keyRingName name
            return $ signOfflineTx keyRing acc tx signData
        let toDat CoinSignData{..} = (coinSignScriptOutput, coinSignOutPoint)
            complete = verifyStdTx signedTx $ map toDat signData
        return $ Just $ toJSON $ TxCompleteRes signedTx complete
  where
    onlineAction before confidence txid = whenOnline $ do
        (_, after, _) <- runDB getBloomFilter
        -- Publish the new bloom filter to our peers only if it changed
        when (after > before) updateNodeFilter
        -- Publish the transaction to the network only wen it is complete
        when (confidence == TxPending) $ sendSPV $ NodePublishTxs [txid]

getTxR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
       => KeyRingName -> AccountName -> TxHash -> Handler m (Maybe Value)
getTxR keyRingName accountName txid = do
    $(logInfo) $ format $ unlines
        [ "GetTxR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack accountName
        , "  Txid        : " ++ encodeTxHashLE txid
        ]
    runDB $ do
        (_, height) <- getBestBlock
        res <- select $ from $ \(k `InnerJoin` a `InnerJoin` t) -> do
            on $ t ^. KeyRingTxAccount      ==. a ^. KeyRingAccountId
            on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
            where_ (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   &&. t ^. KeyRingTxHash      ==. val txid
                   )
            return (k, a, t)
        case res of
            ((Entity _ k, Entity _ a, Entity _ t):_) -> 
                let accJson = toJsonAccount (Just $ toJsonKeyRing k) a
                in  return $ Just $ 
                        toJSON $ toJsonTx (Just accJson) (Just height) t
            _ -> liftIO . throwIO $ WalletException $ unwords
                [ "Transaction", encodeTxHashLE txid, "does not exist." ]

getOfflineTxR :: ( MonadLogger m
                 , MonadIO m
                 , MonadBaseControl IO m
                 , MonadBase IO m
                 , MonadThrow m
                 , MonadResource m
                 ) 
              => KeyRingName -> AccountName -> TxHash -> Handler m (Maybe Value)
getOfflineTxR keyRingName accountName txid = do
    $(logInfo) $ format $ unlines
        [ "GetOfflineTxR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack accountName
        , "  Txid        : " ++ encodeTxHashLE txid
        ]
    (dat, _) <- runDB $ do
        (_, Entity ai _) <- getAccount keyRingName accountName
        getOfflineTxData ai txid
    return $ Just $ toJSON dat

getBalanceR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName -> Word32 -> Bool 
            -> Handler m (Maybe Value)
getBalanceR keyRingName name minconf offline = do
    $(logInfo) $ format $ unlines
        [ "GetBalanceR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Minconf     : " ++ show minconf
        , "  Offline     : " ++ show offline
        ]
    balance <- runDB $ accountBalance keyRingName name minconf offline
    return $ Just $ toJSON $ BalanceRes balance

postNodeR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => NodeAction -> Handler m (Maybe Value)
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
    return $ Just $ toJSON $ RescanRes t
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

