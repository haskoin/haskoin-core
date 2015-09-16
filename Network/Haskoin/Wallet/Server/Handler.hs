module Network.Haskoin.Wallet.Server.Handler where

import Control.Arrow (first)
import Control.Monad (when, unless, liftM)
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
import qualified Data.Map.Strict as M (intersectionWith, fromList, elems)

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
import Network.Haskoin.Transaction
import Network.Haskoin.Block
import Network.Haskoin.Util
import Network.Haskoin.Node
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Node.BlockChain
import Network.Haskoin.Node.Peer

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.KeyRing
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
    res <- runDB keyRings
    return $ Just $ toJSON $ map (\k -> toJsonKeyRing k Nothing Nothing) res

getKeyRingR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> Handler m (Maybe Value)
getKeyRingR name = do
    $(logInfo) $ format $ unwords [ "GetKeyRingR", unpack name ]
    Entity _ keyRing <- runDB $ getKeyRing name
    return $ Just $ toJSON $ toJsonKeyRing keyRing Nothing Nothing

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
    keyRing <- runDB $ newKeyRing name seed
    return $ Just $ toJSON $ toJsonKeyRing keyRing Nothing (Just ms)
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
    (keyRing, accs) <- runDB $ do
        Entity ki keyRing <- getKeyRing keyRingName
        accs <- accounts ki
        return (keyRing, accs)
    return $ Just $ toJSON $ JsonWithKeyRing
        { withKeyRingKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withKeyRingData    = map toJsonAccount accs
        }

postAccountsR
    :: ( MonadResource m, MonadThrow m, MonadLogger m
       , MonadBaseControl IO m, MonadIO m
       )
    => KeyRingName -> NewAccount -> Handler m (Maybe Value)
postAccountsR keyRingName NewAccount{..} = do
    $(logInfo) $ format $ unlines
        [ "PostAccountsR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account type: " ++ show newAccountType
        , "  Account name: " ++ unpack newAccountAccountName
        ]
    (keyRing, Entity _ newAcc) <- runDB $ do
        keyRingE@(Entity ki keyRing) <- getKeyRing keyRingName
        newAcc <- newAccount
            keyRingE newAccountAccountName newAccountType newAccountKeys
        return (keyRing, newAcc)
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc) updateNodeFilter
    return $ Just $ toJSON $ JsonWithKeyRing
        { withKeyRingKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withKeyRingData    = toJsonAccount newAcc
        }

getAccountR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName -> Handler m (Maybe Value)
getAccountR keyRingName name = do
    $(logInfo) $ format $ unlines
        [ "GetAccountR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        ]
    (keyRing, Entity _ acc) <- runDB $ getAccount keyRingName name
    return $ Just $ toJSON $ JsonWithKeyRing
        { withKeyRingKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withKeyRingData    = toJsonAccount acc
        }

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
    (keyRing, newAcc) <- runDB $ do
        (keyRing, accE) <- getAccount keyRingName name
        newAcc <- addAccountKeys accE keys
        return (keyRing, newAcc)
    -- Update the bloom filter if the account is complete
    whenOnline $ when (isCompleteAccount newAcc) updateNodeFilter
    return $ Just $ toJSON $ JsonWithKeyRing
        { withKeyRingKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withKeyRingData    = toJsonAccount newAcc
        }

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
    (keyRing, newAcc) <- runDB $ do
        (keyRing, accE) <- getAccount keyRingName name
        newAcc <- setAccountGap accE gap
        return (keyRing, newAcc)
    -- Update the bloom filter
    whenOnline updateNodeFilter
    return $ Just $ toJSON $ JsonWithKeyRing
        { withKeyRingKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withKeyRingData    = toJsonAccount newAcc
        }

getAddressesR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
              => KeyRingName
              -> AccountName
              -> AddressType
              -> Word32
              -> Bool
              -> PageRequest
              -> Handler m (Maybe Value)
getAddressesR keyRingName name addrType minConf offline page = do
    $(logInfo) $ format $ unlines
        [ "GetAddressesR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Address type: " ++ show addrType
        , "  Page number : " ++ show (pageNum page)
        , "  Page size   : " ++ show (pageLen page)
        , "  Page reverse: " ++ show (pageReverse page)
        , "  MinConf     : " ++ show minConf
        , "  Offline     : " ++ show offline
        ]

    (keyRing, acc, res, bals, maxPage) <- runDB $ do
        (keyRing, accE@(Entity ai acc)) <- getAccount keyRingName name
        (res, maxPage) <- addressPage accE addrType page
        case res of
            [] -> return (keyRing, acc, res, [], maxPage)
            _ -> do
                let is = map keyRingAddrIndex res
                    (iMin, iMax) = (minimum is, maximum is)
                bals <- addressBalances accE iMin iMax addrType minConf offline
                return (keyRing, acc, res, bals, maxPage)

    -- Join addresses and balances together
    let g (addr, bal) = toJsonAddr addr (Just bal)
        addrBals = map g $ M.elems $ joinAddrs res bals
    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = PageRes addrBals maxPage
        }
  where
    joinAddrs addrs bals =
        let f addr = (keyRingAddrIndex addr, addr)
        in  M.intersectionWith (,) (M.fromList $ map f addrs) (M.fromList bals)

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

    (keyRing, acc, addrs) <- runDB $ do
        (keyRing, accE@(Entity _ acc)) <- getAccount keyRingName name
        addrs <- unusedAddresses accE addrType
        return (keyRing, acc, addrs)

    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = map (flip toJsonAddr Nothing) addrs
        }

getAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName -> AccountName -> KeyIndex -> AddressType
            -> Word32 -> Bool
            -> Handler m (Maybe Value)
getAddressR keyRingName name i addrType minConf offline = do
    $(logInfo) $ format $ unlines
        [ "GetAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Address type: " ++ show addrType
        ]

    (keyRing, acc, addr, balM) <- runDB $ do
        (keyRing, accE@(Entity _ acc)) <- getAccount keyRingName name
        addrE <- getAddress accE addrType i
        bals <- addressBalances accE i i addrType minConf offline
        return $ case bals of
            ((_,bal):_) -> (keyRing, acc, entityVal addrE, Just bal)
            _           -> (keyRing, acc, entityVal addrE, Nothing)
    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = toJsonAddr addr balM
        }

putAddressR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => KeyRingName
            -> AccountName
            -> KeyIndex
            -> AddressType
            -> AddressLabel
            -> Handler m (Maybe Value)
putAddressR keyRingName name i addrType (AddressLabel label) = do
    $(logInfo) $ format $ unlines
        [ "PutAddressR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        , "  Label       : " ++ unpack label
        ]

    (keyRing, acc, newAddr) <- runDB $ do
        (keyRing, accE@(Entity _ acc)) <- getAccount keyRingName name
        newAddr <- setAddrLabel accE i addrType label
        return (keyRing, acc, newAddr)

    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = toJsonAddr newAddr Nothing
        }

postAddressesR :: ( MonadLogger m
                  , MonadBaseControl IO m
                  , MonadIO m
                  , MonadThrow m
                  , MonadBase IO m
                  , MonadResource m
                  )
               => KeyRingName
               -> AccountName
               -> KeyIndex
               -> AddressType
               -> Handler m (Maybe Value)
postAddressesR keyRingName name i addrType = do
    $(logInfo) $ format $ unlines
        [ "PostAddressesR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Index       : " ++ show i
        ]

    (keyRing, acc, cnt) <- runDB $ do
        (keyRing, accE@(Entity _ acc)) <- getAccount keyRingName name
        cnt <- generateAddrs accE addrType i
        return (keyRing, acc, cnt)

    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = cnt
        }

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

    (keyRing, acc, res, maxPage, height) <- runDB $ do
        (keyRing, Entity ai acc) <- getAccount keyRingName name
        (_, height) <- getBestBlock
        (res, maxPage) <- txPage ai page
        return (keyRing, acc, res, maxPage, height)

    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData =
            PageRes (map (flip toJsonTx (Just height)) res) maxPage
        }

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

    (keyRing, acc, res, maxPage, height, addr) <- runDB $ do
        (keyRing, accE@(Entity _ acc)) <- getAccount keyRingName name
        Entity addrI addr <- getAddress accE addrType index
        (_, height) <- getBestBlock
        (res, maxPage) <- addrTxPage accE addrI page
        return (keyRing, acc, res, maxPage, height, addr)

    return $ Just $ toJSON $ JsonWithAddr
        { withAddrKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAddrAccount = toJsonAccount acc
        , withAddrAddress = toJsonAddr addr Nothing
        , withAddrData    = PageRes (map (f height) res) maxPage
        }
  where
    f height (tx, bal) = AddrTx (toJsonTx tx (Just height)) bal

postTxsR :: ( MonadLogger m, MonadBaseControl IO m, MonadBase IO m
            , MonadIO m, MonadThrow m, MonadResource m
            )
         => KeyRingName -> AccountName -> TxAction -> Handler m (Maybe Value)
postTxsR keyRingName name action = do
    (keyRing, accE@(Entity ai acc), height) <- runDB $ do
        (keyRing, accE) <- getAccount keyRingName name
        (_, height) <- getBestBlock
        return (keyRing, accE, height)

    (txRes, newAddrs) <- case action of
        CreateTx rs fee minconf rcptFee sign -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR CreateTx"
                , "  KeyRing name: " ++ unpack keyRingName
                , "  Account name: " ++ unpack name
                , "  Recipients  : " ++ show (map (first addrToBase58) rs)
                , "  Fee         : " ++ show fee
                , "  Minconf     : " ++ show minconf
                , "  Rcpt. Fee   : " ++ show rcptFee
                , "  Sign        : " ++ show sign
                ]
            runDB $ createTx keyRing accE rs fee minconf rcptFee sign
        ImportTx tx -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR ImportTx"
                , "  KeyRing name: " ++ unpack keyRingName
                , "  Account name: " ++ unpack name
                , "  Txid        : " ++ encodeTxHashLE (txHash tx)
                ]
            runDB $ do
                (res, newAddrs) <- importTx tx ai
                case filter ((== ai) . keyRingTxAccount) res of
                    (txRes:_) -> return (txRes, newAddrs)
                    _ -> liftIO . throwIO $ WalletException
                        "Could not import the transaction"
        SignTx txid -> do
            $(logInfo) $ format $ unlines
                [ "PostTxsR SignTx"
                , "  KeyRing name: " ++ unpack keyRingName
                , "  Account name: " ++ unpack name
                , "  Txid        : " ++ encodeTxHashLE txid
                ]
            runDB $ do
                (res, newAddrs) <- signKeyRingTx keyRing accE txid
                case filter ((== ai) . keyRingTxAccount) res of
                    (txRes:_) -> return (txRes, newAddrs)
                    _ -> liftIO . throwIO $ WalletException
                        "Could not import the transaction"
    whenOnline $ do
        -- Update the bloom filter
        unless (null newAddrs) updateNodeFilter
        -- If the transaction is pending, broadcast it to the network
        when (keyRingTxConfidence txRes == TxPending) $
            runNode $ broadcastTxs [keyRingTxHash txRes]
    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = toJsonTx txRes (Just height)
        }

getTxR :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
       => KeyRingName -> AccountName -> TxHash -> Handler m (Maybe Value)
getTxR keyRingName name txid = do
    $(logInfo) $ format $ unlines
        [ "GetTxR"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack name
        , "  Txid        : " ++ encodeTxHashLE txid
        ]
    (keyRing, acc, res, height) <- runDB $ do
        (keyRing, Entity ai acc) <- getAccount keyRingName name
        (_, height) <- getBestBlock
        res <- getAccountTx ai txid
        return (keyRing, acc, res, height)
    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = toJsonTx res (Just height)
        }

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
    (keyRing, acc, bal) <- runDB $ do
        (keyRing, Entity ai acc) <- getAccount keyRingName name
        bal <- accountBalance ai minconf offline
        return (keyRing, acc, bal)
    return $ Just $ toJSON $ JsonWithAccount
        { withAccountKeyRing = toJsonKeyRing keyRing Nothing Nothing
        , withAccountAccount = toJsonAccount acc
        , withAccountData    = bal
        }

getOfflineTxR :: ( MonadLogger m, MonadIO m, MonadBaseControl IO m
                 , MonadBase IO m, MonadThrow m, MonadResource m
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

postOfflineTxR :: ( MonadLogger m, MonadIO m, MonadBaseControl IO m
                  , MonadBase IO m, MonadThrow m, MonadResource m
                  )
               => KeyRingName
               -> AccountName
               -> Tx
               -> [CoinSignData]
               -> Handler m (Maybe Value)
postOfflineTxR keyRingName accountName tx signData = do
    $(logInfo) $ format $ unlines
        [ "PostTxsR SignOfflineTx"
        , "  KeyRing name: " ++ unpack keyRingName
        , "  Account name: " ++ unpack accountName
        , "  Txid        : " ++ encodeTxHashLE (txHash tx)
        ]
    (keyRing, Entity _ acc) <- runDB $ getAccount keyRingName accountName
    let signedTx = signOfflineTx keyRing acc tx signData
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

updateNodeFilter :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                 => Handler m ()
updateNodeFilter = do
    $(logInfo) $ format "Sending a new bloom filter"
    (bloom, _, _) <- runDB getBloomFilter
    runNode $ atomicallyNodeT $ sendBloomFilter bloom

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

