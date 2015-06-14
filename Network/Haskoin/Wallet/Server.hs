module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import System.ZMQ4

import Control.Applicative ((<$>))
import Control.Monad (when, void, unless, forM, forM_, forever, liftM, join)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl, control, liftBaseOp)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Control.Exception as E 
    (SomeException(..), ErrorCall(..), Handler(..), catches)
import qualified Control.Concurrent.MSem as Sem (MSem, new)
import Control.Monad.Logger 
    ( MonadLogger
    , runStdoutLoggingT
    , LogLevel(..)
    , LoggingT(..)
    , logError
    , logDebug
    )

import Data.Text (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Aeson (Value, toJSON, decode, encode)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (TBMChan, sourceTBMChan)
import qualified Data.Map.Strict as M (unionsWith, null, toList, empty)

import Database.Persist (get)
import Database.Persist.Sql (ConnectionPool, runMigration)
import qualified Database.LevelDB.Base as DB (Options(..), open, defaultOptions)

import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Node
import Network.Haskoin.Util
import Network.Haskoin.Block
import Network.Haskoin.Transaction

import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Transaction
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server.Handler
import Network.Haskoin.Wallet.Database

-- Filter logs by their log level
filterLevel :: (LogLevel -> Bool) -> LoggingT m a -> LoggingT m a
filterLevel p (LoggingT f) = LoggingT $ \logger ->
    f $ \loc src level msg -> when (p level) $ logger loc src level msg

initDatabase :: Config -> IO (Sem.MSem Int, ConnectionPool)
initDatabase cfg = do
    -- Create a semaphore with 1 resource
    sem <- Sem.new 1
    -- Create a database pool
    pool <- getDatabasePool $ configDatabase cfg
    -- Initialize wallet database
    runDBPool sem pool $ do
        _ <- runMigration migrateWallet 
        initWallet $ configBloomFP cfg
    -- Return the semaphrone and the connection pool
    return (sem, pool)

runSPVServer :: Config -> IO ()
runSPVServer cfg = maybeDetach cfg $ do -- start the server process
    -- Initialize the database
    (sem, pool) <- initDatabase cfg

    -- Setup logging monads
    let logFilter level = level >= configLogLevel cfg
        runLogging = runStdoutLoggingT . filterLevel logFilter
        run = runResourceT . runLogging

    -- Check the operation mode of the server.
    case configMode cfg of
        -- In this mode, we do not launch an SPV node. We only accept
        -- client requests through the ZMQ API.
        SPVOffline -> run $ runWalletApp $ HandlerSession cfg pool Nothing sem
        -- In this mode, we launch the client ZMQ API and we sync the
        -- wallet database with an SPV node.
        SPVOnline -> do
            -- Create leveldb handle
            db <- DB.open "headertree"
                DB.defaultOptions{ DB.createIfMissing = True
                                 , DB.cacheSize       = 2048
                                 }

            -- Get our best block or compute a fast catchup time otherwise
            (best, height) <- runDBPool sem pool getBestBlock
            dwnE <- if height > 0
                -- If we have a best block, use it to download merkles
                then return $ Right best
                -- Otherwise, give the node a fast catchup time
                else do
                    fstKeyTimeM <- runDBPool sem pool firstAddrTime
                    Left . adjustFCTime <$> case fstKeyTimeM of
                        Just ts  -> return ts
                        -- If we have no keys, use the current time
                        Nothing -> round <$> getPOSIXTime

                -- Bitcoin nodes to connect to
            let nodes = configBTCNodes cfg 
                -- Run the SPV monad stack
                runNode = run . (`evalStateT` db)

            -- Compute our bloom filter
            bloom <- liftM fst3 $ runDBPool sem pool getBloomFilter

            -- Launch SPV node
            runNode $ withNode $ \eChan rChan -> do
                -- Connect to remote nodes
                liftIO . atomically $ writeTBMChan rChan $
                    NodeConnectPeers $ map (uncurry RemoteHost) nodes
                -- Send our bloom filter
                liftIO . atomically $ writeTBMChan rChan $
                    NodeBloomFilter bloom
                -- Start the merkle block download process
                liftIO . atomically $ writeTBMChan rChan $
                    NodeStartMerkleDownload dwnE

                -- Listen to SPV events and update the wallet database
                let runEvents = sourceTBMChan eChan $$ 
                                processEvents rChan sem pool

                withAsync runEvents $ \a -> do
                    link a
                    -- Run the zeromq server listening to user requests
                    runWalletApp $ HandlerSession cfg pool (Just rChan) sem

processEvents :: (MonadLogger m, MonadIO m, Functor m)
              => TBMChan NodeRequest -> Sem.MSem Int -> ConnectionPool
              -> Sink WalletMessage m ()
processEvents rChan sem pool = awaitForever $ \req -> lift $ case req of
    WalletTx tx -> void (processTxs [tx])
    WalletGetTx txid -> do
        txM <- tryDBPool sem pool $ getTx txid
        case txM of
            Just tx -> liftIO $ atomically $ writeTBMChan rChan $
                NodeSendTx tx
            Nothing -> $(logDebug) $ pack $ unwords
                [ "Could not find transaction", encodeTxHashLE txid ]
    WalletMerkles action dmbs -> do
        -- Save the old best block before importing
        oldBestM <- tryDBPool sem pool getBestBlock
        let oldBest = maybe (headerHash genesisHeader) fst oldBestM

        -- Import all transactions into the wallet
        let txs = concatMap merkleTxs dmbs
        newAddrsMap <- processTxs txs

        -- Import the merkle blocks into the wallet
        _ <- tryDBPool sem pool $ importMerkles action $ map expectedTxs dmbs

        -- If we received at least 1 transaction, the node will block the
        -- merkle block download and wait for us to continue the download.
        unless (null txs) $ do
            -- Do we have to rescan the current batch ?
            rescan <- shouldRescan $ M.toList newAddrsMap
                    -- If we use addresses in the hidden gap, we must
                    -- rescan this batch.
            let bh | rescan = oldBest
                   -- Otherwise, simply continue the merkle download
                   -- from the new best block
                   | otherwise = nodeBlockHash $ last $ actionNewNodes action
            when rescan $ $(logDebug) $ pack $ unwords
                [ "Generated addresses beyond the account gap."
                , "Rescanning this batch."
                ]
            -- Send a message to the node to continue the download from
            -- the requested block hash
            liftIO . atomically $ 
                writeTBMChan rChan $ NodeStartMerkleDownload $ Right bh
    WalletSynced -> do
        txsM <- tryDBPool sem pool getPendingTxs
        case txsM of
            Just txs -> liftIO $ atomically $ writeTBMChan rChan $
                NodePublishTxs $ map txHash txs
            Nothing -> do
                $(logDebug) $ pack $ "Failed to retrieve pending transactions"
                return ()
    -- Ignore full blocks
    _ -> return ()
  where
    shouldRescan accs = case accs of
        ((ai, cnt):rest) -> do
            accM <- liftM join $ tryDBPool sem pool $ get ai
            -- For every account, check if we busted the gap.
            if maybe False ((cnt >) . keyRingAccountGap) accM
                then return True
                else shouldRescan rest
        _ -> return False
    processTxs txs = do
        -- Import all the transactions into the wallet as network transactions
        resM <- liftM catMaybes $ forM txs $ tryDBPool sem pool . importNetTx
        let newAddrsMap = M.unionsWith (+) $ map snd $ catMaybes resM
        -- Send the bloom filter to peers when new addresses were created
        unless (M.null newAddrsMap) $ do
            bloomM <- tryDBPool sem pool $ getBloomFilter
            case bloomM of
                Just (bloom, _, _) -> liftIO . atomically $ 
                    writeTBMChan rChan $ NodeBloomFilter bloom
                _ -> return ()
        return newAddrsMap

maybeDetach :: Config -> IO () -> IO ()
maybeDetach cfg action =
    if configDetach cfg then runDetached pidFile logFile action else action
  where
    pidFile = Just $ configPidFile cfg
    logFile = ToFile $ configLogFile cfg

stopSPVServer :: Config -> IO ()
stopSPVServer cfg =
    -- TODO: Should we send a message instead of killing the process ?
    killAndWait $ configPidFile cfg

-- Run the main ZeroMQ loop
-- TODO: Support concurrent requests using DEALER socket when we can do
-- concurrent MySQL requests.
runWalletApp :: ( MonadIO m
                , MonadLogger m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => HandlerSession -> m ()
runWalletApp session = liftBaseOp withContext $ \ctx -> 
    liftBaseOp (withSocket ctx Rep) $ \sock -> do
        liftIO $ bind sock $ configBind $ handlerConfig session
        forever $ do
            bs  <- liftIO $ receive sock
            res <- catchErrors $ case decode $ toLazyBS bs of
                Just r  -> runHandler session $ dispatchRequest r
                Nothing -> return $ ResponseError "Could not decode request"
            liftIO $ send sock [] $ toStrictBS $ encode res
  where
    catchErrors m = control $ \runInIO -> E.catches (runInIO m) 
        [ E.Handler (\(WalletException err) -> runInIO $ 
            return $ ResponseError $ pack err)
        , E.Handler (\(E.ErrorCall err) -> runInIO $ 
            return $ ResponseError $ pack err)
        , E.Handler (\(E.SomeException exc) -> runInIO $ 
            return $ ResponseError $ pack $ show exc)
        ]

dispatchRequest :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadThrow m
                   , MonadResource m
                   , MonadIO m
                   ) 
                => WalletRequest -> Handler m (WalletResponse Value)
dispatchRequest req = liftM (ResponseValid . toJSON) $ case req of
    GetKeyRingsR                     -> getKeyRingsR
    GetKeyRingR r                    -> getKeyRingR r
    PostKeyRingsR r                  -> postKeyRingsR r
    GetAccountsR r                   -> getAccountsR r
    PostAccountsR r na               -> postAccountsR r na
    GetAccountR r n                  -> getAccountR r n
    PostAccountKeysR r n ks          -> postAccountKeysR r n ks
    PostAccountGapR r n g            -> postAccountGapR r n g
    GetAddressesR r n t p            -> getAddressesR r n t p
    GetAddressesUnusedR r n t        -> getAddressesUnusedR r n t
    GetAddressR r n i t              -> getAddressR r n i t
    PutAddressR r n i t l            -> putAddressR r n i t l
    GetTxsR r n p                    -> getTxsR r n p
    GetAddrTxsR r n i t p            -> getAddrTxsR r n i t p
    PostTxsR r n a                   -> postTxsR r n a
    GetTxR r n h                     -> getTxR r n h
    GetOfflineTxR r n h              -> getOfflineTxR r n h
    GetBalanceR r n mc               -> getBalanceR r n mc
    GetOfflineBalanceR r n           -> getOfflineBalanceR r n
    PostNodeR na                     -> postNodeR na

