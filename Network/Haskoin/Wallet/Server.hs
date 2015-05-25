module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import System.ZMQ4

import Control.Applicative ((<$>))
import Control.Monad (when, unless, forM, forM_, forever, liftM)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Exception (SomeException(..),  tryJust, catch)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl, control, liftBaseOp)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
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
import Data.Maybe (catMaybes)
import Data.Aeson (Value, toJSON, decode, encode)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (TBMChan, sourceTBMChan)
import qualified Data.Map.Strict as M (unionsWith, null, toList, empty)

import Database.Persist (get)
import Database.Persist.Sql 
    ( ConnectionPool
    , runSqlPersistMPool
    , runMigration
    )
import qualified Database.LevelDB.Base as DB 
    ( DB
    , Options(..)
    , open
    , defaultOptions 
    )

import Network.Haskoin.Constants
import Network.Haskoin.Node
import Network.Haskoin.Util
import Network.Haskoin.Block

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

runSPVServer :: Config -> IO ()
runSPVServer cfg = do
    -- Start server process
    maybeDetach cfg $ do
        -- Get database pool
        pool <- getDatabasePool $ configDatabase cfg
        
        -- Initialize wallet database
        flip runSqlPersistMPool pool $ do 
            _ <- runMigration migrateWallet 
            initWallet $ configBloomFP cfg

        let logFilter level = level >= configLogLevel cfg
            runLogging = runStdoutLoggingT . filterLevel logFilter
            run        = runResourceT . runLogging

        if configMode cfg == SPVOffline
            then run $ runWalletApp $ HandlerSession cfg pool Nothing
            else do
                -- Create leveldb handle
                db <- DB.open "headertree"
                    DB.defaultOptions{ DB.createIfMissing = True
                                     , DB.cacheSize       = 2048
                                     }

                -- Get our best block or compute a fast catchup time otherwise
                (best, height) <- runSqlPersistMPool getBestBlock pool
                dwnE <- if height > 0
                    -- If we have a best block, use it to download merkles
                    then return $ Right best
                    -- Otherwise, give the node a fast catchup time
                    else do
                        fstKeyTimeM <- runSqlPersistMPool firstAddrTime pool
                        Left . adjustFCTime <$> case fstKeyTimeM of
                            Just ts  -> return ts
                            -- If we have no keys, use the current time
                            Nothing -> round <$> getPOSIXTime

                    -- Bitcoin nodes to connect to
                let nodes = configBTCNodes cfg 
                    -- Run the SPV monad stack
                    runNode = run . (flip evalStateT db)

                -- Compute our bloom filter
                bloom <- liftM fst3 $ runSqlPersistMPool getBloomFilter pool

                -- Launch SPV node
                runNode $ withSpvNode $ \eChan rChan -> do
                    -- Connect to remote nodes
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeConnectPeers $ map (\(h,p) -> RemoteHost h p) nodes
                    -- Send our bloom filter
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeBloomFilter bloom
                    -- Start the merkle block download process
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeStartDownload dwnE

                    -- Listen to SPV events and update the wallet database
                    let runEvents = sourceTBMChan eChan $$ 
                                    processEvents rChan db pool

                    withAsync runEvents $ \a -> do
                        link a
                        -- Run the zeromq server listening to user requests
                        runWalletApp $ HandlerSession cfg pool $ Just rChan

processEvents :: (MonadLogger m, MonadIO m)
              => TBMChan NodeRequest -> DB.DB -> ConnectionPool
              -> Sink WalletMessage m ()
processEvents rChan db pool = awaitForever $ \req -> lift $ case req of
    WalletTx tx -> processTxs [tx] >> return ()
    WalletMerkle action dmbs -> do
        -- Save the old best block before importing
        oldBestE <- liftIO $ tryJust f $ runSqlPersistMPool getBestBlock pool
        oldBest <- case oldBestE of
            Right (ob, _) -> return ob
            Left err -> do
                $(logError) $ pack $ unwords
                    [ "processEvents: An error occured:", err ]
                return $ headerHash genesisHeader

        -- Import all transactions into the wallet
        let txs = concat $ map merkleTxs dmbs
        xs <- processTxs txs

        -- Import the merkle blocks into the wallet
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ 
            importMerkles action $ map expectedTxs dmbs
        when (isLeft resE) $ $(logError) $ pack $ unwords
            [ "processEvents: An error occured:", fromLeft resE ]

        -- If we received at least 1 transaction, the node will block the
        -- merkle block download and wait for us to continue the download.
        unless (null txs) $ do
            -- For every account, check if we busted the gap.
            scanE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $
                forM (M.toList xs) $ \(ai, cnt) -> do
                    accM <- get ai
                    return $ maybe False ((cnt >) . keyRingAccountGap) accM

            case scanE of
                Right ys -> do
                    when (or ys) $ $(logDebug) $ pack $ unwords
                        [ "Generated addresses beyond the account gap."
                        , "Rescanning this batch."
                        ]
                           -- If we use addresses in the hidden gap, we must
                           -- rescan this batch.
                    let bh | or ys = oldBest
                           -- Otherwise, simply continue the merkle download
                           -- from the new best block
                           | otherwise = 
                               nodeBlockHash $ last $ actionNewNodes action
                    -- Send a message to the node to continue the download from
                    -- the requested block hash
                    liftIO . atomically $ writeTBMChan rChan $ 
                        NodeStartDownload $ Right bh
                Left err -> $(logError) $ pack $ unwords
                    [ "processEvents: An error occured:", err]

    _ -> return () -- Ignore full blocks
  where
    processTxs txs = do
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ do
            -- Import all transactions into the wallet as network transactions
            xs <- liftM (map snd . catMaybes) $ forM txs importNetTx
            (bloom, _, _) <- getBloomFilter
            -- Return the count of new addresses created for each account
            return (bloom, M.unionsWith (+) xs)
        case resE of
            Right (bloom, xs) -> do
                -- When new addresses were created, we need to send the new
                -- bloom filter to our peers.
                unless (M.null xs) $ liftIO . atomically $ 
                    writeTBMChan rChan $ NodeBloomFilter bloom
                return xs
            Left err -> do
                $(logError) $ pack $ unwords 
                    [ "processEvents: An error occured:", err ]
                return $ M.empty
    f (SomeException e) = Just $ show e

maybeDetach :: Config -> IO () -> IO ()
maybeDetach cfg action =
    if configDetach cfg then runDetached pidFile logFile action else action
  where
    pidFile = Just $ configPidFile cfg
    logFile = ToFile $ configLogFile cfg

stopSPVServer :: Config -> IO ()
stopSPVServer cfg = do
    -- TODO: Should we send a message instead of killing the process ?
    killAndWait $ configPidFile cfg

-- Run the main ZeroMQ loop
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
    -- TODO: Catch ErrorCall and SomeException
    catchErrors m = control $ \runInIO -> catch (runInIO m) $ 
        \(WalletException err) -> runInIO $ return $ ResponseError $ pack err

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
    PostTxsR r n a                   -> postTxsR r n a
    GetTxR r n h                     -> getTxR r n h
    GetOfflineTxR r n h              -> getOfflineTxR r n h
    GetBalanceR r n mc               -> getBalanceR r n mc
    GetOfflineBalanceR r n           -> getOfflineBalanceR r n
    PostNodeR na                     -> postNodeR na

