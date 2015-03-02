module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Posix.Env (getEnv)
import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import System.ZMQ4

import Control.Applicative ((<$>))
import Control.Monad (when, forM, forever, filterM, liftM)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Exception (SomeException(..),  tryJust, catch)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (withAsync, link)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl, control, liftBaseOp)
import Control.Monad.Logger 
    ( MonadLogger
    , LoggingT
    , runStdoutLoggingT
    , logError
    )

import Data.Text (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Aeson (Value, toJSON, decode, encode)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (TBMChan, sourceTBMChan)

import Yesod.Default.Config2 (loadAppSettings, useEnv)
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

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server.Handler
import Network.Haskoin.Wallet.Database

runLogging :: MonadIO m => LoggingT m a -> m a 
runLogging = runStdoutLoggingT

runSPVServer :: Config -> IO ()
runSPVServer cfg = do
    -- Start server process
    maybeDetach cfg $ do
        -- Get database pool
        pool <- getDatabasePool $ configDatabase cfg
        
        -- Initialize wallet database
        flip runSqlPersistMPool pool $ do 
            _ <- runMigration migrateWallet 
            initWalletDB

        if configMode cfg == SPVOffline
            then runLogging $ runWalletApp $ HandlerSession cfg pool Nothing
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
                        fstKeyTimeM <- runSqlPersistMPool firstKeyTime pool
                        Left . adjustFCTime <$> case fstKeyTimeM of
                            Just ts  -> return ts
                            -- If we have no keys, use the current time
                            Nothing -> round <$> getPOSIXTime

                    -- Bloom filter false positive rate
                let fp    = configBloomFP cfg 
                    -- Bitcoin nodes to connect to
                    nodes = configBTCNodes cfg 
                    -- Run the SPV monad stack
                    runNode = runLogging . (flip evalStateT db)

                -- Compute our bloom filter
                bloom <- runSqlPersistMPool (walletBloomFilter fp) pool

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
                    let runEvents =  sourceTBMChan eChan 
                                  $$ processEvents rChan db pool fp

                    withAsync runEvents $ \a -> do
                        link a
                        -- Run the zeromq server listening to user requests
                        runWalletApp $ HandlerSession cfg pool $ Just rChan

processEvents :: (MonadLogger m, MonadIO m)
              => TBMChan NodeRequest 
              -> DB.DB -> ConnectionPool -> Double -> Sink WalletMessage m ()
processEvents rChan db pool fp = awaitForever $ \req -> lift $ case req of
    WalletTx tx -> goTxs [tx]
    WalletMerkle action dmbs -> do
        -- Import all transactions into the wallet
        goTxs $ concat (map merkleTxs dmbs)
        -- Import the merkle blocks into the wallet
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ 
            importBlocks action $ map expectedTxs dmbs
        when (isLeft resE) $ $(logError) $ pack $ unwords
            [ "processEvents: An error occured:", fromLeft resE ]
    _ -> return () -- Ignore full blocks
  where
    goTxs txs = do
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ do
            xs <- forM txs $ \tx -> importTx tx SourceNetwork Nothing
            -- Update the bloom filter if new addresses were generated
            -- TODO: If all gap addresses have been used up, we need to
            -- issue a rescan.
            if or $ map lst3 $ catMaybes xs
                then Just <$> walletBloomFilter fp
                else return Nothing
        case resE of
            Left err -> $(logError) $ pack $ unwords
                [ "processEvents: An error occured:", err ]
            Right (Just bloom) -> liftIO . atomically $ writeTBMChan rChan $ 
                    NodeBloomFilter bloom
            _ -> return ()
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
runWalletApp :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) 
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

dispatchRequest :: (MonadLogger m, MonadIO m) 
                => WalletRequest -> Handler m (WalletResponse Value)
dispatchRequest req = liftM (ResponseValid . toJSON) $ case req of
    GetWalletsR                      -> getWalletsR
    GetWalletR w                     -> getWalletR w
    PostWalletsR nw                  -> postWalletsR nw
    GetAccountsR w                   -> getAccountsR w
    PostAccountsR w na               -> postAccountsR w na
    GetAccountR w n                  -> getAccountR w n
    PostAccountKeysR w n ks          -> postAccountKeysR w n ks
    GetAddressesR w n prM mc i ul us -> getAddressesR w n prM mc i ul us
    PostAddressesR w n ad            -> postAddressesR w n ad
    GetAddressR w n ix mc i          -> getAddressR w n ix mc i
    PutAddressR w n i ad             -> putAddressR w n i ad
    GetTxsR w n prM                  -> getTxsR w n prM
    PostTxsR w n ta                  -> postTxsR w n ta
    GetTxR w n h p                   -> getTxR w n h p
    GetOfflineTxDataR w n h          -> getOfflineTxDataR w n h
    GetBalanceR w n mc               -> getBalanceR w n mc
    GetSpendableR w n mc             -> getSpendableR w n mc
    PostNodeR na                     -> postNodeR na

