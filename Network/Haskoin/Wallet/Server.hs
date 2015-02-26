module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Directory (createDirectoryIfMissing)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files 
    ( fileExist
    , setFileMode
    , setFileCreationMask
    , unionFileModes
    , ownerModes
    , groupModes
    , otherModes
    )
import System.Posix.Env (getEnv)
import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import System.ZMQ4

import Control.Applicative ((<$>))
import Control.Monad (when, forM, forM_, forever, filterM, liftM)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Exception (SomeException(..),  tryJust, catch)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (Async, withAsync, link)
import Control.Monad.Trans.Control 
    ( MonadBaseControl
    , control
    , liftBaseOp
    , liftBaseWith
    )
import Control.Monad.Logger 
    ( MonadLogger
    , LoggingT
    , runStdoutLoggingT
    , logInfo
    , logWarn
    , logDebug
    , logError
    )
import Control.Monad.State (evalStateT)

import Yesod.Default.Config2 (loadAppSettings, useEnv)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe, maybeToList)
import qualified Data.Text as T(pack)
import Data.Aeson (Value, toJSON, decode, encode)

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
import Data.Conduit 
    ( Sink
    , awaitForever
    , mapOutput
    , ($$) 
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , writeTBMChan
    , newTBMChan
    , sourceTBMChan
    )

import Network.Haskoin.Constants
import Network.Haskoin.Block
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

runSPVServer :: Maybe FilePath -> Bool -> IO ()
runSPVServer configM detach = do
    -- Get configuration file
    config <- getSPVConfig configM
    -- Change current working directory
    setWorkDir config

    -- Start server process
    maybeDetach config detach $ do
        -- Get database pool
        pool <- getDatabasePool $ spvDatabase config
        
        -- Initialize wallet database
        flip runSqlPersistMPool pool $ do 
            _ <- runMigration migrateWallet 
            initWalletDB
        
        if spvMode config == SPVOffline
            then runLogging $ runWalletApp $ HandlerSession config pool Nothing
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
                    else Left <$> getFastCatchup pool

                    -- Bloom filter false positive rate
                let fp    = spvBloomFP config 
                    -- Bitcoin nodes to connect to
                    nodes = spvBitcoinNodes config 
                    -- Run the SPV monad stack
                    runNode = runLogging . (flip evalStateT db)

                -- Compute our bloom filter
                bloom <- runSqlPersistMPool (walletBloomFilter fp) pool

                -- Launch SPV node
                runNode $ withSpvNode $ \eChan rChan -> do
                    -- Send our bloom filter
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeBloomFilter bloom
                    -- Connect to remote nodes
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeConnectPeers $ map (\(h,p) -> RemoteHost h p) nodes
                    -- Start the merkle block download process
                    liftIO . atomically $ writeTBMChan rChan $
                        NodeStartDownload dwnE

                    -- Listen to SPV events and update the wallet database
                    let runEvents =  sourceTBMChan eChan 
                                  $$ processEvents rChan db pool fp

                    withAsync runEvents $ \a -> do
                        link a
                        -- Run the zeromq server listening to user requests
                        runWalletApp $ HandlerSession config pool $ Just rChan

getFastCatchup :: ConnectionPool -> IO Timestamp
getFastCatchup pool = do
    fstKeyTimeM <- runSqlPersistMPool firstKeyTime pool
    ts <- case fstKeyTimeM of
        Just ts  -> return ts
        -- If we have no keys, use the current time
        Nothing -> round <$> getPOSIXTime
    -- Adjust time backwards by a week to handle clock drifts.
    return $ fromInteger $ max 0 $ (toInteger ts) - 86400 * 7

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
        when (isLeft resE) $ $(logError) $ T.pack $ unwords
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
            Left err -> $(logError) $ T.pack $ unwords
                [ "processEvents: An error occured:", err ]
            Right (Just bloom) -> liftIO . atomically $ writeTBMChan rChan $ 
                    NodeBloomFilter bloom
            _ -> return ()
    f (SomeException e) = Just $ show e

maybeDetach :: SPVConfig -> Bool -> IO () -> IO ()
maybeDetach config det action =
    if det then runDetached pidFile logFile action else action
  where
    pidFile = Just $ spvPidFile config
    logFile = ToFile $ spvLogFile config

stopSPVServer :: Maybe FilePath -> IO ()
stopSPVServer configM = do
    -- Get configuration file
    config <- getSPVConfig configM
    -- Change current working directory
    setWorkDir config
    -- TODO: Should we send a message instead of killing the process ?
    killAndWait $ spvPidFile config

-- Get the server configuration from the following sources:
-- * File provided at the command line
-- * File specified in the configuration file at compile time
-- * Default configuration values specified at compile time
getSPVConfig :: Maybe FilePath -> IO SPVConfig
getSPVConfig configM = do
    changeWorkingDirectory . (fromMaybe err) =<< getEnv "HOME"
    validLocs <- liftIO $ filterM fileExist locs
    let files = maybeToList configM ++ validLocs
    loadAppSettings files [configSettingsYmlValue] useEnv
  where
    err = "No HOME environment variable"
    defCfgFile = spvConfigFile compileTimeSPVConfig
    -- Look for the config file in . and work-dir/network
    locs = [ defCfgFile
           , concat [ spvWorkDir compileTimeSPVConfig
                    , "/", networkName
                    , "/", defCfgFile
                    ]
           ]

-- Create and change current working directory
setWorkDir :: SPVConfig -> IO ()
setWorkDir config = do
    let workDir = concat [ spvWorkDir config, "/", networkName ]
    _ <- setFileCreationMask $ otherModes `unionFileModes` groupModes
    createDirectoryIfMissing True workDir
    setFileMode workDir ownerModes
    changeWorkingDirectory workDir

-- Run the main ZeroMQ loop
runWalletApp :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) 
             => HandlerSession -> m ()
runWalletApp session = liftBaseOp withContext $ \ctx -> 
    liftBaseOp (withSocket ctx Rep) $ \sock -> do
        liftIO $ bind sock $ spvBind $ handlerConfig session
        forever $ do
            bs  <- liftIO $ receive sock
            res <- catchErrors $ case decode $ toLazyBS bs of
                Just r  -> runHandler session $ dispatchRequest r
                Nothing -> return $ ResponseError "Could not decode request"
            liftIO $ send sock [] $ toStrictBS $ encode res
  where
    -- TODO: Catch ErrorCall and SomeException
    catchErrors m = control $ \runInIO -> catch (runInIO m) $ 
        \(WalletException err) -> runInIO $ return $ ResponseError $ T.pack err

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

