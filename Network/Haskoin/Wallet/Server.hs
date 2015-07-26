{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans.Control (StM, MonadBaseControl, control, liftBaseOp)
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.DeepSeq (NFData(..), ($!!))
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
    , filterLogger
    )

import Data.HashMap.Strict ((!))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Aeson (Value, toJSON, decode, encode)
import Data.Conduit (Sink, awaitForever, ($$))
import Data.Conduit.TMChan (TBMChan, sourceTBMChan)
import Data.Word (Word32)
import qualified Data.Map.Strict as M 
    (Map, unionWith, null, toList, empty, fromListWith, assocs, elems)

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

data EventSession = EventSession
    { eventBatchSize :: !Int 
    , eventNewAddrs  :: !(M.Map KeyRingAccountId Word32)
    }
    deriving (Eq, Show, Read)

instance NFData EventSession where
    rnf EventSession{..} =
        rnf eventBatchSize `seq`
        rnf (M.elems eventNewAddrs)

initDatabase :: Config -> IO (Sem.MSem Int, ConnectionPool)
initDatabase cfg = do
    -- Create a semaphore with 1 resource
    sem <- Sem.new 1
    -- Create a database pool
    pool <- getDatabasePool $ configDatabase cfg ! pack networkName
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
    let logFilter _ level = level >= configLogLevel cfg
        runLogging = runStdoutLoggingT . filterLogger logFilter
        run = runResourceT . runLogging

    -- Check the operation mode of the server.
    case configMode cfg of
        -- In this mode, we do not launch an SPV node. We only accept
        -- client requests through the ZMQ API.
        SPVOffline -> run $ runWalletApp $ HandlerSession cfg pool Nothing sem
        -- In this mode, we launch the client ZMQ API and we sync the
        -- wallet database with an SPV node.
        SPVOnline -> do
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
            let nodes = configBTCNodes cfg ! pack networkName
                -- LevelDB options
                fp = "headertree"
                opts = DB.defaultOptions
                    { DB.createIfMissing = True
                    , DB.cacheSize       = 2048
                    , DB.writeBufferSize = 4096
                    }

            -- Compute our bloom filter
            bloom <- liftM fst3 $ runDBPool sem pool getBloomFilter

            -- Launch SPV node
            run $ withNode fp opts $ \eChan rChan -> do
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
                let eventSession = EventSession { eventBatchSize = 500 
                                                , eventNewAddrs  = M.empty
                                                }
                -- Set the batch size to 500
                liftIO . atomically $ writeTBMChan rChan $
                    NodeBatchSize $ eventBatchSize eventSession

                let runEvents = sourceTBMChan eChan $$ 
                                processEvents rChan sem pool

                withAsync (evalStateT runEvents eventSession) $ \a -> do
                    link a
                    -- Run the zeromq server listening to user requests
                    runWalletApp $ HandlerSession cfg pool (Just rChan) sem

processEvents :: (MonadLogger m, MonadIO m)
              => TBMChan NodeRequest -> Sem.MSem Int -> ConnectionPool
              -> Sink WalletMessage (StateT EventSession m) ()
processEvents rChan sem pool = awaitForever $ \req -> lift $ case req of
    WalletTx tx fromMerkle -> do
        -- Import all the transactions into the wallet as network transactions
        resM <- tryDBPool sem pool $ importNetTx tx
        let (_, newAddrs) = fromMaybe ([],[]) resM
        unless (null newAddrs) $ if fromMerkle
            -- Save that we got new addresses in the current merkle block
            then do
                oldMap <- gets eventNewAddrs
                let newMap = M.unionWith (+) oldMap $ groupByAcc newAddrs
                modify' $ \s -> s{ eventNewAddrs = newMap }
            -- Send the bloom filter to peers when new addresses were created.
            else do
                bloomM <- tryDBPool sem pool getBloomFilter
                case bloomM of
                    Just (bloom, _, _) -> liftIO . atomically $ 
                        writeTBMChan rChan $ NodeBloomFilter bloom
                    _ -> return ()

    WalletGetTx txid -> do
        txM <- tryDBPool sem pool $ getTx txid
        case txM of
            Just tx -> liftIO $ atomically $ writeTBMChan rChan $
                NodeSendTx tx
            Nothing -> $(logDebug) $ pack $ unwords
                [ "Could not find transaction", encodeTxHashLE txid ]

    WalletMerkles action mTxs -> do
        -- Save the old best block before importing
        oldBestM <- tryDBPool sem pool getBestBlock
        let oldBest = maybe (headerHash genesisHeader) fst oldBestM

        -- Import the merkle blocks into the wallet
        _ <- tryDBPool sem pool $ importMerkles action mTxs

        -- Get the transaction state during tx import
        newAddrs <- gets eventNewAddrs

        -- Do we have to rescan the current batch ?
        rescan <- shouldRescan $ M.assocs newAddrs
                -- If we use addresses in the hidden gap, we must rescan this
                -- batch. oldBest could be equal to the genesis block which
                -- would trigger a download from height 0. That is fine.
        let bh | rescan = oldBest
               -- Otherwise, simply continue the merkle download from the new
               -- best block
               | otherwise = nodeBlockHash $ last $ actionNewNodes action

        -- Send a new bloom filter to our peers if new addresses were generated
        unless (M.null newAddrs) $ do
            bloomM <- tryDBPool sem pool getBloomFilter
            case bloomM of
                Just (bloom, _, _) -> liftIO . atomically $ 
                    writeTBMChan rChan $ NodeBloomFilter bloom
                _ -> return ()

        -- Reset the merkle block state
        modify' $ \s -> s{ eventNewAddrs = M.empty }

        bSize <- gets eventBatchSize
        let newBSize | rescan    = max 1 $ bSize `div` 2
                     | otherwise = min 500 $ bSize + max 1 (bSize `div` 20)
                
        when (newBSize /= bSize) $ do
            $(logDebug) $ pack $ unwords
                [ "Changing block batch size from"
                , show bSize, "to", show newBSize 
                ]
            liftIO . atomically $ writeTBMChan rChan $ NodeBatchSize newBSize
            modify' $ \s -> s{ eventBatchSize = newBSize }

        -- If we have expected txs in the merkle batch, the node will stop
        -- the download and wait for our instructions
        let dwnStopped = any (not . null) mTxs
        -- Continue the download for the given block if the download is stopped
        -- or we need to rescan.
        when (dwnStopped || rescan) $ liftIO . atomically $ 
            writeTBMChan rChan $ NodeStartMerkleDownload $ Right bh

    WalletSynced -> do
        txsM <- tryDBPool sem pool $ getPendingTxs 100
        case txsM of
            Just txs -> liftIO $ atomically $ writeTBMChan rChan $
                NodePublishTxs $ map txHash txs
            Nothing ->
                $(logDebug) $ pack "Failed to retrieve pending transactions"
    -- Ignore full blocks
    _ -> return ()
  where
    shouldRescan newAddrs = case newAddrs of
        ((ai, cnt):rest) -> do
            accM <- liftM join $ tryDBPool sem pool $ get ai
            -- For every account, check if we busted the gap.
            if maybe False ((cnt >) . keyRingAccountGap) accM
                then return True
                else shouldRescan rest
        _ -> return False
    groupByAcc addrs =
        let xs = map (\a -> (keyRingAddrAccount a, 1)) addrs
        in  M.fromListWith (+) xs

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
runWalletApp :: forall m.
                ( Monad m
                , MonadIO m
                , MonadLogger m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => HandlerSession -> m ()
runWalletApp session = liftBaseOp withContext f
  where
    f :: Context -> m ()
    f ctx = liftBaseOp (withSocket ctx Rep) g
    g :: Socket Rep -> m ()
    g sock = do
        liftIO $ bind sock $ configBind $ handlerConfig session
        forever $ do
            bs  <- liftIO $ receive sock
            res <- catchErrors $ h bs
            liftIO $ send sock [] $ toStrictBS $ encode res
    h :: ByteString -> m (WalletResponse Value)
    h bs = case decode $ toLazyBS bs of
        Just r  -> runHandler session $ dispatchRequest r
        Nothing -> return $ ResponseError "Could not decode request"
    catchErrors :: m (WalletResponse Value) -> m (WalletResponse Value)
    catchErrors m = control $ \runInIO -> E.catches (runInIO m) 
        [ E.Handler (\(WalletException err) -> runInIO
            (return $ ResponseError $ pack err :: m (WalletResponse Value)))
        , E.Handler (\(E.ErrorCall err) -> runInIO
            (return $ ResponseError $ pack err :: m (WalletResponse Value)))
        , E.Handler (\(E.SomeException exc) -> runInIO
            (return $ ResponseError $ pack $ show exc
                :: m (WalletResponse Value)))
        ]

dispatchRequest :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadThrow m
                   , MonadResource m
                   , MonadIO m
                   ) 
                => WalletRequest -> Handler m (WalletResponse Value)
dispatchRequest req = liftM ResponseValid $ case req of
    GetKeyRingsR                     -> getKeyRingsR
    GetKeyRingR r                    -> getKeyRingR r
    PostKeyRingsR r                  -> postKeyRingsR r
    GetAccountsR r                   -> getAccountsR r
    PostAccountsR r na               -> postAccountsR r na
    GetAccountR r n                  -> getAccountR r n
    PostAccountKeysR r n ks          -> postAccountKeysR r n ks
    PostAccountGapR r n g            -> postAccountGapR r n g
    GetAddressesR r n t m o p        -> getAddressesR r n t m o p
    GetAddressesUnusedR r n t        -> getAddressesUnusedR r n t
    GetAddressR r n i t m o          -> getAddressR r n i t m o
    PutAddressR r n i t l            -> putAddressR r n i t l
    GetTxsR r n p                    -> getTxsR r n p
    GetAddrTxsR r n i t p            -> getAddrTxsR r n i t p
    PostTxsR r n a                   -> postTxsR r n a
    GetTxR r n h                     -> getTxR r n h
    GetOfflineTxR r n h              -> getOfflineTxR r n h
    PostOfflineTxR r n t cs          -> postOfflineTxR r n t cs
    GetBalanceR r n mc o             -> getBalanceR r n mc o
    PostNodeR na                     -> postNodeR na

