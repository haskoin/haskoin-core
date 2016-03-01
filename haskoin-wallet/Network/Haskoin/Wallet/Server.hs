{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import Data.List.NonEmpty (NonEmpty((:|)))
import System.ZMQ4
    ( Context, Rep(..), KeyFormat(..), bind
    , receive, receiveMulti
    , send, sendMulti
    , setCurveServer, setCurveSecretKey
    , withContext, withSocket
    , z85Decode
    )

import Control.Monad (when, unless, forever, void)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOpDiscard)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.DeepSeq (NFData(..))
import Control.Concurrent.STM (retry)
import Control.Concurrent.Async.Lifted (async, waitAnyCancel)
import Control.Exception.Lifted (SomeException(..), ErrorCall(..), catches)
import qualified Control.Exception.Lifted as E (Handler(..))
import qualified Control.Concurrent.MSem as Sem (MSem, new)
import Control.Monad.Logger
    ( MonadLoggerIO
    , runStdoutLoggingT
    , logDebug
    , logError
    , logWarn
    , logInfo
    , filterLogger
    )

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import qualified Data.HashMap.Strict as H (lookup)
import Data.Text (pack)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Aeson (Value, decode, encode)
import Data.Conduit (await, awaitForever, ($$))
import Data.Word (Word32)
import qualified Data.Map.Strict as M
    (Map, unionWith, null, empty, fromListWith, assocs, elems)
import Data.String.Conversions (cs)

import Database.Persist.Sql (ConnectionPool, runMigration)
import qualified Database.LevelDB.Base as DB (Options(..), defaultOptions)

import Database.Esqueleto (from, where_, val , (^.), (==.), (&&.), (<=.))

import Network.Haskoin.Constants
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.BlockChain
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree

import Network.Haskoin.Wallet.Accounts
import Network.Haskoin.Wallet.Transaction
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server.Handler
import Network.Haskoin.Wallet.Database

data EventSession = EventSession
    { eventBatchSize :: !Int
    , eventNewAddrs  :: !(M.Map AccountId Word32)
    }
    deriving (Eq, Show, Read)

instance NFData EventSession where
    rnf EventSession{..} =
        rnf eventBatchSize `seq`
        rnf (M.elems eventNewAddrs)

runSPVServer :: Config -> IO ()
runSPVServer cfg = maybeDetach cfg $ run $ do -- start the server process
    -- Initialize the database
    -- Check the operation mode of the server.
    (sem, pool) <- initDatabase cfg
    case configMode cfg of
        -- In this mode, we do not launch an SPV node. We only accept
        -- client requests through the ZMQ API.
        SPVOffline -> runWalletApp $ HandlerSession cfg pool Nothing sem
        -- In this mode, we launch the client ZMQ API and we sync the
        -- wallet database with an SPV node.
        SPVOnline -> do
            -- Initialize the node state
            nodeState <- getNodeState fp opts
            -- Spin up the node threads
            as <- mapM async
                -- Start the SPV node
                [ runNodeT nodeState $ do
                    -- Get our bloom filter
                    (bloom, elems, _) <- runDBPool sem pool getBloomFilter
                    startSPVNode hosts bloom elems
                -- Merkle block synchronization
                , runMerkleSync nodeState sem pool
                -- Import solo transactions as they arrive from peers
                , runNodeT nodeState $ txSource $$ processTx sem pool
                -- Respond to transaction GetData requests
                , runNodeT nodeState $
                    handleGetData $ runDBPool sem pool . getTx
                -- Re-broadcast pending transactions
                , broadcastPendingTxs nodeState sem pool
                -- Run the ZMQ API server
                , runWalletApp $ HandlerSession cfg pool (Just nodeState) sem
                ]
            _ <- waitAnyCancel as
            return ()
  where
    -- Setup logging monads
    run               = runResourceT . runLogging
    runLogging        = runStdoutLoggingT . filterLogger logFilter
    logFilter _ level = level >= configLogLevel cfg
        -- Bitcoin nodes to connect to
    nodes = fromMaybe
        (error $ "BTC nodes for " ++ networkName ++ " not found")
        (pack networkName `H.lookup` configBTCNodes cfg)
    hosts = map (\x -> PeerHost (btcNodeHost x) (btcNodePort x)) nodes
    -- LevelDB options
    fp = "headertree"
    opts = DB.defaultOptions { DB.createIfMissing = True
                             , DB.cacheSize       = 2048
                             }
    -- Run the merkle syncing thread
    runMerkleSync nodeState sem pool = runNodeT nodeState $ do
        $(logDebug) "Waiting for a valid bloom filter for merkle downloads..."

        -- Only download merkles if we have a valid bloom filter
        _ <- atomicallyNodeT waitBloomFilter

        -- Provide a fast catchup time if we are at height 0
        fcM <- fmap (fmap adjustFCTime) $ runDBPool sem pool $ do
            (_, h) <- getBestBlock
            if h == 0 then firstAddrTime else return Nothing
        maybe (return ()) (atomicallyNodeT . rescanTs) fcM

        -- Start the merkle sync
        merkleSync sem pool 500
    -- Run a thread that will re-broadcast pending transactions
    broadcastPendingTxs nodeState sem pool = runNodeT nodeState $ forever $ do
        -- Wait until we are synced
        atomicallyNodeT $ do
            synced <- areBlocksSynced
            unless synced $ lift retry
        -- Send an INV for those transactions to all peers
        broadcastTxs =<< runDBPool sem pool (getPendingTxs 0)
        -- Wait until we are not synced
        atomicallyNodeT $ do
            synced <- areBlocksSynced
            when synced $ lift retry
    processTx sem pool = awaitForever $ \tx -> lift $ do
        (_, newAddrs) <- runDBPool sem pool $ importNetTx tx
        unless (null newAddrs) $ do
            $(logInfo) $ pack $ unwords
                [ "Generated", show $ length newAddrs
                , "new addresses while importing the tx."
                , "Updating the bloom filter"
                ]
            (bloom, elems, _) <- runDBPool sem pool getBloomFilter
            atomicallyNodeT $ sendBloomFilter bloom elems

initDatabase :: (MonadBaseControl IO m, MonadLoggerIO m)
             => Config -> m (Sem.MSem Int, ConnectionPool)
initDatabase cfg = do
    -- Create a semaphore with 1 resource
    sem <- liftIO $ Sem.new 1
    -- Create a database pool
    let dbCfg = fromMaybe
            (error $ "DB config settings for " ++ networkName ++ " not found")
            (pack networkName `H.lookup` configDatabase cfg)
    pool <- getDatabasePool dbCfg
    -- Initialize wallet database
    runDBPool sem pool $ do
        _ <- runMigration migrateWallet
        initWallet $ configBloomFP cfg
    -- Return the semaphrone and the connection pool
    return (sem, pool)

merkleSync
    :: ( MonadLoggerIO m
       , MonadBaseControl IO m
       , MonadThrow m
       , MonadResource m
       )
    => Sem.MSem Int
    -> ConnectionPool
    -> Int
    -> NodeT m ()
merkleSync sem pool bSize = do
    -- Get our best block
    (best, height) <- runDBPool sem pool getBestBlock
    $(logDebug) "Starting merkle batch download"
    -- Wait for a new block or a rescan
    (action, source) <- merkleDownload best height bSize
    $(logDebug) "Received a merkle action and source. Processing the source..."

    -- Read and process the data from the source
    (lastMerkleM, mTxsAcc, aMap) <- source $$ go Nothing [] M.empty
    $(logDebug) "Merkle source processed and closed"

    -- Send a new bloom filter to our peers if new addresses were generated
    unless (M.null aMap) $ do
        $(logInfo) $ pack $ unwords
            [ "Generated", show $ sum $ M.elems aMap
            , "new addresses while importing the merkle block."
            , "Sending our bloom filter."
            ]
        (bloom, elems, _) <- runDBPool sem pool getBloomFilter
        atomicallyNodeT $ sendBloomFilter bloom elems

    -- Check if we should rescan the current merkle batch
    $(logDebug) "Checking if we need to rescan the current batch..."
    rescan <- lift $ shouldRescan aMap
    when rescan $ $(logDebug) "We need to rescan the current batch"
    -- Compute the new batch size
    let newBSize | rescan    = max 1 $ bSize `div` 2
                 | otherwise = min 500 $ bSize + max 1 (bSize `div` 20)

    when (newBSize /= bSize) $ $(logDebug) $ pack $ unwords
        [ "Changing block batch size from", show bSize, "to", show newBSize ]

    -- Did we receive all the merkles that we asked for ?
    let missing = (headerHash <$> lastMerkleM) /=
                  Just (nodeBlockHash $ last $ actionNodes action)

    when missing $ $(logWarn) $ pack $ unwords
        [ "Merkle block stream closed prematurely"
        , show lastMerkleM
        ]

    -- TODO: We could still salvage a partially received batch
    unless (rescan || missing) $ do
        $(logDebug) "Importing merkles into the wallet..."
        -- Confirm the transactions
        runDBPool sem pool $ importMerkles action mTxsAcc
        $(logDebug) "Done importing merkles into the wallet"
        logBlockChainAction action

    merkleSync sem pool newBSize
  where
    go lastMerkleM mTxsAcc aMap = await >>= \resM -> case resM of
        Just (Right tx) -> do
            $(logDebug) $ pack $ unwords
                [ "Importing merkle tx", cs $ txHashToHex $ txHash tx ]
            (_, newAddrs) <- lift $ runDBPool sem pool $ importNetTx tx
            $(logDebug) $ pack $ unwords
                [ "Generated", show $ length newAddrs
                , "new addresses while importing tx"
                , cs $ txHashToHex $ txHash tx
                ]
            let newMap = M.unionWith (+) aMap $ groupByAcc newAddrs
            go lastMerkleM mTxsAcc newMap
        Just (Left (MerkleBlock mHead _ _ _, mTxs)) -> do
            $(logDebug) $ pack $ unwords
                [ "Buffering merkle block"
                , cs $ blockHashToHex $ headerHash mHead
                ]
            go (Just mHead) (mTxs:mTxsAcc) aMap
        -- Done processing this batch. Reverse mTxsAcc as we have been
        -- prepending new values to it.
        _ -> return (lastMerkleM, reverse mTxsAcc, aMap)
    groupByAcc addrs =
        let xs = map (\a -> (walletAddrAccount a, 1)) addrs
        in  M.fromListWith (+) xs
    shouldRescan aMap = do
        -- Try to find an account whos gap is smaller than the number of new
        -- addresses generated in that account.
        res <- runDBPool sem pool $ splitSelect (M.assocs aMap) $ \ks ->
            from $ \a -> do
                let andCond (ai, cnt) =
                        a ^. AccountId ==. val ai &&.
                        a ^. AccountGap <=. val cnt
                where_ $ join2 $ map andCond ks
                return $ a ^. AccountId
        return $ not $ null res
    -- Some logging of the blocks
    logBlockChainAction action = case action of
        BestChain nodes -> $(logInfo) $ pack $ unwords
            [ "Best chain height"
            , show $ nodeHeaderHeight $ last nodes
            , "(", cs $ blockHashToHex $ nodeBlockHash $ last nodes, ")"
            ]
        ChainReorg _ o n -> $(logInfo) $ pack $ unlines $
            [ "Chain reorg."
            , "Orphaned blocks:"
            ]
            ++ map (("  " ++) . cs . blockHashToHex . nodeBlockHash) o
            ++ [ "New blocks:" ]
            ++ map (("  " ++) . cs . blockHashToHex . nodeBlockHash) n
            ++ [ unwords [ "Best merkle chain height"
                        , show $ nodeHeaderHeight $ last n
                        ]
            ]
        SideChain n -> $(logWarn) $ pack $ unlines $
            "Side chain:" :
            map (("  " ++) . cs . blockHashToHex . nodeBlockHash) n
        KnownChain n -> $(logWarn) $ pack $ unlines $
            "Known chain:" :
            map (("  " ++) . cs . blockHashToHex . nodeBlockHash) n

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
runWalletApp :: ( MonadLoggerIO m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                )
             => HandlerSession -> m ()
runWalletApp session = liftBaseOpDiscard withContext $ \ctx ->
    liftBaseOpDiscard (withSocket ctx Rep) $ \sock -> do
        when (isJust serverKeyM) $ liftIO $ do
            let k = fromJust $ configServerKey $ handlerConfig session
            setCurveServer True sock
            setCurveSecretKey TextFormat k sock
        when (isJust clientKeyPubM) $ do
            k <- z85Decode (fromJust clientKeyPubM)
            void $ async $ runZapAuth ctx k
        liftIO $ bind sock $ configBind $ handlerConfig session
        forever $ do
            bs  <- liftIO $ receive sock
            res <- case decode $ BL.fromStrict bs of
                Just r  -> catchErrors $
                    runHandler session $ dispatchRequest r
                Nothing -> return $ ResponseError "Could not decode request"
            liftIO $ send sock [] $ BL.toStrict $ encode res
  where
    cfg = handlerConfig session
    serverKeyM = configServerKey cfg
    clientKeyPubM = configClientKeyPub cfg
    catchErrors m = catches m
        [ E.Handler $ \(WalletException err) -> do
            $(logError) $ pack err
            return $ ResponseError $ pack err
        , E.Handler $ \(ErrorCall err) -> do
            $(logError) $ pack err
            return $ ResponseError $ pack err
        , E.Handler $ \(SomeException exc) -> do
            $(logError) $ pack $ show exc
            return $ ResponseError $ pack $ show exc
        ]

runZapAuth :: ( MonadLoggerIO m
              , MonadBaseControl IO m
              , MonadBase IO m
              )
           => Context -> ByteString -> m ()
runZapAuth ctx k = do
    $(logDebug) $ "Starting ØMQ authentication thread"
    liftBaseOpDiscard (withSocket ctx Rep) $ \zap -> do
        liftIO $ bind zap "inproc://zeromq.zap.01"
        forever $ do
            buffer <- liftIO $ receiveMulti zap
            let actionE =
                    case buffer of
                      v:q:_:_:_:m:p:_ -> do
                          when (v /= "1.0") $
                              Left (q, "500", "Version number not valid")
                          when (m /= "CURVE") $
                              Left (q, "400", "Mechanism not supported")
                          when (p /= k) $
                              Left (q, "400", "Invalid client public key")
                          return q
                      _ -> Left ("", "500", "Malformed request")
            case actionE of
              Right q -> do
                  $(logInfo) "Authenticated client successfully"
                  liftIO $ sendMulti zap $
                      "1.0" :| [q, "200", "OK", "client", ""]
              Left (q, c, m) -> do
                  $(logError) $ pack $ unwords
                      [ "Failed to authenticate client:" , cs c, cs m ]
                  liftIO $ sendMulti zap $
                      "1.0" :| [q, c, m, "", ""]


dispatchRequest :: ( MonadLoggerIO m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadThrow m
                   , MonadResource m
                   )
                => WalletRequest -> Handler m (WalletResponse Value)
dispatchRequest req = fmap ResponseValid $ case req of
    GetAccountsR                     -> getAccountsR
    PostAccountsR na                 -> postAccountsR na
    PostAccountRenameR n n'          -> postAccountRenameR n n'
    GetAccountR n                    -> getAccountR n
    PostAccountKeysR n ks            -> postAccountKeysR n ks
    PostAccountGapR n g              -> postAccountGapR n g
    GetAddressesR n t m o p          -> getAddressesR n t m o p
    GetAddressesUnusedR n t          -> getAddressesUnusedR n t
    GetAddressR n i t m o            -> getAddressR n i t m o
    PutAddressR n i t l              -> putAddressR n i t l
    PostAddressesR n i t             -> postAddressesR n i t
    GetTxsR n p                      -> getTxsR n p
    GetAddrTxsR n i t p              -> getAddrTxsR n i t p
    PostTxsR n k a                   -> postTxsR n k a
    GetTxR n h                       -> getTxR n h
    GetOfflineTxR n h                -> getOfflineTxR n h
    PostOfflineTxR n k t c           -> postOfflineTxR n k t c
    GetBalanceR n mc o               -> getBalanceR n mc o
    PostNodeR na                     -> postNodeR na
    DeleteTxIdR t                    -> deleteTxIdR t

