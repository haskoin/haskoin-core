{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.DeepSeq (NFData, rnf)
import Control.Exception

import Data.Maybe
import Data.String (fromString)
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit 
    ( Sink
    , awaitForever
    , yield
    , addCleanup
    , ($$), ($=), (=$)
    )
import Data.Conduit.Network
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Sqlite (open)

import Network.Haskoin.Util
import Network.Haskoin.Script

import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Types

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Coin
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Server.Types
import Network.Haskoin.Server.Config

-- TODO: Handle parse errors and exceptions
runServer :: IO ()
runServer = do
    dir <- getWorkDir
    let walletFile = T.pack $ concat [dir, "/wallet"]
        configFile = concat [dir, "/config"]

    configExists <- doesFileExist configFile

    unless configExists $ encodeFile configFile defaultServerConfig

    configM <- decodeFile configFile
    unless (isJust configM) $ throwIO $ NodeException $ unwords
        [ "Could node parse config file"
        , configFile
        ]

    let bind  = fromString $ configBind $ fromJust configM
        port  = configPort $ fromJust configM
        hosts = configHosts $ fromJust configM

    -- Create sqlite connection & initialization
    mvar <- newMVar =<< wrapConnection =<< open walletFile
    (bloom, fstKeyTime) <- runDB mvar $ do
        runMigrationSilent migrateWallet 
        initWalletDB
        liftM2 (,) walletBloomFilter firstKeyTime

    -- Launch SPV node
    withAsyncNode dir $ \eChan rChan _ -> do
        let eventPipe = sourceTBMChan eChan $$ processNodeEvents mvar rChan 
        withAsync eventPipe $ \_ -> do
            atomically $ do
                -- Bloom filter
                writeTBMChan rChan $ BloomFilterUpdate bloom
                -- Fast catchup time
                when (isJust fstKeyTime) $ writeTBMChan rChan $ 
                    FastCatchupTime $ fromJust fstKeyTime
                -- Bitcoin hosts to connect to
                forM_ hosts $ \(h,p) -> writeTBMChan rChan $ ConnectNode h p

            -- Launch JSON-RPC server
            -- TODO: Put connection stuff in config file
            runTCPServer (serverSettings port bind) $ \client ->
                appSource client
                    -- TODO: The server ignores bad requests here. Change that?
                    $= CL.mapMaybe (eitherToMaybe . decodeWalletRequest)
                    $$ CL.mapM (processWalletRequest mvar rChan)
                    =$ CL.map (\(res,i) -> encodeWalletResponse res i)
                    =$ appSink client

processWalletRequest :: MVar Connection 
                     -> TBMChan NodeRequest
                     -> (WalletRequest, Int) 
                     -> IO (Either String WalletResponse, Int)
processWalletRequest mvar rChan (wr, i) = do
    res <- tryJust f $ runDB mvar $ go wr
    return (res, i)
  where
    -- TODO: What if we have other exceptions than WalletException ?
    f (WalletException err)  = Just err
    go (NewFullWallet n p m) = liftM ResMnemonic $ newWalletMnemo n p m
    go (NewReadWallet _ _)   = error "Not implemented"
    go (GetWallet n)         = liftM ResWallet $ getWallet n
    go WalletList            = liftM ResWalletList $ walletList
    go (NewAccount w n)      = do
        a <- newAccount w n
        setLookAhead n 30
        bloom      <- walletBloomFilter
        fstKeyTime <- liftM fromJust firstKeyTime
        liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
            writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (NewMSAccount w n r t ks) = do
        a <- newMSAccount w n r t ks
        when (length (accountKeys a) == t - 1) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter
            fstKeyTime <- liftM fromJust firstKeyTime
            liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (AddAccountKeys n ks) = do
        a <- addAccountKeys n ks
        when (length (accountKeys a) == accountTotal a - 1) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter
            fstKeyTime <- liftM fromJust firstKeyTime
            liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (GetAccount n)         = liftM ResAccount $ getAccount n
    go AccountList            = liftM ResAccountList $ accountList
    go (GenAddress n i')      = do
        addrs      <- newAddrs n i'
        bloom      <- walletBloomFilter
        fstKeyTime <- liftM fromJust firstKeyTime
        liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
            writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAddressList addrs
    go (AddressLabel n i' l)  = liftM ResAddress $ setAddrLabel n i' l
    go (AddressList n)        = liftM ResAddressList $ addressList n
    go (AddressPage n p a)    = do
        (as, m) <- addressPage n p a
        return $ ResAddressPage as m
    go (TxList n)      = liftM ResAccTxList $ txList n
    go (TxPage n p t)  = do
        (l,m) <- txPage n p t
        return $ ResAccTxPage l m
    go (TxSend n xs s) = do
        (tid, complete) <- sendTx n xs s
        when complete $ do
            newTx <- getTx tid
            liftIO $ atomically $ writeTBMChan rChan $ PublishTx newTx
        return $ ResTxStatus tid complete
    go (TxSign n tx)   = do
        (tid, complete) <- signWalletTx n tx (SigAll False)
        when complete $ do
            newTx <- getTx tid
            liftIO $ atomically $ writeTBMChan rChan $ PublishTx newTx
        return $ ResTxStatus tid complete
    go (Balance n)     = liftM ResBalance $ balance n

processNodeEvents :: MVar Connection -> TBMChan NodeRequest
                  -> Sink NodeEvent IO ()
processNodeEvents mvar rChan = awaitForever $ \e -> lift $ runDB mvar $ 
    case e of
        MerkleBlockEvent a txs -> void $ importBlock a txs
        TxEvent tx             -> void $ importTx tx False

runDB :: MVar Connection -> SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB mvar m = withMVar mvar $ \conn -> 
    runResourceT $ runNoLoggingT $ runSqlConn m conn

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return dir

