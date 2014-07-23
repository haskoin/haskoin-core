{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )

import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception

import Data.Maybe
import Data.String (fromString)
import Data.Conduit 
    ( Sink
    , awaitForever
    , ($$), ($=), (=$)
    )
import Data.Conduit.Network
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Data.Conduit.List as CL

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Sqlite (open)

import Network.Haskoin.Util
import Network.Haskoin.Script
import Network.Haskoin.Constants

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
    bloom <- runDB mvar $ do
        _ <- runMigrationSilent migrateWallet 
        initWalletDB
        walletBloomFilter 

    -- Launch SPV node
    withAsyncNode dir $ \eChan rChan _ -> do
        let eventPipe = sourceTBMChan eChan $$ processNodeEvents mvar rChan 
        withAsync eventPipe $ \_ -> do
            atomically $ do
                -- Bloom filter
                writeTBMChan rChan $ BloomFilterUpdate bloom
                -- Bitcoin hosts to connect to
                forM_ hosts $ \(h,p) -> writeTBMChan rChan $ ConnectNode h p

            -- Launch JSON-RPC server
            runTCPServer (serverSettings port bind) $ \client ->
                appSource client
                    -- TODO: The server ignores bad requests here. Change that?
                    $= CL.mapMaybe (eitherToMaybe . decodeWalletRequest)
                    $$ CL.mapM (processWalletRequest mvar rChan)
                    =$ CL.map (\(res,i) -> encodeWalletResponse res i)
                    =$ appSink client

processNodeEvents :: MVar Connection -> TBMChan NodeRequest
                  -> Sink NodeEvent IO ()
processNodeEvents mvar rChan = awaitForever $ \e -> do
    res <- lift $ tryJust f $ runDB mvar $ case e of
        MerkleBlockEvent xs -> void $ importBlocks xs
        TxEvent ts          -> do
            before <- count ([] :: [Filter (DbAddressGeneric b)])
            forM_ ts $ \tx -> importTx tx False
            after <- count ([] :: [Filter (DbAddressGeneric b)])
            -- Update the bloom filter if new addresses were generated
            when (after > before) $ do
                bloom <- walletBloomFilter
                liftIO $ atomically $ do
                    writeTBMChan rChan $ BloomFilterUpdate bloom
                
    when (isLeft res) $ liftIO $ print $ fromLeft res
  where
    -- TODO: What if we have other exceptions than WalletException ?
    f (SomeException e) = Just $ show e

processWalletRequest :: MVar Connection 
                     -> TBMChan NodeRequest
                     -> (WalletRequest, Int) 
                     -> IO (Either String WalletResponse, Int)
processWalletRequest mvar rChan (wr, i) = do
    res <- tryJust f $ runDB mvar $ go wr
    return (res, i)
  where
    f (SomeException e)  = Just $ show e
    go (NewFullWallet n p m) = liftM ResMnemonic $ newWalletMnemo n p m
    go (NewReadWallet _ _)   = error "Not implemented"
    go (GetWallet n)         = liftM ResWallet $ getWallet n
    go WalletList            = liftM ResWalletList $ walletList
    go (NewAccount w n)      = do
        fstKeyBefore <- firstKeyTime
        a <- newAccount w n
        setLookAhead n 30
        bloom      <- walletBloomFilter
        fstKeyTime <- liftM fromJust firstKeyTime
        liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
            when (isNothing fstKeyBefore) $
                writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (NewMSAccount w n r t ks) = do
        fstKeyBefore <- firstKeyTime
        a <- newMSAccount w n r t ks
        when (length (accountKeys a) == t - 1) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter
            fstKeyTime <- liftM fromJust firstKeyTime
            liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (AddAccountKeys n ks) = do
        fstKeyBefore <- firstKeyTime
        a <- addAccountKeys n ks
        when (length (accountKeys a) == accountTotal a - 1) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter
            fstKeyTime <- liftM fromJust firstKeyTime
            liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (GetAccount n)         = liftM ResAccount $ getAccount n
    go AccountList            = liftM ResAccountList $ accountList
    go (GenAddress n i')      = do
        fstKeyBefore <- firstKeyTime
        addrs      <- newAddrs n i'
        bloom      <- walletBloomFilter
        fstKeyTime <- liftM fromJust firstKeyTime
        liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
            when (isNothing fstKeyBefore) $
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
    go (Rescan (Just t)) = do
        liftIO $ atomically $ writeTBMChan rChan $ FastCatchupTime t
        return $ ResRescan t
    go (Rescan Nothing) = do
        fstKeyTimeM <- firstKeyTime
        if (isJust fstKeyTimeM)
            then do
                liftIO $ atomically $ writeTBMChan rChan $ 
                    FastCatchupTime $ fromJust fstKeyTimeM
                return $ ResRescan $ fromJust fstKeyTimeM
            else liftIO $ throwIO $ WalletException
                "No keys have been generated in the wallet"

runDB :: MVar Connection -> SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB mvar m = withMVar mvar $ \conn -> 
    runResourceT $ runNoLoggingT $ runSqlConn m conn

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    haskoinDir <- getAppUserDataDirectory "haskoin"
    let dir = concat [ haskoinDir, "/", networkName ]
    createDirectoryIfMissing True dir
    return dir

