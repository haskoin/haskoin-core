{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    )

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.DeepSeq (NFData, rnf)
import Control.Exception

import Data.Maybe
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

import Network.Haskoin.Util
import Network.Haskoin.Stratum
import Network.Haskoin.Script

import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Coin
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Server.Types

-- TODO: Handle parse errors and exceptions
runServer :: IO ()
runServer = do
    dir <- getWorkDir
    let walletFile = T.pack $ concat [dir, "/wallet"]
        headerFile = concat [dir, "/headerchain"]

    -- Create sqlite connection pool & initialization
    pool <- createSqlitePool walletFile 10 -- TODO: Put 10 in a config file?
    bloom <- runDB pool $ do
        runMigrationSilent migrateWallet 
        initWalletDB
        walletBloomFilter

    -- Launch SPV node
    (eChan, rChan) <- startNode headerFile 
    forkIO $ sourceTBMChan eChan $$ processNodeEvents pool
    atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom

    -- Launch JSON-RPC server
    -- TODO: Put connection stuff in config file
    runTCPServer (serverSettings 4000 "127.0.0.1") $ \client ->
        appSource client
            -- TODO: The server ignores bad requests here. Change that?
            $= CL.mapMaybe (eitherToMaybe . decodeWalletRequest)
            $$ CL.mapM (processWalletRequest pool rChan)
            =$ CL.map (\(res,i) -> encodeWalletResponse res i)
            =$ appSink client

processWalletRequest :: ConnectionPool 
                     -> TBMChan NodeRequest
                     -> (WalletRequest, Int) 
                     -> IO (Either String WalletResponse, Int)
processWalletRequest pool rChan (wr, i) = do
    res <- tryJust f $ runDB pool $ go wr
    return (res, i)
  where
    -- TODO: What if we have other exceptions than WalletException ?
    f (WalletException err)  = Just err
    go (NewFullWallet n p m) = liftM ResMnemonic $ newWalletMnemo n p m
    go (NewReadWallet n k)   = error "Not implemented"
    go (GetWallet n)         = liftM ResWallet $ getWallet n
    go WalletList            = liftM ResWalletList $ walletList
    go (NewAccount w n)      = do
        a <- newAccount w n
        setLookAhead n 30
        bloom <- walletBloomFilter
        liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
        return $ ResAccount a
    go (NewMSAccount w n r t ks) = do
        a <- newMSAccount w n r t ks
        when (length (accountKeys a) == t - 1) $ do
            setLookAhead n 30
            bloom <- walletBloomFilter
            liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
        return $ ResAccount a
    go (AddAccountKeys n ks) = do
        a <- addAccountKeys n ks
        when (length (accountKeys a) == accountTotal a - 1) $ do
            setLookAhead n 30
            bloom <- walletBloomFilter
            liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
        return $ ResAccount a
    go (GetAccount n)         = liftM ResAccount $ getAccount n
    go AccountList            = liftM ResAccountList $ accountList
    go (GenAddress n i')      = do
        addrs <- newAddrs n i'
        bloom <- walletBloomFilter
        liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
        return $ ResAddressList addrs
    go (AddressLabel n i' l)  = liftM ResAddress $ setAddrLabel n i' l
    go (AddressList n)        = liftM ResAddressList $ addressList n
    go (AddressPage n p a)    = do
        (as, m) <- addressPage n p a
        return $ ResAddressPage as m
    go (TxList n)      = liftM ResAccTxList $ txList n
    go (TxPage n p t)  = do
        (l,i) <- txPage n p t
        return $ ResAccTxPage l i
    go (TxSend n xs f) = do
        (tid, complete) <- sendTx n xs f
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

processNodeEvents :: ConnectionPool -> Sink NodeEvent IO ()
processNodeEvents pool = awaitForever $ \e -> lift $ runDB pool $ case e of
    MerkleBlockEvent xs -> void $ importBlocks xs
    TxEvent tx          -> void $ importTx tx False

runDB :: ConnectionPool -> SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB pool m = runResourceT $ runNoLoggingT $ runSqlPool m pool

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return dir

