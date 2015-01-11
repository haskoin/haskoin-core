module Network.Haskoin.Wallet.Server where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_, forM, liftM, mzero, forever)
import Control.Exception 
    (SomeException(..), ErrorCall(..), tryJust, throw, catch)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, runNoLoggingT)
import qualified Control.Monad.State as S (StateT, evalStateT, get, gets)

import Data.Monoid (mempty)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes)
import Data.Default (def)
import Data.Word (Word16)
import Data.Aeson
import Data.List (stripPrefix)
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T(pack)
import qualified Data.ByteString as BS (ByteString, append, empty)

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
import System.ZMQ4.Monadic
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)

import Network.Haskoin.Constants
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server.Handler
import Network.Haskoin.Wallet.Database

runSPVServer :: SPVConfig -> IO ()
runSPVServer config = do

    -- Get database pool
    pool <- getDatabasePool $ spvDatabase config
    
    -- Initialize wallet database
    flip runSqlPersistMPool pool $ do 
        _ <- runMigration migrateWallet 
        initWalletDB

    if spvMode config == SPVOffline
        then runWalletApp $ HandlerSession config pool Nothing
        else do
            -- Find earliest key creation time
            fstKeyTimeM <- flip runSqlPersistMPool pool firstKeyTime
            fstKeyTime  <- case fstKeyTimeM of
                Just t  -> return t
                Nothing -> round <$> getPOSIXTime

            -- Create leveldb handle
            db <- DB.open "headerchain"
                DB.defaultOptions{ DB.createIfMissing = True
                                 , DB.cacheSize       = 2048
                                 }
            -- Launch SPV node
            let fp    = spvBloomFP config
                nodes = spvBitcoinNodes config
                session = NodeSession db pool fp

            withAsyncSPV nodes fstKeyTime session $ \rChan _ -> do
                -- Send the bloom filter
                bloom <- flip runSqlPersistMPool pool $ walletBloomFilter fp
                atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
                runWalletApp $ HandlerSession config pool $ Just rChan

runWalletApp :: HandlerSession -> IO ()
runWalletApp session = runZMQ $ do
    sock <- socket Rep
    bind sock $ spvBind $ handlerConfig session
    forever $ do
        bs  <- receive sock
        res <- case decode $ toLazyBS bs of
            Just r  -> liftIO $ catchErrors $ 
                runHandler session $ dispatchRequest r
            Nothing -> return $ ResponseError "Could not decode request"
        send sock [] $ toStrictBS $ encode res
  where
    -- TODO: Catch ErrorCall and SomeException
    catchErrors m = catch m $ 
        \(WalletException err) -> return $ ResponseError $ T.pack err

dispatchRequest :: WalletRequest -> Handler (WalletResponse Value)
dispatchRequest req = ResponseValid . toJSON <$> case req of
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

data NodeSession = NodeSession
    { chainHandle :: DB.DB
    , walletPool  :: ConnectionPool
    , bloomFP     :: Double
    }

instance SPVNode LevelDBChain NodeSession where
    runHeaderChain s = do
        db <- chainHandle <$> S.gets spvData
        resE <- liftIO $ tryJust f $ runLevelDBChain db s
        case resE of
            Left err -> liftIO (print err) >> undefined
            Right res -> return res
      where
        f (SomeException e) = Just $ show e

    spvImportTxs txs = do
        pool <- walletPool <$> S.gets spvData
        fp <- bloomFP <$> S.gets spvData
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ do
            xs <- forM txs $ \tx -> importTx tx SourceNetwork Nothing
            -- Update the bloom filter if new addresses were generated
            if or $ map lst3 $ catMaybes xs
                then Just <$> walletBloomFilter fp
                else return Nothing
        case resE of
            Left err -> liftIO $ print err
            Right bloomM -> when (isJust bloomM) $ 
                processBloomFilter $ fromJust bloomM
      where
        f (SomeException e) = Just $ show e

    spvImportMerkleBlock mb expTxs = do
        pool <- walletPool <$> S.gets spvData
        resE <- liftIO $ tryJust f $ flip runSqlPersistMPool pool $ 
            importBlock mb expTxs
        when (isLeft resE) $ liftIO $ print $ fromLeft resE
      where
        f (SomeException e) = Just $ show e

