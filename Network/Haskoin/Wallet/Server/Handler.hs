module Network.Haskoin.Wallet.Server.Handler where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Exception (throwIO)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import qualified Control.Monad.State as S (StateT, evalStateT, gets)

import Data.Aeson (Value(..), toJSON)
import Data.Word (Word32)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import qualified Data.Text as T (unpack)

import Database.Persist.Sql (SqlPersistT, ConnectionPool, runSqlPool)

import Network.Haskoin.Crypto
import Network.Haskoin.Node
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Root

type Handler = S.StateT HandlerSession IO

data HandlerSession = HandlerSession
    { handlerConfig :: SPVConfig
    , handlerPool   :: ConnectionPool
    , handlerChan   :: Maybe (TBMChan SPVRequest)
    }

runHandler :: HandlerSession -> Handler a -> IO a
runHandler session m = S.evalStateT m session

runDB :: SqlPersistT IO a -> Handler a
runDB action = do
    pool <- S.gets handlerPool
    liftIO $ runSqlPool action pool

sendSPV :: SPVRequest -> Handler ()
sendSPV request = do
    chanM <- S.gets handlerChan 
    case chanM of
        Just chan -> liftIO $ atomically $ writeTBMChan chan request
        Nothing   -> return ()

{- Server Handlers -}

getWalletsR :: Handler Value
getWalletsR = toJSON <$> runDB walletList

getWalletR :: WalletName -> Handler Value
getWalletR w = toJSON <$> runDB (getWallet w)

postWalletsR :: NewWallet -> Handler Value
postWalletsR (NewWallet name passM msM) = do
    (ms, seed) <- case msM of
        Just ms -> case mnemonicToSeed pass (T.unpack ms) of
            Left err -> liftIO $ throwIO $ WalletException err
            Right seed -> return (T.unpack ms, seed)
        Nothing -> do
            ent <- liftIO $ devURandom 16
            let msSeedE = do
                ms <- toMnemonic ent
                seed <- mnemonicToSeed pass ms
                return (ms, seed)
            case msSeedE of
                Left err -> liftIO $ throwIO $ WalletException err
                Right msSeed -> return msSeed
    _ <- runDB $ newWallet name seed
    return $ toJSON $ MnemonicRes ms
  where
    pass = T.unpack $ fromMaybe "" $ passM

getAccountsR :: WalletName -> Handler Value
getAccountsR wallet = toJSON <$> runDB (accountList wallet)

postAccountsR :: WalletName -> NewAccount -> Handler Value
postAccountsR wallet newAcc = do
    gap <- spvGap <$> S.gets handlerConfig
    toJSON <$> case newAcc of
        NewAccountRegular name -> do
            acc <- runDB $ newAccount wallet name
            runDB $ addLookAhead wallet name gap
            whenOnline updateNodeFilter
            return acc
        NewAccountMultisig name m n keys -> do
            acc <- runDB $ newAccountMultisig wallet name m n keys
            when (length (accountKeys acc) == n) $ do
                runDB $ addLookAhead wallet name gap
                whenOnline updateNodeFilter
            return acc
        NewAccountRead name key -> do
            acc <- runDB $ newAccountRead wallet name key
            runDB $ addLookAhead wallet name gap
            whenOnline updateNodeFilter
            return acc
        NewAccountReadMultisig name m n keys -> do
            acc <- runDB $ newAccountReadMultisig wallet name m n keys
            when (length (accountKeys acc) == n) $ do
                runDB $ addLookAhead wallet name gap
                whenOnline updateNodeFilter
            return acc

getAccountR :: WalletName -> AccountName -> Handler Value
getAccountR wallet name = toJSON <$> runDB (getAccount wallet name)

postAccountKeysR :: WalletName -> AccountName -> [XPubKey] -> Handler Value
postAccountKeysR wallet name keys = do
    acc <- runDB $ addAccountKeys wallet name keys
    when (length (accountKeys acc) == accountTotal acc) $ do
        gap <- spvGap <$> S.gets handlerConfig
        runDB $ addLookAhead wallet name gap
        whenOnline updateNodeFilter
    return $ toJSON acc

getAddressesR :: WalletName -> AccountName -> (Maybe PagedResult) 
              -> Word32 -> Bool -> Bool -> Bool -> Handler Value
getAddressesR wallet name pageM minconf internal unlabeled unused 
    | internal && unlabeled = goUnused -- There are no labels on internal addrs
    | unlabeled = goUnlabeled
    | unused = goUnused
    | otherwise = runDB $ case pageM of
        Just (PagedResult page elemPerPage) -> do
            (xs, m) <- addressPage wallet name page elemPerPage internal
            ba      <- mapM (flip addressBalance minconf) xs
            return $ toJSON $ AddressPageRes ba m
        Nothing -> do
            xs <- addressList wallet name internal
            toJSON <$> mapM (flip addressBalance minconf) xs
  where
    goUnlabeled = runDB $ do
        pa <- unlabeledAddrs wallet name
        toJSON <$> mapM (flip addressBalance minconf) pa
    goUnused = runDB $ do
        pa <- unusedAddrs wallet name internal
        toJSON <$> mapM (flip addressBalance minconf) pa

postAddressesR :: WalletName -> AccountName -> AddressData 
               -> Handler Value
postAddressesR wallet name (AddressData label) = runDB $ do
    unlabeled <- unlabeledAddrs wallet name
    toJSON <$> case unlabeled of
        [] -> liftIO $ throwIO $ WalletException "No more available addresses"
        (newAddr:_) -> setAddrLabel wallet name (laIndex newAddr) label

getAddressR :: WalletName -> AccountName -> KeyIndex -> Word32 -> Bool 
            -> Handler Value
getAddressR wallet name i minconf internal = runDB $ do
    pa <- getAddress wallet name i internal
    toJSON <$> addressBalance pa minconf

putAddressR :: WalletName -> AccountName -> KeyIndex -> AddressData
            -> Handler Value
putAddressR wallet name i (AddressData label) = 
    toJSON <$> runDB (setAddrLabel wallet name i label)

getTxsR :: WalletName -> AccountName -> (Maybe PagedResult) -> Handler Value
getTxsR wallet name pageM = runDB $ case pageM of
    Just (PagedResult page elemPerPage) -> do
        (xs, m) <- txPage wallet name page elemPerPage
        return $ toJSON $ TxPageRes xs m
    Nothing -> toJSON <$> txList wallet name

postTxsR :: WalletName -> AccountName -> AccTxAction -> Handler Value
postTxsR wallet name action = case action of
    CreateTx rs fee minconf sign -> do
        (tid, complete, genA) <- runDB $ 
            createTx wallet name minconf rs fee sign
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . PublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete
    SignTx tx finalize -> do
        (tid, complete, genA) <- runDB $ signWalletTx wallet name tx finalize
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . PublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete
    SignOfflineTxData otd finalize -> do
        (tx, complete) <- runDB $ signOfflineTxData wallet name finalize otd
        return $ toJSON $ TxStatusRes tx complete
    ImportTx tx -> do
        resM <- runDB $ importTx tx SourceUnknown $ Just (wallet, name)
        let (tid, conf, genA) = fromJust resM
            complete = conf `elem` [ TxPending, TxBuilding ]
        when (isNothing resM) $ liftIO $ throwIO $
            WalletException "Transaction could not be imported"
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            sendSPV . PublishTx =<< runDB (getTx tid)
        return $ toJSON $ TxHashStatusRes tid complete

getTxR :: WalletName -> AccountName -> TxHash -> Bool -> Handler Value
getTxR wallet name hash prop = runDB $ do
    aTx <- getAccTx wallet name hash
    if prop
        then do
            p <- getProposition wallet name hash
            return $ toJSON aTx{ accTxTx = p }
        else return $ toJSON aTx

getOfflineTxDataR :: WalletName -> AccountName -> TxHash -> Handler Value
getOfflineTxDataR wallet name hash = 
    toJSON <$> runDB (getOfflineTxData wallet name hash)

getBalanceR :: WalletName -> AccountName -> Word32 -> Handler Value
getBalanceR wallet name minconf = do
    (balance, cs) <- runDB $ accountBalance wallet name minconf
    return $ toJSON $ BalanceRes balance cs

getSpendableR :: WalletName -> AccountName -> Word32 -> Handler Value
getSpendableR wallet name minconf = do
    balance <- runDB $ spendableAccountBalance wallet name minconf
    return $ toJSON $ SpendableRes balance

postNodeR :: NodeAction -> Handler Value
postNodeR action = toJSON <$> case action of
    Rescan (Just t) -> do
        whenOnline $ do
            runDB resetRescan
            sendSPV $ NodeRescan t
        return $ RescanRes t
    Rescan Nothing -> do
        fstKeyTimeM <- runDB firstKeyTime
        when (isNothing fstKeyTimeM) $ liftIO $ throwIO $
            WalletException "No keys have been generated in the wallet"
        let fstKeyTime = fromJust fstKeyTimeM       
        whenOnline $ do
            runDB resetRescan
            sendSPV $ NodeRescan fstKeyTime
        return $ RescanRes fstKeyTime

{- Helpers -}

whenOnline :: Handler () -> Handler ()
whenOnline handler = do
    mode <- spvMode <$> S.gets handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter :: Handler ()
updateNodeFilter = do
    bloomFP <- spvBloomFP <$> S.gets handlerConfig
    sendSPV . BloomFilterUpdate =<< runDB (walletBloomFilter bloomFP)

