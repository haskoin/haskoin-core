{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
module Network.Haskoin.REST where

import Control.Applicative ((<$>), (<*>))
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Logger
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Exception.Lifted (catch)

import Data.Aeson 
    ( Value
    , Result(..)
    , ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , withText
    )
import Data.Maybe
import Data.Word (Word32)
import Data.String (fromString)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Default (Default, def)

import Database.Persist.Sqlite
import Database.Sqlite (open)

import Yesod
    ( Yesod
    , YesodPersist(..)
    , RenderMessage
    , FormMessage
    , makeSessionBackend
    , renderMessage
    , defaultFormMessage
    , getYesod
    , runDB
    , mkYesod
    , parseRoutes
    , toWaiApp
    , renderRoute
    , intField
    , parseJsonBody
    , iopt
    , runInputGet
    , permissionDenied
    , invalidArgs
    )
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Middleware.HttpAuth (basicAuth)

import Network.Haskoin.Constants
import Network.Haskoin.Node
import Network.Haskoin.Crypto

import Network.Haskoin.REST.Types
import Network.Haskoin.SPV

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

data ServerMode
    = ServerOnline
    | ServerOffline
    | ServerVault
    deriving (Eq, Show, Read)

instance ToJSON ServerMode where
    toJSON m = case m of
        ServerOnline  -> "online"
        ServerOffline -> "offline"
        ServerVault   -> "vault"

instance FromJSON ServerMode where
    parseJSON = withText "servermode" $ \t -> case t of
        "online"  -> return ServerOnline
        "offline" -> return ServerOffline
        "vault"   -> return ServerVault
        _         -> mzero

data ServerConfig = ServerConfig
    { configBind         :: String
    , configPort         :: Int
    , configBitcoinHosts :: [(String, Int)]
    , configBatch        :: Int
    , configBloomFP      :: Double
    , configMode         :: ServerMode
    , configGap          :: Int
    , configUser         :: Text
    , configPassword     :: Text
    } deriving (Eq, Read, Show)

haskoinPort :: Int
haskoinPort 
    | networkName == "prodnet" = 8555
    | otherwise                = 18555

instance Default ServerConfig where
    def = ServerConfig
        { configBind         = "127.0.0.1"
        , configPort         = haskoinPort
        , configBitcoinHosts = [("127.0.0.1", defaultPort)]
        , configBatch        = 100
        , configBloomFP      = 0.00001
        , configMode         = ServerOnline
        , configUser         = "haskoin"
        , configPassword     = "haskoin"
        , configGap          = 10
        }

data HaskoinServer = HaskoinServer 
    { serverPool   :: ConnectionPool
    , serverNode   :: Maybe (TBMChan SPVRequest)
    , serverConfig :: ServerConfig
    }

instance Yesod HaskoinServer where
    makeSessionBackend _ = return Nothing

instance YesodPersist HaskoinServer where
    type YesodPersistBackend HaskoinServer = SqlBackend
    
    runDB action = do
        HaskoinServer pool _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage HaskoinServer FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "HaskoinServer" [parseRoutes|
/api/wallets                             WalletsR     GET POST
/api/wallets/#Text                       WalletR      GET
/api/accounts                            AccountsR    GET POST
/api/accounts/#Text                      AccountR     GET
/api/accounts/#Text/keys                 AccountKeysR POST
/api/accounts/#Text/addrs                AddressesR   GET POST
/api/accounts/#Text/addrs/#Int           AddressR     GET PUT
/api/accounts/#Text/acctxs               AccTxsR      GET POST
/api/accounts/#Text/acctxs/#Text         AccTxR       GET
/api/accounts/#Text/acctxs/#Text/sigblob SigBlobR     GET
/api/accounts/#Text/balance              BalanceR     GET
/api/txs/#Text                           TxR          GET 
/api/node                                NodeR        POST
|]

runServer :: ServerConfig -> IO ()
runServer config = do
    let walletFile = pack "wallet"

    pool <- runNoLoggingT $
        createSqlPool (\lf -> open walletFile >>= flip wrapConnection lf) 1
    flip runSqlPersistMPool pool $ do 
        _ <- runMigrationSilent migrateWallet 
        initWalletDB

    let bind     = fromString $ configBind config
        port     = configPort config
        hosts    = configBitcoinHosts config
        batch    = configBatch config
        fp       = configBloomFP config
        mode     = configMode config 
        settings = setHost bind $ setPort port defaultSettings
        user     = encodeUtf8 $ configUser config
        password = encodeUtf8 $ configPassword config
        checkCreds u p = return $ u == user && p == password
        runApp = runSettings settings . basicAuth checkCreds "haskoin"

    if mode `elem` [ServerOffline, ServerVault]
        then do
            app <- toWaiApp $ HaskoinServer pool Nothing config
            runApp app
        else do

            -- Find earliest key creation time
            fstKeyTimeM <- flip runSqlPersistMPool pool firstKeyTime
            fstKeyTime  <- case fstKeyTimeM of
                Just t  -> return t
                Nothing -> round <$> getPOSIXTime

            -- Adjust time backwards by a week to handle clock drifts.
            let fastCatchupI = max 0 ((toInteger fstKeyTime) - 86400 * 7)
                fc           = fromInteger fastCatchupI :: Word32

            -- Get best known blockhash
            conf <- flip runSqlPersistMPool pool $
                        selectFirst [] [Asc DbConfigCreated]
            let bb = dbConfigBestBlockHash $ entityVal $ fromJust conf

            -- Launch SPV node
            withAsyncSPV hosts batch fc bb (runNodeHandle fp pool) $ 
                \rChan _ -> do
                -- Send the bloom filter
                bloom <- flip runSqlPersistMPool pool $ walletBloomFilter fp
                atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
                -- Launch the haskoin server
                app <- toWaiApp $ HaskoinServer pool (Just rChan) config
                runApp app

guardVault :: Handler ()
guardVault = do
    HaskoinServer _ _ config <- getYesod
    when (configMode config == ServerVault) $ 
        permissionDenied "This operation is not supported in vault mode"

whenOnline :: Handler () -> Handler ()
whenOnline action = do
    HaskoinServer _ _ config <- getYesod
    when (configMode config == ServerOnline) action

updateNode :: Handler a -> Handler a
updateNode action = do
    res <- action
    whenOnline $ do
        HaskoinServer _ (Just rChan) config <- getYesod
        bloom <- runDB $ walletBloomFilter $ configBloomFP config
        liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
    return res

handleErrors :: Handler a -> Handler a
handleErrors action = 
    catch action f
  where
    f (WalletException err) = invalidArgs [pack err]

getWalletsR :: Handler Value
getWalletsR = handleErrors $ do
    guardVault 
    toJSON <$> runDB walletList

postWalletsR :: Handler Value
postWalletsR = handleErrors $ do
    guardVault 
    parseJsonBody >>= \res -> case res of
        Success (NewWallet name pass msM) -> do
            (ms, seed) <- case msM of
                Just ms -> case mnemonicToSeed pass ms of
                    Left err -> liftIO $ throwIO $ WalletException err
                    Right seed -> return (ms, seed)
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
        -- TODO: What happens here ?
        Error _ -> undefined

getWalletR :: Text -> Handler Value
getWalletR wname = handleErrors $ do
    guardVault 
    toJSON <$> runDB (getWallet $ unpack wname)

getAccountsR :: Handler Value
getAccountsR = handleErrors $ toJSON <$> runDB accountList

postAccountsR :: Handler Value
postAccountsR = handleErrors $ do
    HaskoinServer _ _ config <- getYesod
    let c = configGap config
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (NewAccount w n) -> do
            acc <- runDB $ newAccount w n
            updateNode $ runDB $ addLookAhead n c
            return acc
        Success (NewMSAccount w n r t ks) -> do
            acc <- runDB $ newMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead n c
            return acc
        Success (NewReadAccount n k) -> do
            acc <- runDB $ newReadAccount n k
            updateNode $ runDB $ addLookAhead n c
            return acc
        Success (NewReadMSAccount n r t ks) -> do
            acc <- runDB $ newReadMSAccount n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead n c
            return acc
        -- TODO: What happens here ?
        Error _ -> undefined

getAccountR :: Text -> Handler Value
getAccountR name = handleErrors $ toJSON <$> runDB (getAccount $ unpack name)

postAccountKeysR :: Text -> Handler Value
postAccountKeysR name = handleErrors $ do
    HaskoinServer _ _ config <- getYesod
    let c = configGap config
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success ks -> do
            acc <- runDB $ addAccountKeys (unpack name) ks
            when (length (accountKeys acc) == accountTotal acc) $ do
                updateNode $ runDB $ addLookAhead (unpack name) c
            return acc
        -- TODO: What happens here ?
        Error _ -> undefined

getAddressesR :: Text -> Handler Value
getAddressesR name = handleErrors $ do
    (pageM, elemM) <- runInputGet $ (,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
    if isJust pageM
        then do
            let e | isJust elemM = fromJust elemM
                  | otherwise    = 10
            (as, m) <- runDB $ addressPage (unpack name) (fromJust pageM) e
            return $ toJSON $ AddressPageRes as m
        else toJSON <$> runDB (addressList $ unpack name)

postAddressesR :: Text -> Handler Value
postAddressesR name = handleErrors $ do
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (AddressData label) -> updateNode $ do
            addr' <- runDB $ unlabeledAddr $ unpack name
            runDB $ setAddrLabel (unpack name) (addressIndex addr') label
        -- TODO: What happens here ?
        Error _ -> undefined

getAddressR :: Text -> Int -> Handler Value
getAddressR name i = handleErrors $ do
    addr <- runDB $ getAddress (unpack name) (fromIntegral i)
    return $ toJSON addr

putAddressR :: Text -> Int -> Handler Value
putAddressR name i = handleErrors $ do 
    parseJsonBody >>= \res -> case res of
        Success (AddressData label) -> runDB $ do
            addr <- setAddrLabel (unpack name) (fromIntegral i) label
            return $ toJSON addr
        -- TODO: What happens here ?
        Error _ -> undefined

getAccTxsR :: Text -> Handler Value
getAccTxsR name = handleErrors $ do
    (pageM, elemM) <- runInputGet $ (,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
    if isJust pageM
        then do
            let e | isJust elemM = fromJust elemM
                  | otherwise    = 10
            (as, m) <- runDB $ txPage (unpack name) (fromJust pageM) e
            return $ toJSON $ TxPageRes as m
        else toJSON <$> runDB (txList $ unpack name)

postAccTxsR :: Text -> Handler Value
postAccTxsR name = handleErrors $ guardVault >>
    parseJsonBody >>= \res -> case res of
        Success (SendCoins rs fee) -> do
            (tid, complete) <- runDB $ sendTx (unpack name) rs fee
            whenOnline $ when complete $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            return $ toJSON $ TxHashStatusRes tid complete
        Success (SignTx tx) -> do
            (tid, complete) <- runDB $ signWalletTx (unpack name) tx
            whenOnline $ when complete $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            return $ toJSON $ TxHashStatusRes tid complete
        Success (SignSigBlob blob) -> do
            (tx, c) <- runDB $ signSigBlob (unpack name) blob
            return $ toJSON $ TxStatusRes tx c
        -- TODO: What happens here ?
        Error _ -> undefined

getAccTxR :: Text -> Text -> Handler Value
getAccTxR name tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
        tid  = fromJust tidM
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    accTx <- runDB $ getAccTx (unpack name) tid
    return $ toJSON accTx

getTxR :: Text -> Handler Value
getTxR tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
        tid  = fromJust tidM
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    tx <- runDB $ getTx tid
    return $ toJSON $ TxRes tx

getBalanceR :: Text -> Handler Value
getBalanceR name = handleErrors $ 
    toJSON . BalanceRes <$> runDB (balance $ unpack name)

getSigBlobR :: Text -> Text -> Handler Value
getSigBlobR name tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    blob <- runDB $ getSigBlob (unpack name) $ fromJust tidM
    return $ toJSON blob

postNodeR :: Handler Value
postNodeR = handleErrors $ guardVault >> 
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (Rescan (Just t)) -> do
            whenOnline $ do
                runDB resetRescan
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                liftIO $ atomically $ writeTBMChan rChan $ NodeRescan t
            return $ RescanRes t
        Success (Rescan Nothing) -> do
            fstKeyTimeM <- runDB firstKeyTime
            let fstKeyTime   = fromJust fstKeyTimeM       
                fastCatchupI = max 0 ((toInteger fstKeyTime) - 86400 * 7)
                fc           = fromInteger fastCatchupI :: Word32
            when (isNothing fstKeyTimeM) $ liftIO $ throwIO $
                WalletException "No keys have been generated in the wallet"
            whenOnline $ do
                runDB resetRescan
                HaskoinServer _ rChanM _ <- getYesod
                let rChan      = fromJust rChanM
                liftIO $ atomically $ do
                    writeTBMChan rChan $ NodeRescan fc
            return $ RescanRes fstKeyTime
        -- TODO: What happens here ?
        Error _ -> undefined

