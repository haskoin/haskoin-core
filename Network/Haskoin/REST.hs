{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
module Network.Haskoin.REST 
( 
  -- * REST Server
  ServerMode(..)
, ServerConfig(..)
, runServer
, haskoinPort

  -- * REST Types
, NewWallet(..) 
, MnemonicRes(..)
, NewAccount(..)
, AddressPageRes(..)
, TxPageRes(..)
, AddressData(..)
, AccTxAction(..)
, TxHashStatusRes(..)
, TxRes(..)
, TxStatusRes(..)
, BalanceRes(..)
, SpendableRes(..)
, NodeAction(..)
, RescanRes(..)
)
where

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
import qualified Data.ByteString as BS (empty)

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
    , boolField
    , parseJsonBody
    , iopt
    , runInputGet
    , permissionDenied
    , invalidArgs
    )
import Yesod.Static (Static, staticDevel)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Middleware.HttpAuth (basicAuth)

import Network.Haskoin.Constants
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction

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
    , serverStatic :: Static
    }

instance Yesod HaskoinServer where
    makeSessionBackend _ = return Nothing

instance YesodPersist HaskoinServer where
    type YesodPersistBackend HaskoinServer = SqlBackend
    
    runDB action = do
        pool <- fmap serverPool getYesod
        runSqlPool action pool

instance RenderMessage HaskoinServer FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "HaskoinServer" [parseRoutes|
/wallets                                           WalletsR     GET POST
/wallets/#Text                                     WalletR      GET
/wallets/#Text/accounts                            AccountsR    GET POST
/wallets/#Text/accounts/#Text                      AccountR     GET
/wallets/#Text/accounts/#Text/keys                 AccountKeysR POST
/wallets/#Text/accounts/#Text/addrs                AddressesR   GET POST
/wallets/#Text/accounts/#Text/addrs/#Int           AddressR     GET PUT
/wallets/#Text/accounts/#Text/txs                  TxsR         GET POST
/wallets/#Text/accounts/#Text/txs/#Text            TxR          GET
/wallets/#Text/accounts/#Text/txs/#Text/sigblob    SigBlobR     GET
/wallets/#Text/accounts/#Text/balance              BalanceR     GET
/wallets/#Text/accounts/#Text/spendablebalance     SpendableR   GET
/node                                              NodeR        POST
|]

runServer :: ServerConfig -> IO ()
runServer config = do
    let walletFile = pack "wallet"
    staticSite <- staticDevel "html"

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
            app <- toWaiApp $ HaskoinServer
                { serverPool = pool
                , serverNode = Nothing
                , serverConfig = config
                , serverStatic = staticSite
                }
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
                    app <- toWaiApp $ HaskoinServer
                        { serverPool = pool
                        , serverNode = (Just rChan)
                        , serverConfig = config
                        , serverStatic = staticSite
                        }
                    runApp app

guardVault :: Handler ()
guardVault = do
    config <- fmap serverConfig getYesod
    when (configMode config == ServerVault) $ 
        permissionDenied "This operation is not supported in vault mode"

whenOnline :: Handler () -> Handler ()
whenOnline action = do
    config <- fmap serverConfig getYesod
    when (configMode config == ServerOnline) action

updateNode :: Handler a -> Handler a
updateNode action = do
    res <- action
    whenOnline $ do
        hs <- getYesod
        let rChanM = serverNode hs
            rChan = fromJust rChanM
            config = serverConfig hs
        bloom <- runDB $ walletBloomFilter $ configBloomFP config
        liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
    return res

handleErrors :: Handler a -> Handler a
handleErrors action = 
    catch action f
  where
    f (WalletException err) = invalidArgs [pack err]

getWalletsR :: Handler Value
getWalletsR = handleErrors $ guardVault >> toJSON <$> runDB walletList

postWalletsR :: Handler Value
postWalletsR = handleErrors $ guardVault >> 
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
getWalletR wallet = handleErrors $ guardVault >>
    toJSON <$> runDB (getWallet $ unpack wallet)

getAccountsR :: Text -> Handler Value
getAccountsR wallet = handleErrors $ 
    toJSON <$> runDB (accountList $ unpack wallet)

postAccountsR :: Text -> Handler Value
postAccountsR wallet = handleErrors $ do
    config <- fmap serverConfig getYesod
    let c = configGap config
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (NewAccount n) -> do
            acc <- runDB $ newAccount w n
            updateNode $ runDB $ addLookAhead w n c
            return acc
        Success (NewMSAccount n r t ks) -> do
            acc <- runDB $ newMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead w n c
            return acc
        Success (NewReadAccount n k) -> do
            acc <- runDB $ newReadAccount w n k
            updateNode $ runDB $ addLookAhead w n c
            return acc
        Success (NewReadMSAccount n r t ks) -> do
            acc <- runDB $ newReadMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead w n c
            return acc
        -- TODO: What happens here ?
        Error _ -> undefined
  where
    w = unpack wallet

getAccountR :: Text -> Text -> Handler Value
getAccountR wallet name = handleErrors $ 
    toJSON <$> runDB (getAccount (unpack wallet) (unpack name))

postAccountKeysR :: Text -> Text -> Handler Value
postAccountKeysR wallet name = handleErrors $ do
    config <- fmap serverConfig getYesod
    let c = configGap config
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success ks -> do
            acc <- runDB $ addAccountKeys w n ks
            when (length (accountKeys acc) == accountTotal acc) $ do
                updateNode $ runDB $ addLookAhead w n c
            return acc
        -- TODO: What happens here ?
        Error _ -> undefined
  where
    n = unpack name
    w = unpack wallet

getAddressesR :: Text -> Text -> Handler Value
getAddressesR wallet name = handleErrors $ do
    (pageM, elemM, confM, internalM) <- runInputGet $ (,,,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
        <*> iopt intField "minconf"
        <*> iopt boolField "internal"
    let minConf  = fromMaybe 0 confM
        internal = fromMaybe False internalM
        e        = fromMaybe 10 elemM
    runDB $ if isJust pageM
        then do
            (pa, m) <- addressPage w n (fromJust pageM) e internal
            ba <- mapM (flip addressBalance minConf) pa
            return $ toJSON $ AddressPageRes ba m
        else do
            pa <- addressList w n internal
            liftM toJSON $ mapM (flip addressBalance minConf) pa
  where
    n = unpack name
    w = unpack wallet

postAddressesR :: Text -> Text -> Handler Value
postAddressesR wallet name = handleErrors $ do
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (AddressData label) -> updateNode $ do
            addr' <- runDB $ unlabeledAddr w n
            runDB $ setAddrLabel w n (addressIndex addr') label
        -- TODO: What happens here ?
        Error _ -> undefined
  where
    n = unpack name
    w = unpack wallet

getAddressR :: Text -> Text -> Int -> Handler Value
getAddressR wallet name i = handleErrors $ do
    (confM, internalM) <- runInputGet $ (,)
        <$> iopt intField "minconf"
        <*> iopt boolField "internal"
    let minConf  = fromMaybe 0 confM
        internal = fromMaybe False internalM
    runDB $ do
        pa <- getAddress (unpack wallet) (unpack name) (fromIntegral i) internal
        ba <- addressBalance pa minConf
        return $ toJSON ba

putAddressR :: Text -> Text -> Int -> Handler Value
putAddressR wallet name i = handleErrors $ do 
    parseJsonBody >>= \res -> case res of
        Success (AddressData label) -> runDB $ 
            toJSON <$> setAddrLabel w n (fromIntegral i) label
        -- TODO: What happens here ?
        Error _ -> undefined
  where
    w = unpack wallet
    n = unpack name

getTxsR :: Text -> Text -> Handler Value
getTxsR wallet name = handleErrors $ do
    (pageM, elemM) <- runInputGet $ (,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
    if isJust pageM
        then do
            let e | isJust elemM = fromJust elemM
                  | otherwise    = 10
            (as, m) <- runDB $ txPage w n (fromJust pageM) e
            return $ toJSON $ TxPageRes as m
        else toJSON <$> runDB (txList w n)
  where
    w = unpack wallet
    n = unpack name

postTxsR :: Text -> Text -> Handler Value
postTxsR wallet name = handleErrors $ guardVault >>
    parseJsonBody >>= \res -> case res of
        Success (SendCoins rs fee minConf) -> do
            (tid, complete, p) <- runDB $ sendTx w n minConf rs fee
            whenOnline $ when complete $ do
                rChanM <- fmap serverNode getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            let prop = if complete then Nothing else Just p
            return $ toJSON $ TxHashStatusRes tid complete prop
        Success (SignTx tx) -> do
            (tid, complete, p) <- runDB $ signWalletTx w n tx
            whenOnline $ when complete $ do
                rChanM <- fmap serverNode getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            let prop = if complete then Nothing else Just p
            return $ toJSON $ TxHashStatusRes tid complete prop
        Success (SignSigBlob blob) -> do
            (tx, complete, p) <- runDB $ signSigBlob w n blob
            let prop = if complete then Nothing else Just p
            return $ toJSON $ TxStatusRes tx complete prop
        Success (ImportTx tx) -> do
            resM <- runDB $ importTx tx UnknownSource (Just (w, n))
            let (tid, conf) = fromJust resM
                complete = conf `elem` [ TxPending, TxBuilding ]
            when (isNothing resM) $ liftIO $ throwIO $
                WalletException "Transaction could not be imported"
            whenOnline $ when complete $ do
                Just rChan <- serverNode <$> getYesod
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            let prop = if complete then Nothing else Just tx{ txIn = 
                    map (\ti -> ti{ scriptInput = BS.empty }) $ txIn tx }
            return $ toJSON $ TxHashStatusRes tid complete prop
        -- TODO: What happens here ?
        Error _ -> undefined
  where
    w = unpack wallet
    n = unpack name
    

getTxR :: Text -> Text -> Text -> Handler Value
getTxR wallet name tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
        tid  = fromJust tidM
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    aTx <- runDB $ getAccTx (unpack wallet) (unpack name) tid
    return $ toJSON aTx

getSigBlobR :: Text -> Text -> Text -> Handler Value
getSigBlobR wallet name tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    blob <- runDB $ getSigBlob (unpack wallet) (unpack name) $ fromJust tidM
    return $ toJSON blob

getBalanceR :: Text -> Text -> Handler Value
getBalanceR wallet name = handleErrors $ do
    confM <- runInputGet $ iopt intField "minconf"
    let minConf = fromMaybe 0 confM
    (balance, cs) <- runDB $ accountBalance w n minConf
    return $ toJSON $ BalanceRes balance cs
  where
    w = unpack wallet
    n = unpack name

getSpendableR :: Text -> Text -> Handler Value
getSpendableR wallet name = handleErrors $ do
    confM <- runInputGet $ iopt intField "minconf"
    let minConf = fromMaybe 0 confM
    balance <- runDB $ spendableAccountBalance w n minConf
    return $ toJSON $ SpendableRes balance
  where
    w = unpack wallet
    n = unpack name

postNodeR :: Handler Value
postNodeR = handleErrors $ guardVault >> 
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (Rescan (Just t)) -> do
            whenOnline $ do
                runDB resetRescan
                rChanM <- fmap serverNode getYesod
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
                rChanM <- fmap serverNode getYesod
                let rChan      = fromJust rChanM
                liftIO $ atomically $ do
                    writeTBMChan rChan $ NodeRescan fc
            return $ RescanRes fstKeyTime
        -- TODO: What happens here ?
        Error _ -> undefined

