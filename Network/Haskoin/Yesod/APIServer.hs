{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
module Network.Haskoin.Yesod.APIServer
( 
  -- * REST Server
  ServerMode(..)
, ServerConfig(..)
, runServer

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

import Control.Applicative ((<$>))
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Logger
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Exception.Lifted (catch)

import Data.Aeson 
    ( Value
    , ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , withText
    , encode
    )
import Data.Maybe
import Data.Word (Word32)
import Data.String (fromString)
import Data.Text (Text, unpack, pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time (getCurrentTime)
import Data.Default (Default, def)
import qualified Data.ByteString as BS (ByteString, empty)

import Database.Persist.Sqlite
import Database.Sqlite (open)

import Yesod
    ( Yesod(..)
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
    , iopt
    , runInputGet
    , invalidArgs
    , permissionDenied
    , defaultYesodMiddleware
    , requireJsonBody
    )
import Yesod.Static (Static, staticDevel)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)

import Network.Haskoin.Constants
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

import Network.Haskoin.Yesod.TokenAuth
import Network.Haskoin.Yesod.APIServer.Types
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
    deriving (Eq, Show, Read)

instance ToJSON ServerMode where
    toJSON m = case m of
        ServerOnline  -> "online"
        ServerOffline -> "offline"

instance FromJSON ServerMode where
    parseJSON = withText "servermode" $ \t -> case t of
        "online"  -> return ServerOnline
        "offline" -> return ServerOffline
        _         -> mzero

data ServerConfig = ServerConfig
    { configBind         :: !String
    , configPort         :: !Int
    , configAuthUrl      :: !String
    , configBitcoinHosts :: ![(String, Int)]
    , configBloomFP      :: !Double
    , configMode         :: !ServerMode
    , configGap          :: !Int
    , configToken        :: !(Maybe BS.ByteString)
    , configTokenSecret  :: !(Maybe BS.ByteString)
    } deriving (Eq, Read, Show)

instance Default ServerConfig where
    def = ServerConfig
        { configBind         = "127.0.0.1"
        , configPort         = restPort
        , configAuthUrl      = concat [ "http://localhost:", show restPort ]
        , configBitcoinHosts = [("127.0.0.1", defaultPort)]
        , configBloomFP      = 0.00001
        , configMode         = ServerOnline
        , configToken        = Nothing
        , configTokenSecret  = Nothing
        , configGap          = 10
        }
      where
        restPort | networkName == "prodnet" = 8555
                 | otherwise                = 18555

data HaskoinServer = HaskoinServer 
    { serverPool   :: ConnectionPool
    , serverNode   :: Maybe (TBMChan SPVRequest)
    , serverConfig :: ServerConfig
    , getStatic    :: Static
    }

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
/static                                            StaticR Static getStatic
|]

instance Yesod HaskoinServer where
    makeSessionBackend _ = return Nothing
    yesodMiddleware handler = defaultYesodMiddleware $
        catch handler $ \(WalletException err) -> invalidArgs [pack err]

instance YesodTokenAuth HaskoinServer where
    authUrl = (pack . configAuthUrl . serverConfig) <$> getYesod
    lookupToken ident = runDB $ do
        tokenM <- getBy $ UniqueTokenIdent $ bsToString ident
        let f (Entity _ (DbToken i s n e c)) = 
                Token (stringToBS i) (stringToBS s) n e c
        return $ f <$> tokenM
    updateToken (Token i _ n _ _) = runDB $ do
        tokenM <- getBy $ UniqueTokenIdent $ bsToString i
        case tokenM of
            Just (Entity tkey tval) -> replace tkey tval{ dbTokenNonce = n }
            Nothing -> return ()
    expireToken _ = return ()

instance YesodPersist HaskoinServer where
    type YesodPersistBackend HaskoinServer = SqlBackend
    runDB action = do
        pool <- serverPool <$> getYesod
        runSqlPool action pool

instance RenderMessage HaskoinServer FormMessage where
    renderMessage _ _ = defaultFormMessage

runServer :: ServerConfig -> IO ()
runServer config = do
    let walletFile = pack "wallet"
    staticSite <- staticDevel "html"

    let bind     = fromString $ configBind config
        port     = configPort config
        hosts    = configBitcoinHosts config
        fp       = configBloomFP config
        mode     = configMode config 
        tokenM   = configToken config
        secretM  = configTokenSecret config
        settings = setHost bind $ setPort port defaultSettings
        runApp   = runSettings settings 

    pool <- runNoLoggingT $
        createSqlPool (\lf -> open walletFile >>= flip wrapConnection lf) 1
    flip runSqlPersistMPool pool $ do 
        _ <- runMigrationSilent migrateWallet 
        initWalletDB
        -- Create a token if it doesn't already exist in the database
        when (isJust tokenM) $ do
            when (isNothing secretM) $ error $ "No token secret defined"
            let token  = fromJust tokenM
                secret = fromJust secretM
            resM <- getBy $ UniqueTokenIdent $ bsToString token
            case resM of
                Just (Entity _ res) ->
                    unless (bsToString secret == dbTokenSecret res) $ error 
                        "This token with a different secret already exists"
                Nothing -> do
                    now <- liftIO getCurrentTime
                    insert_ $ DbToken (bsToString token) (bsToString secret) 
                                0 Nothing now

    if mode == ServerOffline
        then do
            app <- toWaiApp $ HaskoinServer
                { serverPool = pool
                , serverNode = Nothing
                , serverConfig = config
                , getStatic = staticSite
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
            withAsyncSPV hosts fc bb (runNodeHandle fp pool) $ 
                \rChan _ -> do
                    -- Send the bloom filter
                    bloom <- flip runSqlPersistMPool pool $ walletBloomFilter fp
                    atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
                    -- Launch the haskoin server
                    app <- toWaiApp $ HaskoinServer
                        { serverPool = pool
                        , serverNode = (Just rChan)
                        , serverConfig = config
                        , getStatic = staticSite
                        }
                    runApp app

whenOnline :: Handler () -> Handler ()
whenOnline handler = do
    mode <- (configMode . serverConfig) <$> getYesod
    when (mode == ServerOnline) handler

updateNodeFilter :: Handler ()
updateNodeFilter = do
    hs <- getYesod
    let rChan = fromJust $ serverNode hs
        config = serverConfig hs
    bloom <- runDB $ walletBloomFilter $ configBloomFP config
    liftIO $ atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom

requireJsonBodyAuth :: (FromJSON a, ToJSON a) => Handler a
requireJsonBodyAuth = requireJsonBody >>= \x -> do
    checkAuthorization $ toStrictBS $ encode x
    return x

requireAuth :: Handler ()
requireAuth = checkAuthorization BS.empty

checkAuthorization :: BS.ByteString -> Handler ()
checkAuthorization body = getYesod >>= \hs -> do
    let tokenM = configToken $ serverConfig hs
    if isNothing tokenM
        then return ()
        else extractVerifyToken body >>= \tokenE -> case tokenE of
            Left err    -> permissionDenied $ pack err
            Right token -> 
                if Just (tokenIdent token) == tokenM
                    then return ()
                    else permissionDenied "Invalid or expired token"

getWalletsR :: Handler Value
getWalletsR = requireAuth >> toJSON <$> runDB walletList

postWalletsR :: Handler Value
postWalletsR = requireJsonBodyAuth >>= \(NewWallet name pass msM) -> do
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

getWalletR :: Text -> Handler Value
getWalletR wallet = requireAuth >> do
    toJSON <$> runDB (getWallet $ unpack wallet)

getAccountsR :: Text -> Handler Value
getAccountsR wallet = requireAuth >> do
    toJSON <$> runDB (accountList $ unpack wallet)

postAccountsR :: Text -> Handler Value
postAccountsR wallet = requireJsonBodyAuth >>= \res -> do
    c <- (configGap . serverConfig) <$> getYesod
    toJSON <$> case res of
        NewAccount n -> do
            acc <- runDB $ newAccount w n
            runDB $ addLookAhead w n c
            whenOnline updateNodeFilter
            return acc
        NewMSAccount n r t ks -> do
            acc <- runDB $ newMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ do
                runDB $ addLookAhead w n c
                whenOnline updateNodeFilter
            return acc
        NewReadAccount n k -> do
            acc <- runDB $ newReadAccount w n k
            runDB $ addLookAhead w n c
            whenOnline updateNodeFilter
            return acc
        NewReadMSAccount n r t ks -> do
            acc <- runDB $ newReadMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ do
                runDB $ addLookAhead w n c
                whenOnline updateNodeFilter
            return acc
  where
    w = unpack wallet

getAccountR :: Text -> Text -> Handler Value
getAccountR wallet name = requireAuth >> do
    toJSON <$> runDB (getAccount (unpack wallet) (unpack name))

postAccountKeysR :: Text -> Text -> Handler Value
postAccountKeysR wallet name = requireJsonBodyAuth >>= \ks -> do
    acc <- runDB $ addAccountKeys w n ks
    when (length (accountKeys acc) == accountTotal acc) $ do
        c <- (configGap . serverConfig) <$> getYesod
        runDB $ addLookAhead w n c
        whenOnline updateNodeFilter
    return $ toJSON acc
  where
    n = unpack name
    w = unpack wallet

getAddressesR :: Text -> Text -> Handler Value
getAddressesR wallet name = requireAuth >> do
    pageM    <- runInputGet (iopt intField "page")
    e        <- (fromMaybe 10) <$> runInputGet (iopt intField "elemperpage")
    minConf  <- (fromMaybe 0) <$> runInputGet (iopt intField "minconf")
    internal <- (fromMaybe False) <$> runInputGet (iopt boolField "internal")
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
postAddressesR wallet name = requireJsonBodyAuth >>= \(AddressData label) -> do
    newAddr <- runDB $ unlabeledAddr w n
    res <- runDB $ setAddrLabel w n (addressIndex newAddr) label
    whenOnline updateNodeFilter
    return $ toJSON res
  where
    n = unpack name
    w = unpack wallet

getAddressR :: Text -> Text -> Int -> Handler Value
getAddressR wallet name i = requireAuth >> do
    minConf  <- (fromMaybe 0) <$> runInputGet (iopt intField "minconf")
    internal <- (fromMaybe False) <$> runInputGet (iopt boolField "internal")
    runDB $ do
        pa <- getAddress (unpack wallet) (unpack name) (fromIntegral i) internal
        ba <- addressBalance pa minConf
        return $ toJSON ba

putAddressR :: Text -> Text -> Int -> Handler Value
putAddressR wallet name i = requireJsonBodyAuth >>= \(AddressData label) -> 
    toJSON <$> runDB (setAddrLabel w n (fromIntegral i) label)
  where
    w = unpack wallet
    n = unpack name

getTxsR :: Text -> Text -> Handler Value
getTxsR wallet name = requireAuth >> do
    pageM <- runInputGet (iopt intField "page")
    e     <- (fromMaybe 10) <$> runInputGet (iopt intField "elemperpage")
    runDB $ case pageM of
        Just page -> (\(as, m) -> toJSON $ TxPageRes as m) <$> txPage w n page e
        Nothing -> toJSON <$> txList w n
  where
    w = unpack wallet
    n = unpack name

postTxsR :: Text -> Text -> Handler Value
postTxsR wallet name = requireJsonBodyAuth >>= \res -> case res of
    SendCoins rs fee minConf -> do
        (tid, complete, genA) <- runDB $ sendTx w n minConf rs fee
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            Just rChan <- serverNode <$> getYesod
            newTx <- runDB $ getTx tid
            liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
        return $ toJSON $ TxHashStatusRes tid complete
    SignTx tx -> do
        (tid, complete, genA) <- runDB $ signWalletTx w n tx
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            Just rChan <- serverNode <$> getYesod
            newTx <- runDB $ getTx tid
            liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
        return $ toJSON $ TxHashStatusRes tid complete
    SignSigBlob blob -> do
        (tx, complete) <- runDB $ signSigBlob w n blob
        return $ toJSON $ TxStatusRes tx complete
    ImportTx tx -> do
        resM <- runDB $ importTx tx UnknownSource (Just (w, n))
        let (tid, conf, genA) = fromJust resM
            complete = conf `elem` [ TxPending, TxBuilding ]
        when (isNothing resM) $ liftIO $ throwIO $
            WalletException "Transaction could not be imported"
        whenOnline $ when complete $ do
            when genA updateNodeFilter
            Just rChan <- serverNode <$> getYesod
            newTx <- runDB $ getTx tid
            liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
        return $ toJSON $ TxHashStatusRes tid complete
  where
    w = unpack wallet
    n = unpack name

getTxR :: Text -> Text -> Text -> Handler Value
getTxR wallet name tidStr = requireAuth >> do
    prop <- (fromMaybe False) <$> runInputGet (iopt boolField "proposition")
    let tidM = decodeTxHashLE $ unpack tidStr
        tid  = fromJust tidM
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    aTx <- runDB $ getAccTx w n tid
    toJSON <$> if prop
        then do
            p <- runDB $ getProposition w n tid
            return aTx{ accTx = p }
        else return aTx
  where
    w = unpack wallet
    n = unpack name

getSigBlobR :: Text -> Text -> Text -> Handler Value
getSigBlobR wallet name tidStr = requireAuth >> do
    let tidM = decodeTxHashLE $ unpack tidStr
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    blob <- runDB $ getSigBlob (unpack wallet) (unpack name) $ fromJust tidM
    return $ toJSON blob

getBalanceR :: Text -> Text -> Handler Value
getBalanceR wallet name = requireAuth >> do
    minConf <- (fromMaybe 0) <$> runInputGet (iopt intField "minconf")
    (balance, cs) <- runDB $ accountBalance w n minConf
    return $ toJSON $ BalanceRes balance cs
  where
    w = unpack wallet
    n = unpack name

getSpendableR :: Text -> Text -> Handler Value
getSpendableR wallet name = requireAuth >> do
    minConf <- (fromMaybe 0) <$> runInputGet (iopt intField "minconf")
    balance <- runDB $ spendableAccountBalance w n minConf
    return $ toJSON $ SpendableRes balance
  where
    w = unpack wallet
    n = unpack name

postNodeR :: Handler Value
postNodeR = requireJsonBodyAuth >>= \res -> toJSON <$> case res of
    Rescan (Just t) -> do
        whenOnline $ do
            Just rChan <- serverNode <$> getYesod
            runDB resetRescan
            liftIO $ atomically $ writeTBMChan rChan $ NodeRescan t
        return $ RescanRes t
    Rescan Nothing -> do
        fstKeyTimeM <- runDB firstKeyTime
        let fstKeyTime   = fromJust fstKeyTimeM       
            fastCatchupI = max 0 ((toInteger fstKeyTime) - 86400 * 7)
            fc           = fromInteger fastCatchupI :: Word32
        when (isNothing fstKeyTimeM) $ liftIO $ throwIO $
            WalletException "No keys have been generated in the wallet"
        whenOnline $ do
            Just rChan <- serverNode <$> getYesod
            runDB resetRescan
            liftIO $ atomically $ writeTBMChan rChan $ NodeRescan fc
        return $ RescanRes fstKeyTime

