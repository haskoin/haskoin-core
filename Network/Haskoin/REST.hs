{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Haskoin.REST where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )

import Control.Applicative ((<$>), (<*>))
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception 
    ( SomeException(..)
    , throwIO
    , tryJust
    , handle
    )
import Control.Exception.Lifted (catch)

import Data.Aeson 
    ( Value (Object, Null)
    , Result(..)
    , object
    , ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , withObject
    , withText
    , (.:), (.:?), (.=)
    )
import Data.Maybe
import Data.String (fromString)
import Data.Conduit (Sink, awaitForever, ($$), ($=))
import Data.Conduit.Network ()
import Data.Conduit.TMChan
import Data.Text (Text, unpack, pack)
import Data.Default (Default, def)
import qualified Data.Conduit.List as CL

import Database.Persist.Sqlite
import Database.Sqlite (open)

import Network.Haskoin.Crypto

import Yesod
    ( Yesod
    , YesodPersist(..)
    , RenderMessage
    , FormMessage
    , renderMessage
    , defaultFormMessage
    , getYesod
    , runDB
    , mkYesod
    , parseRoutes
    , defaultLayout
    , whamlet
    , toWaiApp
    , renderRoute
    , returnJson
    , textField
    , intField
    , runFormPost
    , FormResult (FormSuccess)
    , parseJsonBody
    , iopt
    , runInputGet
    , permissionDenied
    , invalidArgs
    )
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)

import Network.Haskoin.Util
import Network.Haskoin.Constants

import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Types

import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

import Network.Haskoin.REST.Types

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
    parseJSON = withText "servermode" $ \t -> return $ case t of
        "online"  -> ServerOnline
        "offline" -> ServerOffline
        "vault"   -> ServerVault

data ServerConfig = ServerConfig
    { configBind         :: String
    , configPort         :: Int
    , configBitcoinHosts :: [(String, Int)]
    , configBatch        :: Int
    , configBloomFP      :: Double
    , configMode         :: ServerMode
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
        }

data HaskoinServer = HaskoinServer 
    { serverPool   :: ConnectionPool
    , serverNode   :: Maybe (TBMChan NodeRequest)
    , serverConfig :: ServerConfig
    }

instance Yesod HaskoinServer

instance YesodPersist HaskoinServer where
    type YesodPersistBackend HaskoinServer = SqlPersistT
    
    runDB action = do
        HaskoinServer pool _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage HaskoinServer FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "HaskoinServer" [parseRoutes|
/api/wallets                          WalletsR     GET POST
/api/wallets/#Text                    WalletR      GET
/api/accounts                         AccountsR    GET POST
/api/accounts/#Text                   AccountR     GET
/api/accounts/#Text/keys              AccountKeysR POST
/api/accounts/#Text/addresses         AddressesR   GET POST
/api/accounts/#Text/addresses/#Int    AddressR     GET PUT
/api/accounts/#Text/txs               TxsR         GET POST
/api/txs/#Text                        TxR          GET 
/api/accounts/#Text/balance           BalanceR     GET
/api/accounts/#Text/txs/#Text/sigblob SigBlobR     GET 
/api/accounts/#Text/sigblobs          SigBlobsR    POST
/api/node                             NodeR        POST
|]

runServer :: ServerConfig -> IO ()
runServer config = do
    dir <- getWorkDir
    let walletFile = pack $ concat [dir, "/wallet"]

    pool <- createSqlPool (wrapConnection =<< open walletFile) 1
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

    if mode `elem` [ServerOffline, ServerVault]
        then do
            app <- toWaiApp $ HaskoinServer pool Nothing config
            runSettings settings app
        else do
            -- Launch SPV node
            withAsyncNode dir batch $ \eChan rChan _ -> do
            let eventPipe = sourceTBMChan eChan $$ 
                            processNodeEvents pool rChan fp
            withAsync eventPipe $ \_ -> do
            bloom <- flip runSqlPersistMPool pool $ walletBloomFilter fp
            atomically $ do
                -- Bloom filter
                writeTBMChan rChan $ BloomFilterUpdate bloom
                -- Bitcoin hosts to connect to
                forM_ hosts $ \(h,p) -> writeTBMChan rChan $ ConnectNode h p

            app <- toWaiApp $ HaskoinServer pool (Just rChan) config
            runSettings settings app

processNodeEvents :: ConnectionPool 
                  -> TBMChan NodeRequest 
                  -> Double 
                  -> Sink NodeEvent IO ()
processNodeEvents pool rChan fp = awaitForever $ \e -> do
    res <- lift $ tryJust f $ flip runSqlPersistMPool pool $ case e of
        MerkleBlockEvent xs -> void $ importBlocks xs
        TxEvent ts          -> do
            before <- count ([] :: [Filter (DbAddressGeneric b)])
            forM_ ts $ \tx -> importTx tx NetworkSource
            after <- count ([] :: [Filter (DbAddressGeneric b)])
            -- Update the bloom filter if new addresses were generated
            when (after > before) $ do
                bloom <- walletBloomFilter fp
                liftIO $ atomically $ writeTBMChan rChan $ 
                    BloomFilterUpdate bloom
    when (isLeft res) $ liftIO $ print $ fromLeft res
  where
    f (SomeException e) = Just $ show e

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
    fstKeyBefore <- runDB firstKeyTime
    res <- action
    whenOnline $ do
        HaskoinServer _ rChanM config <- getYesod
        let rChan = fromJust rChanM
        bloom      <- runDB $ walletBloomFilter $ configBloomFP config
        fstKeyTime <- runDB $ liftM fromJust firstKeyTime
        liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
            when (isNothing fstKeyBefore) $
                writeTBMChan rChan $ FastCatchupTime fstKeyTime
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
        Error err -> undefined

getWalletR :: Text -> Handler Value
getWalletR wname = handleErrors $ do
    guardVault 
    toJSON <$> runDB (getWallet $ unpack wname)

getAccountsR :: Handler Value
getAccountsR = handleErrors $ toJSON <$> runDB accountList

postAccountsR :: Handler Value
postAccountsR = handleErrors $ do
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (NewAccount w n) -> do
            acc <- runDB $ newAccount w n
            updateNode $ runDB $ addLookAhead n 10
            return acc
        Success (NewMSAccount w n r t ks) -> do
            acc <- runDB $ newMSAccount w n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead n 10
            return acc
        Success (NewReadAccount n k) -> do
            acc <- runDB $ newReadAccount n k
            updateNode $ runDB $ addLookAhead n 10
            return acc
        Success (NewReadMSAccount n r t ks) -> do
            acc <- runDB $ newReadMSAccount n r t ks
            when (length (accountKeys acc) == t) $ 
                updateNode $ runDB $ addLookAhead n 10
            return acc
        Error err -> undefined

getAccountR :: Text -> Handler Value
getAccountR name = handleErrors $ toJSON <$> runDB (getAccount $ unpack name)

postAccountKeysR :: Text -> Handler Value
postAccountKeysR name = handleErrors $ do
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success [ks] -> do
            acc <- runDB $ addAccountKeys (unpack name) ks
            when (length (accountKeys acc) == accountTotal acc) $ do
                updateNode $ runDB $ addLookAhead (unpack name) 10
            return acc
        Error err -> undefined

getAddressesR :: Text -> Handler Value
getAddressesR name = handleErrors $ do
    (pageM, elemM) <- runInputGet $ (,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
    if isJust pageM
        then do
            let elem | isJust elemM = fromJust elemM
                     | otherwise    = 10
            (as, m) <- runDB $ addressPage (unpack name) (fromJust pageM) elem
            return $ toJSON $ AddressPageRes as m
        else toJSON <$> runDB (addressList $ unpack name)

postAddressesR :: Text -> Handler Value
postAddressesR name = handleErrors $ do
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (AddressData label) -> updateNode $ do
            addr' <- runDB $ unlabeledAddr $ unpack name
            runDB $ setAddrLabel (unpack name) (addressIndex addr') label
        Error err -> undefined

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
        Error err -> undefined

getTxsR :: Text -> Handler Value
getTxsR name = handleErrors $ do
    (pageM, elemM) <- runInputGet $ (,)
        <$> iopt intField "page"
        <*> iopt intField "elemperpage"
    if isJust pageM
        then do
            let elem | isJust elemM = fromJust elemM
                     | otherwise    = 10
            (as, m) <- runDB $ txPage (unpack name) (fromJust pageM) elem
            return $ toJSON $ TxPageRes as m
        else toJSON <$> runDB (txList $ unpack name)

postTxsR :: Text -> Handler Value
postTxsR name = handleErrors $ guardVault >>
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (SendCoins rs fee) -> do
            (tid, complete) <- runDB $ sendTx (unpack name) rs fee
            whenOnline $ when complete $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            return $ TxHashStatusRes tid complete
        Success (SignTx tx) -> do
            (tid, complete) <- runDB $ signWalletTx (unpack name) tx
            whenOnline $ when complete $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                newTx <- runDB $ getTx tid
                liftIO $ atomically $ do writeTBMChan rChan $ PublishTx newTx
            return $ TxHashStatusRes tid complete
        Error err -> undefined

getTxR :: Text -> Handler Value
getTxR tidStr = handleErrors $ do
    let tidM = decodeTxHashLE $ unpack tidStr
    unless (isJust tidM) $ liftIO $ throwIO $
        WalletException "Could not parse txhash"
    tx <- runDB $ getTx $ fromJust tidM
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

postSigBlobsR :: Text -> Handler Value
postSigBlobsR name = handleErrors $ do
    parseJsonBody >>= \res -> case res of
        Success blob -> do
            (tx, c) <- runDB $ signSigBlob (unpack name) blob
            return $ toJSON $ TxStatusRes tx c
        Error err -> undefined

postNodeR :: Handler Value
postNodeR = handleErrors $ guardVault >> 
    parseJsonBody >>= \res -> toJSON <$> case res of
        Success (Rescan (Just t)) -> do
            whenOnline $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan = fromJust rChanM
                liftIO $ atomically $ writeTBMChan rChan $ FastCatchupTime t
            return $ RescanRes t
        Success (Rescan Nothing) -> do
            fstKeyTimeM <- runDB firstKeyTime
            let fstKeyTime = fromJust fstKeyTimeM       
            when (isNothing fstKeyTimeM) $ liftIO $ throwIO $
                WalletException "No keys have been generated in the wallet"
            whenOnline $ do
                HaskoinServer _ rChanM _ <- getYesod
                let rChan      = fromJust rChanM
                liftIO $ atomically $ do
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
            return $ RescanRes fstKeyTime
        Error err -> undefined

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    haskoinDir <- getAppUserDataDirectory "haskoin"
    let dir = concat [ haskoinDir, "/", networkName ]
    createDirectoryIfMissing True dir
    return dir

