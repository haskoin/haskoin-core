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
import Control.Exception (SomeException(..), throwIO, tryJust, handle)

import Data.Aeson 
    ( Value (Object, Null)
    , Result(..)
    , object
    , ToJSON
    , toJSON
    , FromJSON
    , parseJSON
    , withObject
    , (.:), (.:?), (.=)
    )
import Data.Maybe
import Data.String (fromString)
import Data.Conduit (Sink, awaitForever, ($$), ($=))
import Data.Conduit.Network ()
import Data.Conduit.TMChan
import Data.Text (Text, unpack, pack)
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

data HaskoinServer = HaskoinServer ConnectionPool

instance Yesod HaskoinServer

instance YesodPersist HaskoinServer where
    type YesodPersistBackend HaskoinServer = SqlPersistT
    
    runDB action = do
        HaskoinServer pool <- getYesod
        runSqlPool action pool

instance RenderMessage HaskoinServer FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "HaskoinServer" [parseRoutes|
/api/wallets                               WalletsR     GET POST
/api/wallets/#Text                         WalletR      GET
/api/accounts                              AccountsR    GET POST
/api/accounts/#Text                        AccountR     GET
/api/accounts/#Text/keys                   AccountKeysR POST
/api/accounts/#Text/addresses              AddressesR   GET POST
/api/accounts/#Text/addresses/#Int         AddressR     GET PUT
/api/accounts/#Text/txs                    TxsR         GET POST
|]
{-
/api/accounts/#Text/txs/#TxHash            TxR          GET 
/api/accounts/#Text/balance                BalanceR     GET
/api/accounts/#Text/txs/#TxHash/sigblob    SigBlobR     GET 
/api/accounts/#Text/sigblobs               SigBlobsR    GET
/api/node                                  NodeR        POST
-}

getWalletsR :: Handler Value
getWalletsR = toJSON <$> runDB walletList

postWalletsR :: Handler Value
postWalletsR = parseJsonBody >>= \res -> case res of
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
getWalletR wname = toJSON <$> runDB (getWallet $ unpack wname)

getAccountsR :: Handler Value
getAccountsR = toJSON <$> runDB accountList

postAccountsR :: Handler Value
postAccountsR = parseJsonBody >>= \res -> case res of
    Success (NewAccount w n) -> do
        a <- runDB $ do
            fstKeyBefore <- firstKeyTime
            a <- newAccount w n
            replicateM_ 30 $ addLookAhead n
            return a
        -- bloom <- walletBloomFilter fp
        -- fstKeyTime <- liftM fromJust firstKeyTime
        -- liftIO $ atomically $ do
        --     writeTBMChan rChan $ BloomFilterUpdate bloom
        --     when (isNothing fstKeyBefore) $
        --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON a
    Success (NewMSAccount w n r t ks) -> do
        a <- runDB $ do
            fstKeyBefore <- firstKeyTime
            a <- newMSAccount w n r t ks
            when (length (accountKeys a) == t) $
                replicateM_ 30 $ addLookAhead n
            return a
            -- bloom <- walletBloomFilter fp
            -- fstKeyTime <- liftM fromJust firstKeyTime
            -- liftIO $ atomically $ do
            --     writeTBMChan rChan $ BloomFilterUpdate bloom
            --     when (isNothing fstKeyBefore) $
            --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON a
    Success (NewReadAccount n k) -> do
        a <- runDB $ do
            fstKeyBefore <- firstKeyTime
            a <- newReadAccount n k
            replicateM_ 30 $ addLookAhead n
            return a
        -- bloom <- walletBloomFilter fp
        -- fstKeyTime <- liftM fromJust firstKeyTime
        -- liftIO $ atomically $ do
        --     writeTBMChan rChan $ BloomFilterUpdate bloom
        --     when (isNothing fstKeyBefore) $
        --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON a
    Success (NewReadMSAccount n r t ks) -> do
        a <- runDB $ do
            fstKeyBefore <- firstKeyTime
            a <- newReadMSAccount n r t ks
            when (length (accountKeys a) == t) $
                replicateM_ 30 $ addLookAhead n
            -- bloom <- walletBloomFilter fp
            -- fstKeyTime <- liftM fromJust firstKeyTime
            -- liftIO $ atomically $ do
            --     writeTBMChan rChan $ BloomFilterUpdate bloom
            --     when (isNothing fstKeyBefore) $
            --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON a
    Error err -> undefined

getAccountR :: Text -> Handler Value
getAccountR name = toJSON <$> runDB (getAccount $ unpack name)

postAccountKeysR :: Text -> Handler Value
postAccountKeysR name = parseJsonBody >>= \res -> case res of
    Success [ks] -> do
        a <- runDB $ do
            fstKeyBefore <- firstKeyTime
            a <- addAccountKeys (unpack name) ks
            when (length (accountKeys a) == accountTotal a) $ do
                replicateM_ 30 $ addLookAhead (unpack name)
            -- bloom      <- walletBloomFilter fp
            -- fstKeyTime <- liftM fromJust firstKeyTime
            -- liftIO $ atomically $ do
            --     writeTBMChan rChan $ BloomFilterUpdate bloom
            --     when (isNothing fstKeyBefore) $
            --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON a
    Error err -> undefined

getAddressesR :: Text -> Handler Value
getAddressesR name = do
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
postAddressesR name = parseJsonBody >>= \res -> case res of
    Success (AddressData label) -> runDB $ do
        fstKeyBefore <- firstKeyTime
        addr' <- newAddr $ unpack name
        addr  <- setAddrLabel (unpack name) (addressIndex addr') label
        -- bloom <- walletBloomFilter fp
        -- fstKeyTime <- liftM fromJust firstKeyTime
        -- liftIO $ atomically $ do
        --     writeTBMChan rChan $ BloomFilterUpdate bloom
        --     when (isNothing fstKeyBefore) $
        --         writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ toJSON addr
    Error err -> undefined

getAddressR :: Text -> Int -> Handler Value
getAddressR name i = do
    addr <- runDB $ getAddress (unpack name) (fromIntegral i)
    return $ toJSON addr

putAddressR :: Text -> Int -> Handler Value
putAddressR name i = parseJsonBody >>= \res -> case res of
    Success (AddressData label) -> runDB $ do
        addr <- setAddrLabel (unpack name) (fromIntegral i) label
        return $ toJSON addr
    Error err -> undefined

getTxsR :: Text -> Handler Value
getTxsR name = do
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
postTxsR name = parseJsonBody >>= \res -> case res of
    Success (SendCoins rs fee) -> runDB $ do
        (tid, complete) <- sendTx (unpack name) rs fee
        -- when complete $ do
        --     newTx <- getTx tid
        --     when (isJust rChanM) $ liftIO $ atomically $ do
        --         writeTBMChan rChan $ PublishTx newTx
        return $ toJSON $ TxHashStatusRes tid complete
    Success (SignTx tx) -> runDB $ do
        (tid, complete) <- signWalletTx (unpack name) tx
        -- when complete $ do
        --     newTx <- getTx tid
        --     when (isJust rChanM) $ liftIO $ atomically $ do
        --         writeTBMChan rChan $ PublishTx newTx
        return $ toJSON $ TxHashStatusRes tid complete
    Error err -> undefined

runServer :: IO ()
runServer = do
    dir <- getWorkDir
    let walletFile = pack $ concat [dir, "/wallet"]
    pool <- createSqlPool (wrapConnection =<< open walletFile) 1
    flip runSqlPersistMPool pool $ do 
        _ <- runMigrationSilent migrateWallet 
        initWalletDB
    app <- toWaiApp $ HaskoinServer pool
    let settings = setHost "127.0.0.1" $ setPort 8555 defaultSettings
    runSettings settings app

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    haskoinDir <- getAppUserDataDirectory "haskoin"
    let dir = concat [ haskoinDir, "/", networkName ]
    createDirectoryIfMissing True dir
    return dir

{-
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

    let bind    = fromString $ configBind $ fromJust configM
        port    = configPort $ fromJust configM
        hosts   = configHosts $ fromJust configM
        batch   = configBatch $ fromJust configM
        fp      = configFpRate $ fromJust configM
        offline = configOffline $ fromJust configM

    -- Create sqlite connection & initialization
    mvar <- newMVar =<< wrapConnection =<< open walletFile
    runDB mvar $ do
        _ <- runMigrationSilent migrateWallet 
        initWalletDB

    if offline || null hosts
      then
        -- Launch JSON-RPC server
        tcpServer V2 (serverSettings port bind) $ \(src, snk) -> src
            $= CL.mapM (processWalletRequest mvar Nothing fp offline)
            $$ snk
      else
        -- Launch SPV node
        withAsyncNode dir batch $ \eChan rChan _ -> do
        let eventPipe = sourceTBMChan eChan $$ 
                        processNodeEvents mvar rChan fp
        withAsync eventPipe $ \_ -> do
        bloom <- runDB mvar $ walletBloomFilter fp
        atomically $ do
            -- Bloom filter
            writeTBMChan rChan $ BloomFilterUpdate bloom
            -- Bitcoin hosts to connect to
            forM_ hosts $ \(h,p) -> writeTBMChan rChan $ ConnectNode h p

        -- Launch JSON-RPC server
        tcpServer V2 (serverSettings port bind) $ \(src, snk) -> src
            $= CL.mapM (processWalletRequest mvar (Just rChan) fp offline)
            $$ snk

processNodeEvents :: MVar Connection -> TBMChan NodeRequest -> Double
                  -> Sink NodeEvent IO ()
processNodeEvents mvar rChan fp = awaitForever $ \e -> do
    res <- lift $ tryJust f $ runDB mvar $ case e of
        MerkleBlockEvent xs -> void $ importBlocks xs
        TxEvent ts          -> do
            before <- count ([] :: [Filter (DbAddressGeneric b)])
            forM_ ts $ \tx -> importTx tx NetworkSource
            after <- count ([] :: [Filter (DbAddressGeneric b)])
            -- Update the bloom filter if new addresses were generated
            when (after > before) $ do
            bloom <- walletBloomFilter fp
            liftIO $ atomically $ do
            writeTBMChan rChan $ BloomFilterUpdate bloom
                
    when (isLeft res) $ liftIO $ print $ fromLeft res
  where
    -- TODO: What if we have other exceptions than WalletException ?
    f (SomeException e) = Just $ show e

processWalletRequest :: MVar Connection 
                     -> Maybe (TBMChan NodeRequest)
                     -> Double
                     -> Bool
                     -- -> (WalletRequest, Int) 
                     -> IncomingMsg () WalletRequest () ()
                     -- -> IO (Either String WalletResponse, Int)
                     -> IO (Message () () WalletResponse)
processWalletRequest mvar rChanM fp offline inmsg =
    handle f $ runDB mvar $ g inmsg
  where
    rChan = fromJust rChanM

    unlessOffline action
        | offline = liftIO $ throwIO $ WalletException 
            "This operation is not supported in offline mode"
        | otherwise = action

    f (SomeException e) =
        return $ MsgError (ErrorObj V2 (show e) (-32603) Null i)
      where
        i = case inmsg of
            IncomingMsg (MsgRequest rq) Nothing -> (getReqId rq)
            _ -> IdNull

    g (IncomingError e) = return $ MsgError e
    g (IncomingMsg (MsgRequest rq) Nothing) = do
        s <- go (getReqParams rq)
        return $ MsgResponse (Response (getReqVer rq) s (getReqId rq))
    g _ = undefined

    go (NewWallet n p m) = unlessOffline $
        liftM ResMnemonic $ newWalletMnemo n p m
    go (GetWallet n) = unlessOffline $ liftM ResWallet $ getWallet n
    go WalletList = unlessOffline $ liftM ResWalletList $ walletList
    go (NewAccount w n)  = do
        fstKeyBefore <- firstKeyTime
        a <- newAccount w n
        unless offline $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter fp
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (NewMSAccount w n r t ks) = do
        fstKeyBefore <- firstKeyTime
        a <- newMSAccount w n r t ks
        unless offline $ when (length (accountKeys a) == t) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter fp
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (NewReadAccount n k) = do
        fstKeyBefore <- firstKeyTime
        a <- newReadAccount n k
        unless offline $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter fp
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (NewReadMSAccount n r t ks) = do
        fstKeyBefore <- firstKeyTime
        a <- newReadMSAccount n r t ks
        unless offline $ when (length (accountKeys a) == t) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter fp
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (AddAccountKeys n ks) = do
        fstKeyBefore <- firstKeyTime
        a <- addAccountKeys n ks
        unless offline $ when (length (accountKeys a) == accountTotal a) $ do
            setLookAhead n 30
            bloom      <- walletBloomFilter fp
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ BloomFilterUpdate bloom
                when (isNothing fstKeyBefore) $
                    writeTBMChan rChan $ FastCatchupTime fstKeyTime
        return $ ResAccount a
    go (GetAccount n)         = liftM ResAccount $ getAccount n
    go AccountList            = liftM ResAccountList $ accountList
    go (GenAddress n i')      = do
        fstKeyBefore <- firstKeyTime
        addrs      <- newAddrs n i'
        bloom      <- walletBloomFilter fp
        unless offline $ do
            fstKeyTime <- liftM fromJust firstKeyTime
            when (isJust rChanM) $ liftIO $ atomically $ do
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
    go (TxSend n xs s) = unlessOffline $ do
        (tid, complete) <- sendTx n xs s
        when complete $ do
            newTx <- getTx tid
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ PublishTx newTx
        return $ ResTxHashStatus tid complete
    go (TxSign n tx)   = unlessOffline $ do
        (tid, complete) <- signWalletTx n tx 
        when complete $ do
            newTx <- getTx tid
            when (isJust rChanM) $ liftIO $ atomically $ do
                writeTBMChan rChan $ PublishTx newTx
        return $ ResTxHashStatus tid complete
    go (GetSigBlob n tid) = unlessOffline $ do
        blob <- getSigBlob n tid 
        return $ ResSigBlob blob
    go (SignSigBlob n blob) = do
        (tx, c) <- signSigBlob n blob
        return $ ResTxStatus tx c
    go (TxGet h) = do
        tx <- getTx h
        return $ ResTx tx
    go (Balance n)     = liftM ResBalance $ balance n
    go (Rescan (Just t)) = unlessOffline $ do
        when (isJust rChanM) $ liftIO $ atomically $ do
            writeTBMChan rChan $ FastCatchupTime t
        return $ ResRescan t
    go (Rescan Nothing) = unlessOffline $ do
        fstKeyTimeM <- firstKeyTime
        if (isJust fstKeyTimeM)
            then do
                when (isJust rChanM) $ liftIO $ atomically $ do
                    writeTBMChan rChan $ FastCatchupTime $ fromJust fstKeyTimeM
                return $ ResRescan $ fromJust fstKeyTimeM
            else liftIO $ throwIO $ WalletException
                "No keys have been generated in the wallet"

-}

