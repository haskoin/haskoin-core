{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import System.Directory
    ( removeFile
    , createDirectoryIfMissing
    , doesFileExist
    , getAppUserDataDirectory
    , setCurrentDirectory
    )
import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    , ArgOrder (Permute)
    )
import qualified System.Environment as E (getArgs)
import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, forM_, when, mzero, liftM2)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (throwIO, throw)

import Data.Char (toLower)
import Data.Default (def)
import Data.Word (Word32, Word64)
import Data.List (intersperse)
import Data.Maybe (listToMaybe, fromJust, isNothing, isJust, fromMaybe)
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Vector as V (toList)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, splitOn)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as YAML (encode, encodeFile, decodeFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Aeson 
    ( Value (String)
    , FromJSON
    , ToJSON
    , object
    , toJSON
    , encode
    , decode
    , withObject
    , withArray
    , eitherDecode
    , (.=), (.:), (.:?)
    )
import Data.Aeson.Types
    ( Parser
    , parseJSON
    )
import qualified Data.Aeson.Encode.Pretty as JSON
    ( encodePretty'
    , defConfig
    , confIndent
    )

import Network.HTTP.Conduit 
    ( RequestBody(..)
    , Request(..)
    , Response(..)
    , httpLbs
    , withManager
    , setQueryString
    , parseUrl
    , applyBasicAuth
    )

import Network.HTTP.Types (Status(..))

import Network.Haskoin.REST

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Script
import Network.Haskoin.Crypto 
import Network.Haskoin.Transaction
import Network.Haskoin.Util
import Network.Haskoin.Constants

type Args = [String]

data Options = Options
    { optWallet   :: String
    , optCount    :: Int
    , optGap      :: Int
    , optFee      :: Word64
    , optMinConf  :: Word32
    , optInternal :: Bool
    , optJson     :: Bool
    , optYaml     :: Bool
    , optPass     :: String
    , optDetach   :: Bool
    , optProvider :: String
    , optBind     :: String
    , optPort     :: Int
    , optHosts    :: [(String, Int)]
    , optBatch    :: Int
    , optBloomFP  :: Double
    , optMode     :: ServerMode
    , optUser     :: Text
    , optPassword :: Text
    , optDir      :: Maybe FilePath
    , optLog      :: Maybe FilePath
    , optPid      :: Maybe FilePath
    , optCfg      :: Maybe FilePath
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optWallet   = "main"
    , optCount    = 5
    , optGap      = 10
    , optFee      = 10000
    , optMinConf  = 0
    , optInternal = False
    , optJson     = False
    , optYaml     = False
    , optPass     = ""
    , optDetach   = False
    , optProvider = concat [ "http://localhost:", show haskoinPort ]
    , optBind     = configBind def
    , optPort     = configPort def
    , optHosts    = configBitcoinHosts def
    , optBatch    = configBatch def
    , optBloomFP  = configBloomFP def
    , optMode     = configMode def
    , optUser     = "haskoin"
    , optPassword = "haskoin"
    , optDir      = Nothing
    , optLog      = Nothing
    , optPid      = Nothing
    , optCfg      = Nothing
    } 

instance ToJSON Options where
    toJSON opt = object $
        [ "wallet"         .= optWallet opt
        , "count"          .= optCount opt
        , "gap"            .= optGap opt
        , "fee"            .= optFee opt
        , "minconf"        .= optMinConf opt
        , "internal"       .= optInternal opt
        , "json"           .= optJson opt
        , "yaml"           .= optYaml opt
        , "passphrase"     .= optPass opt
        , "detach"         .= optDetach opt
        , "provider"       .= optProvider opt
        , "bind"           .= optBind opt
        , "port"           .= optPort opt
        , "bitcoin-hosts"  .= (map f $ optHosts opt)
        , "batch"          .= optBatch opt
        , "false-positive" .= optBloomFP opt
        , "operation-mode" .= optMode opt
        , "user"           .= optUser opt
        , "password"       .= optPassword opt
        ]
        ++ maybe [] (\x -> [("workdir" .= x)]) (optDir opt)
        ++ maybe [] (\x -> [("logfile" .= x)]) (optLog opt)
        ++ maybe [] (\x -> [("pidfile" .= x)]) (optPid opt)
      where
        f (x,y) = object
            [ "host" .= x
            , "port" .= y
            ]

instance FromJSON Options where
    parseJSON = withObject "options" $ \o -> Options
        <$> o .: "wallet"
        <*> o .: "count"
        <*> o .: "gap"
        <*> o .: "fee"
        <*> o .: "minconf"
        <*> o .: "internal"
        <*> o .: "json"
        <*> o .: "yaml"
        <*> o .: "passphrase"
        <*> o .: "detach"
        <*> o .: "provider"
        <*> o .: "bind"
        <*> o .: "port"
        <*> (mapM f =<< o .: "bitcoin-hosts")
        <*> o .: "batch"
        <*> o .: "false-positive"
        <*> o .: "operation-mode"
        <*> o .: "user"
        <*> o .: "password"
        <*> o .:? "workdir"
        <*> o .:? "logfile"
        <*> o .:? "pidfile"
        <*> return Nothing -- configuration file does not contain its own path
      where
        f = withObject "bitcoinhost" $ \x -> do
            a <- x .: "host"
            b <- x .: "port"
            return (a,b)

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['w'] ["wallet"]
        (ReqArg (\w opts -> return opts{ optWallet = w }) "WALLET")
        "Which wallet to use (default: main)"
    , Option ['c'] ["config"]
        (ReqArg (\g opts -> return opts{ optCfg = Just g }) "FILE")
        "Configuration file"
    , Option ['n'] ["count"] (ReqArg parseCount "INT")
        "Count: see commands for details"
    , Option ['f'] ["fee"] 
       (ReqArg (\f opts -> return opts{ optFee = read f }) "SATOSHI")
       "Transaction fee (default: 10000)"
    , Option ['m'] ["minconf"] (ReqArg parseMinConf "INT")
        "Minimum number of required confirmations"
    , Option ['i'] ["internal"]
        (NoArg $ \opts -> return opts{ optInternal = True })
        "Display internal addresses (default: False)"
    , Option ['j'] ["json"]
        (NoArg $ \opts -> return opts{ optJson = True })
        "Format result as JSON"
    , Option ['y'] ["yaml"]
        (NoArg $ \opts -> return opts{ optYaml = True })
        "Format result as YAML"
    , Option ['d'] ["detach"]
        (NoArg $ \opts -> return opts{ optDetach = True })
        "Detach the process from the terminal"
    , Option ['x'] ["passphrase"]
        (ReqArg (\s opts -> return opts{ optPass = s }) "PASSPHRASE")
        "Optional Passphrase for mnemonic"
    , Option ['p'] ["provider"]
        (ReqArg (\p opts -> return opts{ optProvider = p }) "URL")
        "URL of the API server"
    , Option ['h'] ["host"]
        (ReqArg (\h opts -> return opts{ optBind = h }) "HOST")
        "API server bind address"
    , Option ['P'] ["port"] 
        (ReqArg (\p opts -> return opts{ optPort = read p }) "PORT")
        "API server port"
    , Option ['W'] ["workdir"]
        (ReqArg (\w opts -> return opts{ optDir = Just w }) "DIR")
        "Working directory"
    , Option ['L'] ["logfile"]
        (ReqArg (\l opts -> return opts{ optLog = Just l }) "FILE")
        "Log file"
    , Option ['I'] ["pidfile"]
        (ReqArg (\i opts -> return opts{ optPid = Just i }) "FILE")
        "PID file"
    ]

parseCount :: String -> Options -> IO Options
parseCount s opts 
    | res > 0   = return opts{ optCount = res }
    | otherwise = error $ unwords ["Invalid count option:", s]
    where res = read s

parseMinConf :: String -> Options -> IO Options
parseMinConf m opts 
    | res >= 0   = return opts{ optMinConf = res }
    | otherwise = error $ unwords ["Invalid minconf option:", m]
    where res = read m

usageHeader :: String
usageHeader = "Usage: hw [<options>] <command> [<args>]"

cmdHelp :: [String]
cmdHelp = 
    [ "Server commands:" 
    , "  start [--detach]                    Start the haskoin daemon"
    , "  stop                                Stop the haskoin daemon"
    , ""
    , "Wallet commands:" 
    , "  newwallet [mnemonic] [-w name]      Create a new wallet"
    , "  getwallet [-w name]                 Display a wallet by name"
    , "  walletlist                          List all wallets"
    , "  rescan [timestamp]                  Rescan the wallet"
    , ""
    , "Account commands:" 
    , "  newacc    name                      Create a new account"
    , "  newms     name M N [pubkey...]      Create a new multisig account"
    , "  newread   name pubkey               Create a new read-only account"
    , "  newreadms name [pubkey...]          Create a new read-only ms account"
    , "  addkeys   acc  {pubkey...}          Add pubkeys to a multisig account"
    , "  acclist                             List all accounts"
    , "  getacc    acc                       Display an account by name"
    , ""
    , "Address commands:" 
    , "  new    acc labels                   Generate an address with a label"
    , "  list   acc                          Display all account addresses"
    , "  page   acc page [-c addr/page]      Display account addresses by page"
    , "  label  acc index label              Add a label to an address"
    , ""
    , "Transaction commands:" 
    , "  txlist    acc                       Display transactions in an account"
    , "  txpage    acc page [-c tx/page]     Display transactions by page"
    , "  send      acc addr amount [-m]      Send coins to an address"
    , "  sendmany  acc {addr:amount...} [-m] Send coins to many addresses"
    , "  signtx    acc tx                    Sign a transaction (sign + import)"
    , "  importtx  acc tx                    Import a transaction"
    , "  balance   acc [-m]                  Display account balance"
    , "  spendable acc [-m]                  Display account spendable balance"
    , "  getprop   acc hash                  Get a transaction proposition"
    , "  gettx     acc hash                  Get a raw transaction"
    , ""
    , "Offline tx commands:" 
    , "  getblob   acc txhash                Get data to sign a tx offline"
    , "  signblob  acc blob                  Sign an offline tx"
    , ""
    , "Utility commands: "
    , "  decodetx  tx                        Decode HEX transaction"
    , ""
    , "Other commands: "
    , "  version                             Display version information"
    , "  help                                Display this help information"
    ]

warningMsg :: String
warningMsg = unwords [ "***"
                     , "This software is experimental."
                     , "Use only small amounts of Bitcoins"
                     , "***"
                     ]

usage :: String
usage = unlines $ [warningMsg, usageInfo usageHeader options] ++ cmdHelp

main :: IO ()
main = E.getArgs >>= \args -> case getOpt Permute options args of
    (o,xs,[]) -> do
        opts0 <- foldl (>>=) (return defaultOptions) o
        opts  <- foldl (>>=) (getConfig opts0) o
        workDir <- maybe getWorkDir return $ optDir opts
        setCurrentDirectory workDir
        processCommand opts xs
    (_,_,msgs) -> print $ unlines $ msgs ++ [usage]

getConfig :: Options -> IO Options
getConfig opts = do
    configFile <- case optCfg opts of
        Nothing -> case (optDir opts) of
            Nothing -> do
                dir <- getWorkDir
                return $ concat [dir, "/config.yaml"]
            Just wd -> do
                return $ concat [wd, "/config.yaml"]
        Just cfg -> return cfg
    prevConfig <- doesFileExist configFile
    unless prevConfig $ YAML.encodeFile configFile defaultOptions
    configM <- YAML.decodeFile configFile
    unless (isJust configM) $ throwIO $ WalletException $ unwords
        [ "Could not parse config file:"
        , configFile
        ]
    return $ fromJust configM

invalidErr :: String
invalidErr = "Invalid request"

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

printJSONOr :: (FromJSON a, ToJSON a)
            => Options 
            -> BL.ByteString
            -> (a -> IO ()) 
            -> IO ()
printJSONOr opts bs action
    | isLeft resE = error $ fromLeft resE
    | optJson opts = do
        let bs' = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } res
        formatStr $ bsToString $ toStrictBS bs'
    | optYaml opts = formatStr $ bsToString $ YAML.encode res
    | otherwise = action res
  where
    resE = eitherDecode bs
    res  = fromRight resE

-- TODO: Rename existing log files to prevent overwriting them
processCommand :: Options -> Args -> IO ()
processCommand opts args = case args of
    ["start"] -> do
        let config = ServerConfig
                { configBind         = optBind opts
                , configPort         = optPort opts
                , configBitcoinHosts = optHosts opts
                , configBatch        = optBatch opts
                , configBloomFP      = optBloomFP opts
                , configMode         = optMode opts
                , configGap          = optGap opts
                , configUser         = optUser opts
                , configPassword     = optPassword opts
                }
        prevLog <- doesFileExist $ logFile
        -- TODO: Should we move the log file to an archive directory?
        when prevLog $ removeFile $ logFile
        if optDetach opts
            then runDetached (Just $ pidFile) (ToFile $ logFile) $
                    runServer config
            else runServer config
        putStrLn "Haskoin daemon started"
        putStrLn $ unwords [ "Configuration file:", configFile ]
    ["stop"] -> do
        -- TODO: Should we send a message instead of killing the process ?
        killAndWait $ pidFile
        putStrLn "Haskoin daemon stopped"
    "newwallet" : mnemonic -> do
        let url = "/wallets"
            req = Just $ encode $ case mnemonic of
                []  -> NewWallet (optWallet opts) (optPass opts) Nothing
                [m] -> NewWallet (optWallet opts) (optPass opts) (Just m)
                _   -> error invalidErr
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(MnemonicRes m) -> do
            putStrLn "Write down your seed:"
            putStrLn m
    ["getwallet"] -> do
        let url = concat [ "/wallets/", optWallet opts ]
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ putStr . printWallet
    ["walletlist"] -> do
        let url = "/wallets"
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ \ws -> do
            let xs = map (putStr . printWallet) ws
            sequence_ $ intersperse (putStrLn "-") xs
    ["newacc", name] -> do
        let url = concat [ "/wallets/", optWallet opts, "/accounts" ]
            req = Just $ encode $ NewAccount name
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStr . printAccount
    "newms" : name : m : n : ks -> do
        let url   = concat [ "/wallets/", optWallet opts, "/accounts" ] 
            keysM = mapM xPubImport ks
            m'    = read m
            n'    = read n
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let req = Just $ encode $ NewMSAccount name m' n' $ fromJust keysM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStr . printAccount
    ["newread", name, key] -> do
        let keyM = xPubImport key
        when (isNothing keyM) $ throwIO $ 
            WalletException "Could not parse key"
        let url   = concat [ "/wallets/", optWallet opts, "/accounts" ] 
            req = Just $ encode $ NewReadAccount name $ fromJust keyM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStr . printAccount
    "newreadms" : name : m : n : ks -> do
        let keysM = mapM xPubImport ks
            m'    = read m
            n'    = read n
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let url   = concat [ "/wallets/", optWallet opts, "/accounts" ] 
            req = Just $ encode $ NewReadMSAccount name m' n' $ fromJust keysM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStr . printAccount
    "addkeys" : name : ks -> do
        let keysM = mapM xPubImport ks
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/keys" 
                         ] 
            req = encode <$> keysM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStr . printAccount
    ["getacc", name] -> do
        let url = concat [ "/wallets/", optWallet opts , "/accounts/", name ] 
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ putStr . printAccount
    ["acclist"] -> do
        let url = concat [ "/wallets/", optWallet opts , "/accounts" ] 
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ \as -> do
            let xs = map (putStr . printAccount) as
            sequence_ $ intersperse (putStrLn "-") xs
    ["list", name] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/addrs" 
                         ] 
            qs  = [ ("minconf", Just $ stringToBS $ show $ optMinConf opts)
                  , ("internal", Just $ stringToBS $ map toLower $ show $ 
                        optInternal opts)
                  ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ mapM_ (putStrLn . printBalanceAddress)
    ["page", name, page] -> do
        let p   = read page :: Int
            url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/addrs" 
                         ] 
            qs  = [ ("page",Just $ stringToBS $ show p)
                  , ("elemperpage",Just $ stringToBS $ show $ optCount opts)
                  , ("minconf", Just $ stringToBS $ show $ optMinConf opts)
                  , ("internal", Just $ stringToBS $ map toLower $ show $ 
                        optInternal opts)
                  ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ \(AddressPageRes as m) -> do
            -- page 0 is the last page
            let x = if p == 0 then m else p
            putStrLn $ unwords [ "Page", show x, "of", show m ]
            forM_ as $ putStrLn . printBalanceAddress
    ["new", name, label] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/addrs" 
                         ] 
            req = Just $ encode $ AddressData label
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ putStrLn . printPaymentAddress
    ["label", name, index, label] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name
                         , "/addrs/", index 
                         ] 
            req = Just $ encode $ AddressData label
        res <- sendRequest url "PUT" [] req opts
        printJSONOr opts res $ putStrLn . printPaymentAddress
    ["txlist", name] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ \ts -> do
            let xs = map (putStr . printAccTx) ts
            sequence_ $ intersperse (putStrLn "-") xs
    ["txpage", name, page] -> do
        let p   = read page
            url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
            qs  = [ ("page",Just $ stringToBS $ show p)
                  , ("elemperpage",Just $ stringToBS $ show $ optCount opts)
                  ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ \(TxPageRes ts m) -> do
            -- page 0 is the last page
            let x = if p == 0 then m else p
            putStrLn $ unwords [ "Page", show x, "of", show m ]
            let xs = map (putStr . printAccTx) ts
            sequence_ $ intersperse (putStrLn "-") xs
    ["send", name, add, amount] -> do
        let a = base58ToAddr add
            v = read amount
        when (isNothing a) $ throwIO $ 
            WalletException "Could not parse address"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
            req = Just $ encode $ 
                SendCoins [(fromJust a, v)] (optFee opts) (optMinConf opts) 
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(TxHashStatusRes h c) -> do
            putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
    "sendmany" : name : xs -> do
        let g str   = map T.unpack $ T.splitOn ":" (T.pack str)
            f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read v)
            f _     = throw $ WalletException "Could not parse recipient list"
            recipients = mapM (f . g) xs
        when (isNothing recipients) $ throwIO $
            WalletException "Could not parse recipient list"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
            req = Just $ encode $ 
                SendCoins (fromJust recipients) (optFee opts) (optMinConf opts)
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(TxHashStatusRes h c) -> do
            putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
    ["signtx", name, tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
            req = Just $ encode $ SignTx $ fromJust txM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(TxHashStatusRes h c) -> do
            putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
    ["importtx", name, tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ]
            req = Just $ encode $ ImportTx $ fromJust txM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(TxHashStatusRes h c) -> do
            putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
    ["getblob", name, tid] -> do
        let h = decodeTxHashLE tid
        when (isNothing h) $ throwIO $
            WalletException "Could not parse hash"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name
                         , "/txs/", encodeTxHashLE $ fromJust h
                         , "/sigblob"
                         ] 
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ \blob ->
            putStrLn $ bsToHex $ toStrictBS $ encode (blob :: SigBlob)
    ["signblob", name, blob] -> do
        let blobM = decode . toLazyBS =<< hexToBS blob :: Maybe SigBlob
        when (isNothing blobM) $ throwIO $
            WalletException "Could not parse sig blob"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/txs"
                         ] 
            req = Just $ encode $ SignSigBlob $ fromJust blobM
        res <- sendRequest url "POST" [] req opts
        printJSONOr opts res $ \(TxStatusRes tx c) -> do
            putStrLn $ unwords [ "Tx      :", bsToHex $ encode' tx ]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No" ]
    ["balance", name] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/balance"
                         ] 
            qs  = [ ("minconf", Just $ stringToBS $ show $ optMinConf opts) ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ \(BalanceRes b cs) -> do
            putStrLn $ unwords [ "Balance:", printBalance b ]
            unless (null cs) $ do
                putStrLn "Conflicts:"
                formatStr $ unlines $ map encodeTxHashLE cs 
    ["spendable", name] -> do
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name, "/spendablebalance"
                         ] 
            qs  = [ ("minconf", Just $ stringToBS $ show $ optMinConf opts) ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ \(SpendableRes b) -> do
            putStrLn $ unwords [ "Spendable balance:", show b ]
    ["getprop", name, hash] -> do
        let h = decodeTxHashLE hash
        when (isNothing h) $ throwIO $
            WalletException "Could not parse hash"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name
                         , "/txs/", encodeTxHashLE $ fromJust h
                         ]
            qs  = [ ("proposition", Just $ stringToBS "true") ]
        res <- sendRequest url "GET" qs Nothing opts
        printJSONOr opts res $ \(AccTx _ _ _ _ _ _ tx _ _) -> 
            putStrLn $ bsToHex $ encode' tx
    ["gettx", name, hash] -> do
        let h = decodeTxHashLE hash
        when (isNothing h) $ throwIO $
            WalletException "Could not parse hash"
        let url = concat [ "/wallets/", optWallet opts
                         , "/accounts/", name
                         , "/txs/", encodeTxHashLE $ fromJust h
                         ]
        res <- sendRequest url "GET" [] Nothing opts
        printJSONOr opts res $ \(AccTx _ _ _ _ _ _ tx _ _) -> 
            putStrLn $ bsToHex $ encode' tx
    "rescan" : rescantime -> do
        let t = read <$> listToMaybe rescantime
            req = Just $ encode $ Rescan t
        res <- sendRequest "/node" "POST" [] req opts
        printJSONOr opts res $ \(RescanRes ts) ->
            putStrLn $ unwords [ "Timestamp:", show ts]
    ["decodetx", tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        let res = encodeTxJSON $ fromJust txM
            bs = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } res
        if optJson opts
            then formatStr $ bsToString $ toStrictBS bs
            else formatStr $ bsToString $ YAML.encode res
    [] -> formatStr usage
    ["help"] -> formatStr usage
    ["version"] -> putStrLn haskoinUserAgent
    _ -> error invalidErr
  where
    pidFile     = fromMaybe "hw.pid"      $ optPid opts
    logFile     = fromMaybe "stdout.log"  $ optLog opts
    configFile  = fromMaybe "config.yaml" $ optCfg opts

data ErrorMsg = ErrorMsg !String ![String]
    deriving (Eq, Show, Read)

instance FromJSON ErrorMsg where
    parseJSON = withObject "errormsg" $ \o -> do
        err  <- o .: "message"
        msgM <- o .:? "errors"
        return $ ErrorMsg err $ fromMaybe [] msgM

sendRequest :: String              -- Path
            -> BS.ByteString       -- Method
            -> [(BS.ByteString, Maybe BS.ByteString)] -- Query String
            -> Maybe BL.ByteString -- Body 
            -> Options             -- Options
            -> IO BL.ByteString    -- Response
sendRequest p m qs bodyM opts = withManager $ \manager -> do
    let url = concat [ optProvider opts, p ]
    urlReq <- parseUrl url
    let req' = setQueryString qs $ urlReq
                   { method         = m
                   -- Do not throw exceptions on error status codes
                   , checkStatus    = (\_ _ _ -> Nothing) 
                   , requestHeaders = [("accept", "application/json")]
                   }
        req = applyBasicAuth (encodeUtf8 $ optUser opts)
                             (encodeUtf8 $ optPassword opts)
                             req'
    res <- flip httpLbs manager $ case bodyM of
        Just b -> req{ requestBody = RequestBodyLBS b }
        _      -> req
    let b    = responseBody res
        errM = decode b
    when (statusCode (responseStatus res) >= 400 && isJust errM) $ do
        let (ErrorMsg err msgs) = fromJust errM
        if null msgs then error err else
            error $ unwords $ (err ++ ":") : msgs
    return b

encodeTxJSON :: Tx -> Value
encodeTxJSON tx@(Tx v is os i) = object
    [ "txid"     .= encodeTxHashLE (txHash tx)
    , "version"  .= v
    , "inputs"   .= map input (zip is [0..])
    , "outputs"  .= map output (zip os [0..])
    , "locktime" .= i
    ]
  where 
    input (x,j) = object 
      [T.pack ("input " ++ show (j :: Int)) .= encodeTxInJSON x]
    output (x,j) = object 
      [T.pack ("output " ++ show (j :: Int)) .= encodeTxOutJSON x]

encodeTxInJSON :: TxIn -> Value
encodeTxInJSON (TxIn o s i) = object $ concat 
    [ [ "outpoint"   .= encodeOutPointJSON o
      , "sequence"   .= i
      , "raw-script" .= bsToHex s
      , "script"     .= encodeScriptJSON sp
      ] 
      , decoded
    ]
  where 
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const []) f $ decodeInputBS s
    f inp = ["decoded-script" .= encodeScriptInputJSON inp]

encodeTxOutJSON :: TxOut -> Value
encodeTxOutJSON (TxOut v s) = object $
    [ "value"      .= v
    , "raw-script" .= bsToHex s
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded 
  where 
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const [])
                 (\out -> ["decoded-script" .= encodeScriptOutputJSON out]) 
                 (decodeOutputBS s)

encodeOutPointJSON :: OutPoint -> Value
encodeOutPointJSON (OutPoint h i) = object
    [ "txid" .= encodeTxHashLE h
    , "pos"  .= toJSON i
    ]

encodeScriptJSON :: Script -> Value
encodeScriptJSON (Script ops) = 
    toJSON $ map f ops
  where
    f (OP_PUSHDATA bs _) = String $ T.pack $ unwords 
        ["OP_PUSHDATA", bsToHex bs]
    f x = String $ T.pack $ show x

encodeScriptInputJSON :: ScriptInput -> Value
encodeScriptInputJSON si = case si of
    RegularInput (SpendPK s) -> object 
        [ "spendpubkey" .= object [ "sig" .= encodeSigJSON s ] ]
    RegularInput (SpendPKHash s p) -> object 
        [ "spendpubkeyhash" .= object
            [ "sig"            .= encodeSigJSON s
            , "pubkey"         .= bsToHex (encode' p)
            , "sender-address" .= addrToBase58 (pubKeyAddr p)
            ]
        ]
    RegularInput (SpendMulSig sigs) -> object 
        [ "spendmulsig" .= object [ "sigs" .= map encodeSigJSON sigs ] ]
    ScriptHashInput s r -> object
        [ "spendscripthash" .= object
            [ "scriptinput" .= encodeScriptInputJSON (RegularInput s)
            , "redeem" .= encodeScriptOutputJSON r
            , "raw-redeem" .= bsToHex (encodeOutputBS r)
            , "sender-address" .= addrToBase58 (scriptAddr r)
            ]
        ]

encodeScriptOutputJSON :: ScriptOutput -> Value
encodeScriptOutputJSON so = case so of
    PayPK p -> object
        [ "pay2pubkey" .= object [ "pubkey" .= bsToHex (encode' p) ] ]
    PayPKHash a -> object 
        [ "pay2pubkeyhash" .= object
            [ "address-base64" .= bsToHex (encode' $ getAddrHash a)
            , "address-base58" .= addrToBase58 a
            ]
        ]
    PayMulSig ks r -> object 
        [ "pay2mulsig" .= object
            [ "required-keys" .= r
            , "pubkeys"       .= map (bsToHex . encode') ks
            ]
        ]
    PayScriptHash a -> object 
        [ "pay2scripthash" .= object
            [ "address-base64" .= bsToHex (encode' $ getAddrHash a)
            , "address-base58" .= addrToBase58 a
            ]
        ]

encodeSigJSON :: TxSignature -> Value
encodeSigJSON ts@(TxSignature _ sh) = object
    [ "raw-sig" .= bsToHex (encodeSig ts)
    , "sighash" .= encodeSigHashJSON sh
    ]

encodeSigHashJSON :: SigHash -> Value
encodeSigHashJSON sh = case sh of
    SigAll acp -> object
        [ "type" .= String "SigAll"
        , "acp"  .= acp
        ]
    SigNone acp -> object
        [ "type" .= String "SigNone"
        , "acp"  .= acp
        ]
    SigSingle acp -> object
        [ "type" .= String "SigSingle"
        , "acp"  .= acp
        ]
    SigUnknown acp v -> object
        [ "type"  .= String "SigUnknown"
        , "acp"   .= acp
        , "value" .= v
        ]

{- Utility Commands -}

-- | Build a raw transaction from a list of outpoints and recipients encoded
-- in JSON.
--
-- Outpoint format as JSON:
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       },...
-- >   ] 
--
--  Recipient list as JSON:
--
-- >   { addr: amnt,... }
--
buildRawTx :: MonadIO m 
           => String  -- ^ List of JSON encoded Outpoints.
           -> String  -- ^ List of JSON encoded Recipients.
           -> m Tx    -- ^ Transaction result.
buildRawTx i o 
    | isJust opsM && isJust destsM = do
        when (isLeft txE) $ liftIO $ throwIO $
            WalletException $ fromLeft txE
        return tx
    | otherwise = liftIO $ throwIO $
        WalletException "Could not parse input values"
  where
    opsM   = decode $ toLazyBS $ stringToBS i
    destsM = decode $ toLazyBS $ stringToBS o
    (RawTxOutPoints ops) = fromJust opsM
    (RawTxDests dests)   = fromJust destsM
    txE = buildAddrTx ops dests
    tx  = fromRight txE

-- | Sign a raw transaction by providing the signing parameters and private
-- keys manually. None of the keys in the wallet will be used for signing.
--
-- Signing data as JSON (redeemScript is optional):
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       , "scriptPubKey": hex
-- >       , "redeemScript": hex
-- >       },...
-- >    ]
--
-- Private keys in JSON foramt:
--
-- >   [ WIF,... ]
signRawTx :: MonadIO m
          => Tx       -- ^ Transaction to sign.
          -> String   -- ^ List of JSON encoded signing parameters.
          -> String   -- ^ List of JSON encoded WIF private keys.
          -> SigHash  -- ^ Signature type. 
          -> m (Tx, Bool)
signRawTx tx strSigi strKeys sh 
    | isJust fsM && isJust keysM = do
        let resE = detSignTx tx (map (\f -> f sh) fs) keys
        when (isLeft resE) $ liftIO $ throwIO $
            WalletException $ fromLeft resE
        return $ fromRight resE
    | otherwise = liftIO $ throwIO $
        WalletException "Could not parse input values"
  where
    fsM   = decode $ toLazyBS $ stringToBS strSigi
    keysM = decode $ toLazyBS $ stringToBS strKeys
    (RawSigInput fs) = fromJust fsM
    (RawPrvKey keys) = fromJust keysM

data RawTxOutPoints = RawTxOutPoints [OutPoint] 
    deriving (Eq, Show)

instance FromJSON RawTxOutPoints where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawTxOutPoints <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            let i = maybeToEither ("Failed to decode txid" :: String)
                                  (decodeTxHashLE tid)
                o = OutPoint <$> i <*> (return vout)
            either (const mzero) return o

data RawTxDests = RawTxDests [(String,Word64)]
    deriving (Eq, Show)

instance FromJSON RawTxDests where
    parseJSON = withObject "Expected: Object" $ \obj ->
        RawTxDests <$> (mapM f $ H.toList obj)
      where
        f (add,v) = do
            amnt <- parseJSON v :: Parser Word64
            return (T.unpack add, amnt)

data RawSigInput = RawSigInput [(SigHash -> SigInput)]

instance FromJSON RawSigInput where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawSigInput <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            scp  <- obj .: "scriptPubKey" :: Parser String
            rdm  <- obj .:? "redeemScript" :: Parser (Maybe String)
            let s = decodeOutputBS =<< maybeToEither "Hex parsing failed" 
                        (hexToBS scp)
                i = maybeToEither "Failed to decode txid" (decodeTxHashLE tid)
                o = OutPoint <$> i <*> (return vout)
                r = decodeOutputBS =<< maybeToEither "Hex parsing failed" 
                        (hexToBS $ fromJust rdm)
                res | isJust rdm = 
                        (flip <$> (SigInput <$> s <*> o)) <*> (Just <$> r)
                    | otherwise  = 
                        (flip <$> (SigInput <$> s <*> o)) <*> (return Nothing)
            either (const mzero) return res

data RawPrvKey = RawPrvKey [PrvKey]
    deriving (Eq, Show)

instance FromJSON RawPrvKey where
    parseJSON = withArray "Expected: Array" $ \arr ->
        RawPrvKey <$> (mapM f $ V.toList arr)
      where
        f v = do
            str <- parseJSON v :: Parser String  
            maybe mzero return $ fromWIF str

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    haskoinDir <- getAppUserDataDirectory "haskoin"
    let dir = concat [ haskoinDir, "/", networkName ]
        html = concat [ haskoinDir, "/", networkName, "/html" ]
    createDirectoryIfMissing True dir
    createDirectoryIfMissing True html
    return dir

