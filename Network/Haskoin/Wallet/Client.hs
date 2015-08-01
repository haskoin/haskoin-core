module Network.Haskoin.Wallet.Client (clientMain) where

import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files 
    ( setFileMode
    , setFileCreationMask
    , unionFileModes
    , ownerModes
    , groupModes
    , otherModes
    , fileExist
    )
import System.Environment (getArgs, getEnv, lookupEnv)
import System.Info (os)
import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    , ArgOrder (Permute)
    )

import Control.Applicative ((<$>))
import Control.Monad (when, forM_, filterM)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.Reader as R (runReaderT)

import Data.Default (def)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T (pack, unpack)
import Data.Yaml (decodeFileEither)

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Client.Commands
import Network.Haskoin.Wallet.Types

import System.FilePath.Posix (isAbsolute)

usageHeader :: String
usageHeader = "Usage: hw [<options>] <command> [<args>]"

cmdHelp :: [String]
cmdHelp = lines $ bsToString $ $(embedFile "config/help")

warningMsg :: String
warningMsg = unwords 
    [ "!!!", "This software is experimental."
    , "Use only small amounts of Bitcoins.", "!!!"
    ]

usage :: [String]
usage = warningMsg : usageInfo usageHeader options : cmdHelp

options :: [OptDescr (Config -> Config)]
options =
    [ Option "k" ["keyring"]
        (ReqArg (\s cfg -> cfg { configKeyRing = T.pack s }) "KEYRING") $
        "Which keyring to use (default: "
            ++ T.unpack (configKeyRing def) ++ ")"
    , Option "c" ["count"] 
        (ReqArg (\s cfg -> cfg { configCount = read s }) "INT") $
        "Set the output size of some commands (default: "
            ++ show (configCount def) ++ ")"
    , Option "m" ["minconf"] 
        (ReqArg (\s cfg -> cfg { configMinConf = read s }) "INT") $
        "Required minimum confirmations for balances (default: "
            ++ show (configMinConf def) ++ ")"
    , Option "f" ["fee"] 
        (ReqArg (\s cfg -> cfg { configFee = read s }) "INT") $
        "Fee per 1000 bytes for new transactions (default: "
            ++ show (configFee def) ++ ")"
    , Option "R" ["rcptfee"]
        (NoArg $ \cfg -> cfg { configRcptFee = True }) $
        "Recipient pays fee (first if multiple) (default: "
            ++ show (configRcptFee def) ++ ")"
    , Option "S" ["nosig"]
        (NoArg $ \cfg -> cfg { configSignTx = False }) $
        "Do not sign transactions (default: "
            ++ show (not $ configSignTx def) ++ ")"
    , Option "i" ["internal"]
        (NoArg $ \cfg -> cfg { configAddrType = AddressInternal }) $
        "Display internal addresses (default: "
            ++ show (configAddrType def == AddressInternal) ++ ")"
    , Option "o" ["offline"]
        (NoArg $ \cfg -> cfg { configOffline = True }) $
        "Display offline balance (default: "
            ++ show (configOffline def) ++ ")"
    , Option "r" ["revpage"]
        (NoArg $ \cfg -> cfg { configReversePaging = True }) $
        "Use reverse paging (default: "
            ++ show (configReversePaging def) ++ ")"
    , Option "p" ["passphrase"]
        (ReqArg (\s cfg -> cfg { configPass = Just $ T.pack s }) "PASSPHRASE")
        "Optional mnemonic passphrase when creating wallets"
    , Option "j" ["json"]
        (NoArg $ \cfg -> cfg { configFormat = OutputJSON })
        "Format result as JSON"
    , Option "y" ["yaml"]
        (NoArg $ \cfg -> cfg { configFormat = OutputYAML })
        "Format result as YAML"
    , Option "s" ["socket"]
        (ReqArg (\s cfg -> cfg { configConnect = s }) "URI") $
        "ZeroMQ socket of the server (default: "
            ++ configConnect def ++ ")"
    , Option "d" ["detach"]
        (NoArg $ \cfg -> cfg { configDetach = True }) $
        "Detach the server process (default: "
            ++ show (configDetach def) ++ ")"
    , Option "t" ["testnet"]
        (NoArg $ \cfg -> cfg { configTestnet = True }) "Use Testnet3 network"
    , Option "g" ["config"]
        (ReqArg (\s cfg -> cfg { configFile = s }) "FILE") $
        "Configuration file (default: "
            ++ configFile def ++ ")"
    , Option "w" ["workdir"]
        (ReqArg (\s cfg -> cfg { configDir = s }) "DIR")
        "Working directory (OS-specific default)"
    ]

-- Create and change current working directory
setWorkDir :: Config -> IO ()
setWorkDir cfg = do
    let workDir = configDir cfg </> networkName
    _ <- setFileCreationMask $ otherModes `unionFileModes` groupModes
    createDirectoryIfMissing True workDir
    setFileMode workDir ownerModes
    changeWorkingDirectory workDir

-- Build application configuration
getConfig :: [Config -> Config] -> IO Config
getConfig fs = do
    -- Create initial configuration from defaults and command-line arguments
    let initCfg = foldr ($) def fs

    -- If working directory set in initial configuration, use it
    dir <- case configDir initCfg of "" -> appDir
                                     d  -> return d

    -- Make configuration file relative to working directory
    let cfgFile = if isAbsolute (configFile initCfg)
                     then configFile initCfg
                     else dir </> configFile initCfg
    
    -- Get configuration from file, if it exists
    e <- fileExist cfgFile
    if e then do
            cfgE <- decodeFileEither cfgFile
            case cfgE of
                Left e -> error $ show e
                -- Override settings from file using command-line
                Right cfg -> return $ fixConfigDir (foldr ($) cfg fs) dir
         else return $ fixConfigDir initCfg dir
  where
    -- If working directory not set, use default
    fixConfigDir cfg dir = case configDir cfg of "" -> cfg{ configDir = dir }
                                                 _  -> cfg


clientMain :: IO ()
clientMain = getArgs >>= \args -> case getOpt Permute options args of
    (fs, commands, []) -> do
        cfg <- getConfig fs
        when (configTestnet cfg) switchToTestnet3
        setWorkDir cfg
        dispatchCommand cfg commands
    (_, _, msgs) -> forM_ (msgs ++ usage) putStrLn

dispatchCommand :: Config -> [String] -> IO ()
dispatchCommand cfg args = flip R.runReaderT cfg $ case args of
    ["start"]                              -> cmdStart
    ["stop"]                               -> cmdStop
    "newkeyring"  : mnemonic               -> cmdNewKeyRing mnemonic
    ["keyring"]                            -> cmdKeyRing
    ["keyrings"]                           -> cmdKeyRings
    "newacc"      : [name]                 -> cmdNewAcc name
    "newms"       : name : m : n : ks      -> cmdNewMS False name m n ks
    "newread"     : [name, key]            -> cmdNewRead name key
    "newreadms"   : name : m : n : ks      -> cmdNewMS True name m n ks
    "addkeys"     : name : ks              -> cmdAddKeys name ks
    "setgap"      : [name, gap]            -> cmdSetGap name gap
    "account"     : [name]                 -> cmdAccount name
    ["accounts"]                           -> cmdAccounts
    "list"        : name : page            -> cmdList name page
    "unused"      : [name]                 -> cmdUnused name
    "label"       : [name, index, label]   -> cmdLabel name index label
    "txs"         : name : page            -> cmdTxs name page
    "addrtxs"     : name : index : page    -> cmdAddrTxs name index page
    "send"        : [name, add, amnt]      -> cmdSend name add amnt
    "sendmany"    : name : xs              -> cmdSendMany name xs
    "import"      : [name, tx]             -> cmdImport name tx
    "sign"        : [name, txid]           -> cmdSign name txid
    "gettx"       : [name, txid]           -> cmdGetTx name txid
    "balance"     : [name]                 -> cmdBalance name
    "getoffline"  : [name, txid]           -> cmdGetOffline name txid
    "signoffline" : [name, tx, dat]        -> cmdSignOffline name tx dat
    "rescan"      : rescantime             -> cmdRescan rescantime
    "decodetx"    : [tx]                   -> cmdDecodeTx tx
    ["status"]                             -> cmdStatus
    ["version"]                            -> cmdVersion
    ["help"]                               -> liftIO $ forM_ usage putStrLn
    []                                     -> liftIO $ forM_ usage putStrLn
    _ -> liftIO $ forM_ ("Invalid command" : usage) putStrLn


appDir :: IO FilePath
appDir = case os of "mingw"   -> windows
                    "mingw32" -> windows
                    "mingw64" -> windows
                    "darwin"  -> osx
                    "linux"   -> unix
                    _         -> unix
  where
    windows = do
        localAppData <- lookupEnv "LOCALAPPDATA"
        dirM <- case localAppData of
            Nothing -> lookupEnv "APPDATA"
            Just l -> return $ Just l
        case dirM of
            Just d -> return $ d </> "Haskoin Wallet"
            Nothing -> return "."
    osx = do
        homeM <- lookupEnv "HOME"
        case homeM of
            Just home -> return $ home </> "Library"
                                       </> "Application Support"
                                       </> "Haskoin Wallet"
            Nothing -> return "."
    unix = do
        homeM <- lookupEnv "HOME"
        case homeM of
            Just home -> return $ home </> ".hw"
            Nothing -> return "."
        
