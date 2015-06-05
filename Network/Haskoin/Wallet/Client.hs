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
import System.Posix.Env (getEnv)
import qualified System.Environment as E (getArgs)
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

import Data.FileEmbed (embedFile)
import qualified Data.Text as T (pack, unpack)

import Yesod.Default.Config2 (loadAppSettings, useEnv)

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
    [ Option ['k'] ["keyring"]
        (ReqArg (\s cfg -> cfg { configKeyRing = T.pack s }) "KEYRING") $
        "Which keyring to use (default: "
            ++ T.unpack (configKeyRing hardConfig) ++ ")"
    , Option ['c'] ["count"] 
        (ReqArg (\s cfg -> cfg { configCount = read s }) "INT") $
        "Set the output size of some commands (default: "
            ++ show (configCount hardConfig) ++ ")"
    , Option ['m'] ["minconf"] 
        (ReqArg (\s cfg -> cfg { configMinConf = read s }) "INT") $
        "Required minimum confirmations for balances (default: "
            ++ show (configMinConf hardConfig) ++ ")"
    , Option ['f'] ["fee"] 
        (ReqArg (\s cfg -> cfg { configFee = read s }) "INT") $
        "Fee per 1000 bytes for new transactions (default: "
            ++ show (configFee hardConfig) ++ ")"
    , Option ['S'] ["nosig"]
        (NoArg $ \cfg -> cfg { configSignTx = False }) $
        "Do not sign transactions (default: "
            ++ show (not $ configSignTx hardConfig) ++ ")"
    , Option ['i'] ["internal"]
        (NoArg $ \cfg -> cfg { configAddrType = AddressInternal }) $
        "Display internal addresses (default: "
            ++ show (configAddrType hardConfig == AddressInternal) ++ ")"
    , Option ['r'] ["revpage"]
        (NoArg $ \cfg -> cfg { configReversePaging = True }) $
        "Use reverse paging (default: "
            ++ show (configReversePaging hardConfig) ++ ")"
    , Option ['p'] ["passphrase"]
        (ReqArg (\s cfg -> cfg { configPass = Just $ T.pack s }) "PASSPHRASE")
        "Optional mnemonic passphrase when creating wallets"
    , Option ['j'] ["json"]
        (NoArg $ \cfg -> cfg { configFormat = OutputJSON })
        "Format result as JSON"
    , Option ['y'] ["yaml"]
        (NoArg $ \cfg -> cfg { configFormat = OutputYAML })
        "Format result as YAML"
    , Option ['s'] ["socket"]
        (ReqArg (\s cfg -> cfg { configConnect = s }) "URI") $
        "ZeroMQ socket of the server (default: "
            ++ configConnect hardConfig ++ ")"
    , Option ['d'] ["detach"]
        (NoArg $ \cfg -> cfg { configDetach = True }) $
        "Detach the server process (default: "
            ++ show (configDetach hardConfig) ++ ")"
    , Option ['t'] ["testnet"]
        (NoArg $ \cfg -> cfg { configTestnet = True }) $
        "Use Testnet3 network"
    , Option ['g'] ["config"]
        (ReqArg (\s cfg -> cfg { configFile = s }) "FILE") $
        "Configuration file (default: "
            ++ configFile hardConfig ++ ")"
    , Option ['w'] ["workdir"]
        (ReqArg (\s cfg -> cfg { configDir = s }) "DIR") $
        "Working directory (default: "
            ++ configDir hardConfig ++ ")"
    ]

-- Create and change current working directory
setWorkDir :: Config -> IO ()
setWorkDir cfg = do
    let workDir = configDir cfg </> networkName
    _ <- setFileCreationMask $ otherModes `unionFileModes` groupModes
    createDirectoryIfMissing True workDir
    setFileMode workDir ownerModes
    changeWorkingDirectory workDir

getConfig :: [(Config -> Config)] -> IO Config
getConfig fs = do
    homeM <- getEnv "HOME"
    cfg1 <- flip (foldr ($)) fs
            <$> loadAppSettings [] [configValue] useEnv
    cfgFiles <- filterM fileExist $ confFiles cfg1 homeM
    cfg <- flip (foldr ($)) fs
            <$> loadAppSettings cfgFiles [configValue] useEnv
    return cfg { configDir = workDir cfg homeM }
  where
    confFiles conf Nothing =
        [ configFile conf, configDir conf </> configFile conf ]
    confFiles conf homeM@(Just _)
        | isAbsolute (configFile conf) = [ configFile conf ]
        | otherwise = [ workDir conf homeM </> configFile conf ]
    workDir conf (Just home)
        | isAbsolute (configDir conf) = configDir conf
        | otherwise = home </> configDir conf
    workDir conf Nothing = configDir conf

clientMain :: IO ()
clientMain = E.getArgs >>= \args -> case getOpt Permute options args of
    (fs, commands, []) -> do
        cfg <- getConfig fs
        when (configTestnet cfg) switchToTestnet3
        setWorkDir cfg
        dispatchCommand cfg commands
    (_, _, msgs) -> forM_ (msgs ++ usage) putStrLn

dispatchCommand :: Config -> [String] -> IO ()
dispatchCommand cfg args = flip R.runReaderT cfg $ case args of
    "start"       : []                     -> cmdStart
    "stop"        : []                     -> cmdStop
    "newkeyring"  : mnemonic               -> cmdNewKeyRing mnemonic
    "keyring"     : []                     -> cmdKeyRing
    "keyrings"    : []                     -> cmdKeyRings
    "newacc"      : [name]                 -> cmdNewAcc name
    "newms"       : name : m : n : ks      -> cmdNewMS name m n ks
    "newread"     : [name, key]            -> cmdNewRead name key
    "newreadms"   : name : m : n : ks      -> cmdNewReadMS name m n ks
    "addkeys"     : name : ks              -> cmdAddKeys name ks
    "setgap"      : [name, gap]            -> cmdSetGap name gap
    "account"     : [name]                 -> cmdAccount name
    "accounts"    : []                     -> cmdAccounts
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
    "getoffline"  : [name, txid]           -> cmdGetOffline name txid
    "signoffline" : [name, tx, dat]        -> cmdSignOffline name tx dat
    "balance"     : [name]                 -> cmdBalance name
    "offbal"      : [name]                 -> cmdOfflineBalance name
    "rescan"      : rescantime             -> cmdRescan rescantime
    "decodetx"    : [tx]                   -> cmdDecodeTx tx
    "help"        : []                     -> liftIO $ forM_ usage putStrLn
    "version"     : []                     -> liftIO $ putStrLn haskoinUserAgent
    []                                     -> liftIO $ forM_ usage putStrLn
    _ -> liftIO $ forM_ ("Invalid command" : usage) $ putStrLn

