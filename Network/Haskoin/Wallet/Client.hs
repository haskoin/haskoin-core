module Network.Haskoin.Wallet.Client (clientMain) where

import System.Posix.Files (fileExist)
import System.Posix.Env (getEnv)
import System.Posix.Directory (changeWorkingDirectory)
import qualified System.Environment as E (getArgs)
import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    , ArgOrder (Permute)
    )

import Control.Monad (when, forM_, filterM)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as S (evalStateT)

import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack, unpack)

import Yesod.Default.Config2 (loadAppSettings, useEnv)

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Client.Commands

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

options :: [OptDescr (ClientConfig -> ClientConfig)]
options =
    [ Option ['w'] ["wallet"]
        (ReqArg (\s cfg -> cfg{ clientWallet = T.pack s }) "WALLET") $
        unwords [ "Which wallet to use ( default:"
                , T.unpack $ clientWallet compileTimeClientConfig
                ,")"
                ]
    , Option ['c'] ["count"] 
        (ReqArg (\s cfg -> cfg{ clientCount = read s }) "INT") $
        unwords [ "Set the output size of some commands ( default:"
                , show $ clientCount compileTimeClientConfig
                , ")"
                ]
    , Option ['m'] ["minconf"] 
        (ReqArg (\s cfg -> cfg{ clientMinConf = read s }) "INT") $
        unwords [ "Required minimum confirmations for balances ( default:"
                , show $ clientMinConf compileTimeClientConfig
                , ")"
                ]
    , Option ['i'] ["internal"]
        (NoArg $ \cfg -> cfg{ clientInternal = True }) $
        unwords [ "Display internal addresses ( default:"
                , show $ clientInternal compileTimeClientConfig
                , ")"
                ]
    , Option ['p'] ["passphrase"]
        (ReqArg (\s cfg -> cfg{ clientPass = T.pack s }) "PASSPHRASE")
        "Optional mnemonic passphrase when creating wallets"
    , Option ['j'] ["json"]
        (NoArg $ \cfg -> cfg{ clientFormat = OutputJSON })
        "Format result as JSON"
    , Option ['y'] ["yaml"]
        (NoArg $ \cfg -> cfg{ clientFormat = OutputYAML })
        "Format result as YAML"
    , Option ['s'] ["socket"]
        (ReqArg (\s cfg -> cfg{ clientSocket = s }) "SOCKET") $
        unwords [ "ZeroMQ socket of the server ( default:"
                , clientSocket compileTimeClientConfig
                , ")"
                ]
    , Option ['d'] ["detach"]
        (NoArg $ \cfg -> cfg{ clientDetach = True }) $
        unwords [ "Detach the server process ( default:"
                , show $ clientDetach compileTimeClientConfig
                , ")"
                ]
    ]

getClientConfig :: [(ClientConfig -> ClientConfig)] -> IO ClientConfig
getClientConfig fs = do
    changeWorkingDirectory . (fromMaybe err) =<< getEnv "HOME"
    validLocs   <- liftIO $ filterM fileExist locs
    cfgDefaults <- loadAppSettings validLocs [configClientYmlValue] useEnv
    -- Override default config with command-line options
    return $ foldl (flip ($)) cfgDefaults fs
  where
    err = "No HOME environment variable"
    cfgFile = clientConfig compileTimeClientConfig
    -- Look for the config file in . and work-dir/network
    locs = [ cfgFile
           , concat [ spvWorkDir compileTimeSPVConfig
                    , "/", networkName
                    , "/", cfgFile
                    ]
           ]

clientMain :: IO ()
clientMain = E.getArgs >>= \args -> case getOpt Permute options args of
    (fs, commands, []) -> do
        cfg <- getClientConfig fs
        exists <- fileExist workDir
        when exists $ changeWorkingDirectory workDir
        dispatchCommand cfg commands
        return ()
    (_, _, msgs) -> forM_ (msgs ++ usage) putStrLn
  where
    workDir = concat [ spvWorkDir compileTimeSPVConfig
                     , "/", networkName
                     ]

dispatchCommand :: ClientConfig -> [String] -> IO ()
dispatchCommand cfg args = flip S.evalStateT cfg $ case args of
    "start"       : srvCfg                 -> cmdStart srvCfg
    "stop"        : srvCfg                 -> cmdStop srvCfg
    "newwallet"   : mnemonic               -> cmdNewWallet mnemonic
    "getwallet"   : []                     -> cmdGetWallet
    "walletlist"  : []                     -> cmdGetWallets
    "newacc"      : [name]                 -> cmdNewAcc name
    "newms"       : name : m : n : ks      -> cmdNewMS name m n ks
    "newread"     : [name, key]            -> cmdNewRead name key
    "newreadms"   : name : m : n : ks      -> cmdNewReadMS name m n ks
    "addkeys"     : name : ks              -> cmdAddKeys name ks
    "getacc"      : [name]                 -> cmdGetAcc name
    "acclist"     : []                     -> cmdAccList
    "list"        : [name]                 -> cmdList name
    "page"        : name : page            -> cmdPage name page
    "new"         : [name, label]          -> cmdNew name label
    "label"       : [name, index, label]   -> cmdLabel name index label
    "txlist"      : name : []              -> cmdTxList name
    "txpage"      : name : page            -> cmdTxPage name page
    "send"        : name : add : amnt : [] -> cmdSend name add amnt
    "sendmany"    : name : xs              -> cmdSendMany name xs
    "signtx"      : [name, tx]             -> cmdSignTx name tx
    "importtx"    : [name, tx]             -> cmdImportTx name tx
    "getoffline"  : [name, tid]            -> cmdGetOffline name tid
    "signoffline" : [name, offdata]        -> cmdSignOffline name offdata
    "balance"     : [name]                 -> cmdBalance name
    "spendable"   : [name]                 -> cmdSpendable name
    "getprop"     : [name, hash]           -> cmdGetProp name hash
    "gettx"       : [name, hash]           -> cmdGetTx name hash
    "rescan"      : rescantime             -> cmdRescan rescantime
    "decodetx"    : [tx]                   -> cmdDecodeTx tx
    "help"        : []                     -> liftIO $ forM_ usage putStrLn
    "version"     : []                     -> liftIO $ putStrLn haskoinUserAgent
    []                                     -> liftIO $ forM_ usage putStrLn
    _ -> liftIO $ forM_ ("Invalid command" : usage) $ putStrLn

