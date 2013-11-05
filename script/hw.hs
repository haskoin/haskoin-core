module Main where

import System.IO
import System.Directory
import qualified System.Environment as E
import System.Console.GetOpt

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either
import Control.Exception

import Data.Maybe
import Data.Char
import Data.Word
import Data.Yaml
import Data.List.Split
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Haskoin.Wallet.Keys
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Store
import Haskoin.Wallet.Commands
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Address count"
    , Option ['s'] ["sighash"] (ReqArg parseSigHash "SIGHASH") $
        "Signature type = ALL|NONE|SINGLE"
    , Option ['a'] ["anyonecanpay"]
        (NoArg $ \opts -> do
            let sh = optSigHash opts
            return opts{ optSigHash = sh{ anyoneCanPay = True } }
        ) $ "Set signature flag AnyoneCanPay"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> return opts{ optHelp = True }) $
        "Display this help message"
    , Option ['v'] ["version"]
        (NoArg $ \opts -> return opts{ optVersion = True }) $
        "Show version information"
    ]

parseCount :: String -> Options -> IO Options
parseCount s opts 
    | res > 0   = return opts{ optCount = res }
    | otherwise = error $ unwords ["Invalid count option:", s]
    where res = read s

parseSigHash :: String -> Options -> IO Options
parseSigHash s opts = return opts{ optSigHash = res }
    where acp = anyoneCanPay $ optSigHash opts
          res | s == "ALL" = SigAll acp
              | s == "NONE" = SigNone acp
              | s == "SINGLE" = SigSingle acp
              | otherwise = error "SigHash must be one of ALL|NONE|SINGLE"

usageHeader :: String
usageHeader = "Usage: hw [<options>] <command> [<args>]"

cmdHelp :: [String]
cmdHelp = 
    [ "Valid hw commands: " 
    , "  init      <seed>                      Initialize a wallet"
    , "  list      [acc]                       Display most recent addresses" 
    , "  listfrom  <from> [acc]                Display addresses from an index" 
    , "  listall   [acc]                       Display all addresses" 
    , "  new       <label> [acc]               Generate address with a label"
    , "  genaddr   [acc]                       Generate new addresses"
    , "  label     <index> <label> [acc]       Add a label to an address"
    , "  balance   [acc]                       Display account balance"
    , "  totalbalance                          Display total balance"
    , "  focus     <acc>                       Set the focused account"
    , "  newacc    <name>                      Create a new account"
    , "  newms     <name> <M> {pubkeys...}     Create a new multisig account"
    , "  listacc                               List all accounts"
    , "  dumpkey   [acc]                       Dump pubkey to stdout"
    , "  importtx  <tx>                        Import transaction"
    , "  coins     [acc]                       List transaction outputs"
    , "  allcoins                              List all transaction outputs"
    , "  decodetx  <tx>                        Decode HEX transaction"
    , "  buildtx   {txid:id...} {addr:amnt...} Build a new transaction"
    , "  signtx    <tx> {txid:id:script...}    Sign a transaction"
    ]

warningMsg :: String
warningMsg = unwords [ "***"
                     , "This software is experimental."
                     , "Use only small amounts of Bitcoins"
                     , "***"
                     ]

versionMsg :: String
versionMsg = "haskoin wallet version 0.1.1.0"

usage :: String
usage = unlines $ [warningMsg, usageInfo usageHeader options] ++ cmdHelp

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

main :: IO ()
main = E.getArgs >>= \args -> case getOpt Permute options args of
    (o,n,[]) -> do
        opts <- foldl (>>=) (return defaultOptions) o
        process opts n
    (_,_,msgs) -> print $ unlines $ msgs ++ [usage]

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return $ dir ++ "/walletdb"

process :: Options -> [String] -> IO ()
process opts cs 
    -- -h and -v can be called without a command
    | optHelp opts = formatStr usage
    | optVersion opts = print versionMsg
    -- otherwise require a command
    | null cs = formatStr usage
    | otherwise = getWorkDir >>= \dir -> do
        let (c,args) = (head cs, tail cs)
        res <- runResourceT $ runWalletDB dir $ checkInit c >> case c of
            "init"         -> cmdInit opts args
            "list"         -> cmdList opts args
            "listfrom"     -> cmdListFrom opts args
            "listall"      -> cmdListAll opts args
            "new"          -> cmdNew opts args
            "genaddr"      -> cmdGenAddr opts args
            "label"        -> cmdLabel opts args
            "balance"      -> cmdBalance opts args
            "totalbalance" -> cmdTotalBalance opts args
            "focus"        -> cmdFocus opts args
            "newacc"       -> cmdNewAcc opts args
            "newms"        -> cmdNewMS opts args
            "listacc"      -> cmdListAcc opts args
            "dumpkey"      -> cmdDumpKey opts args
            "importtx"     -> cmdImportTx opts args
            "coins"        -> cmdCoins opts args
            "allcoins"     -> cmdAllCoins opts args 
            "decodetx"     -> cmdDecodeTx opts args
            "buildtx"      -> cmdBuildTx opts args
            "signtx"       -> cmdSignTx opts args
            _              -> left $ unwords ["Invalid command:", c]
        case res of
            Left  err -> print err
            Right val -> when (val /= Null ) $ 
                formatStr $ bsToString $ encode val

checkInit :: String -> WalletDB (ResourceT IO) ()
checkInit str 
    -- Commands that can be called without an initialized database
    | str `elem` [ "init", "decodetx", "buildtx" ] = return ()
    | otherwise = dbExists "config" >>= \exists -> if exists
        then return ()
        else left "Wallet not initialized. Call init first"

