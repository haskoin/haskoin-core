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
import qualified Data.Yaml as YAML
import qualified Data.Aeson.Encode.Pretty as JSON
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

data Options = Options
    { optCount    :: Int
    , optSigHash  :: SigHash
    , optJson     :: Bool
    , optHelp     :: Bool
    , optVersion  :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optJson     = False
    , optHelp     = False
    , optVersion  = False
    } 

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
    , Option ['j'] ["json"]
        (NoArg $ \opts -> return opts{ optJson = True }) $
        "Format result as JSON (default: YAML)"
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
    , "  send      addr amount [acc]           Send coins to an address"
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
    (o,xs,[]) -> do
        opts <- foldl (>>=) (return defaultOptions) o
        process opts xs
    (_,_,msgs) -> print $ unlines $ msgs ++ [usage]

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return $ dir ++ "/walletdb"

process :: Options -> [String] -> IO ()
process opts xs 
    -- -h and -v can be called without a command
    | optHelp opts = formatStr usage
    | optVersion opts = print versionMsg
    -- otherwise require a command
    | null xs = formatStr usage
    | otherwise = getWorkDir >>= \dir -> do
        let (cmd,args) = (head xs, tail xs)
        res <- runResourceT $ runWalletDB dir $ do
            checkInit cmd
            dispatchCommand cmd opts args 
        case res of
            Left  err -> formatStr err
            Right val -> if optJson opts 
                then formatStr $ bsToString $ toStrictBS $ 
                    JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } val
                else formatStr $ bsToString $ YAML.encode val

checkInit :: String -> WalletDB (ResourceT IO) ()
checkInit cmd 
    -- Commands that can be called without an initialized database
    | cmd `elem` [ "init", "decodetx", "buildtx" ] = return ()
    | otherwise = dbExists "config" >>= \exists -> if exists
        then return ()
        else left "Wallet not initialized. Call init first"

whenArgs :: Args -> (Int -> Bool) -> Command -> Command
whenArgs args f cmd = if f $ length args then cmd else argErr
    where argErr = left "Invalid number of arguments"

withFocus :: Args -> Int -> (String -> Command) -> Command
withFocus args i f 
    | length args < i  = dbGetConfig cfgFocus >>= f
    | length args == i = f $ last args
    | otherwise        = left "Invalid number of arguments"

dispatchCommand :: String -> Options -> Args -> Command
dispatchCommand cmd opts args = case cmd of
    "init"         -> whenArgs args (== 1) $ cmdInit $ head args 
    "list"         -> withFocus args 1 $ \acc -> cmdList (optCount opts) acc
    "listfrom"     -> withFocus args 2 $ \acc -> 
        cmdListFrom (read $ args !! 0) (optCount opts) acc
    "listall"      -> withFocus args 1 $ \acc -> cmdListAll acc
    "new"          -> withFocus args 2 $ \acc -> cmdNew (args !! 0) acc
    "genaddr"      -> withFocus args 1 $ \acc -> cmdGenAddr (optCount opts) acc
    "focus"        -> whenArgs args (== 1) $ cmdFocus $ head args
    "newacc"       -> whenArgs args (== 1) $ cmdNewAcc $ head args
    "newms"        -> whenArgs args (>= 3) $ 
        cmdNewMS (args !! 0) (read $ args !! 1) $ drop 2 args
    "listacc"      -> whenArgs args (== 0) cmdListAcc 
    "label"        -> withFocus args 3 $ \acc -> 
        cmdLabel (read $ args !! 0) (args !! 1) acc
    "balance"      -> withFocus args 1 $ \acc -> cmdBalance acc
    "totalbalance" -> whenArgs args (== 0) cmdTotalBalance
    "send"         -> withFocus args 3 $ \acc -> 
        cmdSend (head args) (read $ args !! 1) acc
    "dumpkey"      -> withFocus args 1 $ \acc -> cmdDumpKey acc
    "importtx"     -> whenArgs args (== 1) $ cmdImportTx $ head args
    "coins"        -> withFocus args 1 $ \acc -> cmdCoins acc
    "allcoins"     -> whenArgs args (== 0) cmdAllCoins
    "decodetx"     -> whenArgs args (== 1) $ cmdDecodeTx $ head args
    "buildtx"      -> whenArgs args (>= 2) $ do
        let xs     = map (splitOn ":") args
            (os,as) = span ((== 64) . length . head) xs
            f [a,b] = return (a,read b) 
            f _     = left "buildtx: Invalid syntax"
        os' <- mapM f os
        as' <- mapM f as
        cmdBuildTx os' as'
    "signtx"       -> whenArgs args (>= 2) $ do 
        let f [t,i,s] = return (t,read i,s)
            f _       = left "Invalid syntax for txid:index:script"
        xs <- mapM (f . (splitOn ":")) $ tail args
        cmdSignTx (head args) xs $ optSigHash opts
    _ -> left $ unwords ["Invalid command:", cmd]

