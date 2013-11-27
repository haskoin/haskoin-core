{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Directory
import System.IO.Error
import System.Console.GetOpt
import qualified System.Environment as E

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either
import Control.Exception (tryJust)

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Maybe
import Data.Char
import Data.Word
import qualified Data.Text as T
import qualified Data.Yaml as YAML
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as BS

import Haskoin.Wallet.Keys
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Store
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
    [ "hw commands: " 
    , "  init       <seed>                      Initialize a wallet"
    , "  list       [acc]                       Display most recent addresses" 
    , "  listfrom   <from> [acc]                Display addresses from an index" 
    , "  listall    [acc]                       Display all addresses" 
    , "  new        <label> [acc]               Generate address with a label"
    , "  genaddr    [acc]                       Generate new addresses"
    , "  label      <index> <label> [acc]       Add a label to an address"
    , "  balance    [acc]                       Display account balance"
    , "  totalbalance                           Display total balance"
    , "  listtx     [acc]                       Display transactions"
    , "  send       addr amount [acc]           Send coins to an address"
    , "  signtx     <tx>                        Sign a transaction"
    , "  focus      <acc>                       Set the focused account"
    , "  accinfo    [acc]                       Display account information"
    , "  listacc                                List all accounts"
    , "  newacc     <name>                      Create a new account"
    , "  newms      <name> <M> <N> {pubkey...}  Create a new multisig account"
    , "  addkey     <pubkey> [acc]              Add pubkey to a multisig account"
    , "  dumpkeys   [acc]                       Dump pubkey to stdout"
    , "  importtx   <tx>                        Import transaction"
    , "  coins      [acc]                       List transaction outputs"
    , "  allcoins                               List all transaction outputs"
    , "  decodetx   <tx>                        Decode HEX transaction"
    , "  buildrawtx {txid:id...} {addr:amnt...} Build a new transaction"
    , "  signrawtx  <tx> {txid:id:script...}    Sign a raw transaction"
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

catchEx :: IOError -> Maybe String
catchEx = return . ioeGetErrorString

process :: Options -> [String] -> IO ()
process opts xs 
    -- -h and -v can be called without a command
    | optHelp opts = formatStr usage
    | optVersion opts = print versionMsg
    -- otherwise require a command
    | null xs = formatStr usage
    | otherwise = getWorkDir >>= \dir -> do
        let (cmd,args) = (head xs, tail xs)
        res <- tryJust catchEx $ runSqlite (T.pack dir) $ runEitherT $ do
            lift $ runMigration migrateAll
            dispatchCommand cmd opts args 
        case join res of
            Left err -> formatStr err
            Right val -> if val == YAML.Null then return () else if optJson opts 
                then formatStr $ bsToString $ toStrictBS $ 
                    JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } val
                else formatStr $ bsToString $ YAML.encode val

type Command m = EitherT String m YAML.Value
type Args = [String]

whenArgs :: Monad m => Args -> (Int -> Bool) -> Command m -> Command m
whenArgs args f cmd = if f $ length args then cmd else argErr
    where argErr = left "Invalid number of arguments"

dispatchCommand :: (PersistStore m, PersistUnique m, PersistQuery m) 
                => String -> Options -> Args -> Command m
dispatchCommand cmd opts args = case cmd of
    "init" -> whenArgs args (== 1) $ cmdInit $ head args
    "list" -> whenArgs args (== 1) $ cmdList (head args) 0 (optCount opts)
    "listpage" -> whenArgs args (== 2) $ 
        cmdList (head args) (read $ args !! 1) (optCount opts)
    "new" -> whenArgs args (>= 2) $ cmdGenWithLabel (head args) $ drop 1 args
    "genaddr" -> whenArgs args (== 1) $ cmdGenAddr (head args) (optCount opts)
    "label" -> whenArgs args (== 3) $ 
        cmdLabel (head args) (read $ args !! 1) (args !! 2)
    "newacc" -> whenArgs args (== 1) $ cmdNewAcc $ head args
    "newms" -> whenArgs args (>= 3) $ do
        keys <- liftMaybe "newms: Invalid keys" $ mapM xPubImport $ drop 3 args
        cmdNewMS (args !! 0) (read $ args !! 1) (read $ args !! 2) keys
    "addkey" -> whenArgs args (>= 2) $ do
        keys <- liftMaybe "newms: Invalid keys" $ mapM xPubImport $ drop 1 args
        cmdAddKeys (head args) keys
    "accinfo" -> whenArgs args (== 1) $ cmdAccInfo $ head args
    "listacc" -> whenArgs args (== 0) cmdListAcc 
    "dumpkeys" -> whenArgs args (== 1) $ cmdDumpKeys $ head args
--    "balance"      -> withFocus args 1 $ \acc -> cmdBalance acc
--    "totalbalance" -> whenArgs args (== 0) cmdTotalBalance
--    "listtx"       -> withFocus args 1 cmdListTx
--    "send"         -> withFocus args 3 $ \acc -> 
--        cmdSend (head args) (read $ args !! 1) acc
--    "signtx"       -> withFocus args 2 $ \acc -> cmdSignTx (head args) acc
--    "importtx"     -> whenArgs args (== 1) $ cmdImportTx $ head args
--    "coins"        -> withFocus args 1 $ \acc -> cmdCoins acc
--    "allcoins"     -> whenArgs args (== 0) cmdAllCoins
--    "decodetx"     -> whenArgs args (== 1) $ cmdDecodeTx $ head args
--    "buildrawtx"   -> whenArgs args (>= 2) $ do
--        let xs     = map (splitOn ":") args
--            (os,as) = span ((== 64) . length . head) xs
--            f [a,b] = return (a,read b) 
--            f _     = left "buildtx: Invalid syntax"
--        os' <- mapM f os
--        as' <- mapM f as
--        cmdBuildRawTx os' as'
--    "signrawtx"    -> whenArgs args (>= 2) $ do 
--        let f [t,i,s] = return (t,read i,s)
--            f _       = left "Invalid syntax for txid:index:script"
--        xs <- mapM (f . (splitOn ":")) $ tail args
--        cmdSignRawTx (head args) xs $ optSigHash opts
    _ -> left $ unwords ["Invalid command:", cmd]

