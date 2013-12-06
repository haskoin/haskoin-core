{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import System.IO
import System.Directory
import System.IO.Error
import System.Console.GetOpt
import qualified System.Environment as E

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
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
    , optFee      :: Int
    , optJson     :: Bool
    , optHelp     :: Bool
    , optVersion  :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optFee      = 10000
    , optJson     = False
    , optHelp     = False
    , optVersion  = False
    } 

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Count: see commands for details"
    , Option ['s'] ["sighash"] (ReqArg parseSigHash "SIGHASH") $
        "Signature type = ALL|NONE|SINGLE"
    , Option ['a'] ["anyonecanpay"]
        (NoArg $ \opts -> do
            let sh = optSigHash opts
            return opts{ optSigHash = sh{ anyoneCanPay = True } }
        ) $ "Set signature flag AnyoneCanPay"
    , Option ['f'] ["fee"] (ReqArg parseCount "INT") $
        "Transaction fee (default: 10000)"
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
    [ "hw wallet commands: " 
    , "  init       seed                    Initialize a wallet"
    , "  list       acc                     Display last page of addresses"
    , "  listpage   acc page [-c res/page]  Display addresses by page"
    , "  new        acc {labels...}         Generate address with labels"
    , "  genaddr    acc [-c count]          Generate new addresses"
    , "  label      acc index label         Add a label to an address"
    , "  balance    acc                     Display account balance"
    , "  balances                           Display all balances"
    , "  tx         acc                     Display transactions"
    , "  send       acc addr amount         Send coins to an address"
    , "  sendmany   acc {addr:amount...}    Send coins to many addresses"
    , "  newacc     name                    Create a new account"
    , "  newms      name M N {pubkey...}    Create a new multisig account"
    , "  addkeys    acc {pubkey...}         Add pubkeys to a multisig account"
    , "  accinfo    acc                     Display account information"
    , "  listacc                            List all accounts"
    , "  dumpkeys   acc                     Dump account keys to stdout"
    , "  wif        acc index               Dump prvkey as WIF to stdout"
    , "  coins      acc                     List coins"
    , "  allcoins                           List all coins per account"
    , "  signtx     tx                      Sign a transaction"
    , "  importtx   tx                      Import transaction"
    , "  removetx   txid                    Remove transaction"
    , ""
    , "hw utility commands: "
    , "  decodetx   tx                      Decode HEX transaction"
    , "  buildrawtx"
    , "      '[{\"txid\":txid,\"vout\":n},...]' '{addr:amnt,...}'"
    , "  signrawtx "  
    , "      tx" 
    , "      " ++ sigdata
    , "      '[prvkey,...]' [-s SigHash]" 
    ]
  where 
    sigdata = concat
        [ "'[{"
        , "\"txid\":txid,"
        , "\"vout\":n,"
        , "\"scriptPubKey\":hex,"
        , "\"scriptRedeem\":hex"
        , "},...]'"
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

dispatchCommand :: ( PersistStore m, PersistUnique m, PersistQuery m
                   , PersistMonadBackend m ~ SqlBackend
                   ) 
                => String -> Options -> Args -> Command m
dispatchCommand cmd opts args = case cmd of
    "init" -> whenArgs args (== 1) $ cmdInit $ head args
    "list" -> whenArgs args (== 1) $ cmdList (head args) 0 (optCount opts)
    "listpage" -> whenArgs args (== 2) $ 
        cmdList (head args) (read $ args !! 1) (optCount opts)
    "new" -> whenArgs args (>= 2) $ cmdGenWithLabel (head args) $ drop 1 args
    "genaddr" -> whenArgs args (== 1) $ cmdGenAddrs (head args) (optCount opts)
    "label" -> whenArgs args (== 3) $ 
        cmdLabel (head args) (read $ args !! 1) (args !! 2)
    "balance" -> whenArgs args (== 1) $ cmdBalance $ head args
    "balances" -> whenArgs args (== 0) cmdBalances
    "tx" -> whenArgs args (== 1) $ cmdListTx $ head args
    "send" -> whenArgs args (== 3) $ 
        cmdSend (head args) (args !! 1) (read $ args !! 2) (optFee opts)
    "sendmany" -> whenArgs args (>= 2) $ do
        let f [a,b] = (T.unpack a,read $ T.unpack b)
            f _     = error "sendmany: Invalid format addr:amount"
            dests   = map (f . (T.splitOn (T.pack ":")) . T.pack) $ drop 1 args
        cmdSendMany (head args) dests (optFee opts)
    "newacc" -> whenArgs args (== 1) $ cmdNewAcc $ head args
    "newms" -> whenArgs args (>= 3) $ do
        keys <- liftMaybe "newms: Invalid keys" $ mapM xPubImport $ drop 3 args
        cmdNewMS (args !! 0) (read $ args !! 1) (read $ args !! 2) keys
    "addkeys" -> whenArgs args (>= 2) $ do
        keys <- liftMaybe "newms: Invalid keys" $ mapM xPubImport $ drop 1 args
        cmdAddKeys (head args) keys
    "accinfo" -> whenArgs args (== 1) $ cmdAccInfo $ head args
    "listacc" -> whenArgs args (== 0) cmdListAcc 
    "dumpkeys" -> whenArgs args (== 1) $ cmdDumpKeys $ head args
    "wif" -> whenArgs args (== 2) $ cmdWIF (head args) (read $ args !! 1)
    "coins" -> whenArgs args (== 1) $ cmdCoins $ head args
    "allcoins" -> whenArgs args (== 0) cmdAllCoins
    "signtx" -> whenArgs args (== 2) $ do
        bs <- liftMaybe "signtx: Invalid HEX encoding" $ hexToBS $ args !! 1
        tx <- liftEither $ decodeToEither bs
        cmdSignTx (head args) tx (optSigHash opts)
    "importtx" -> whenArgs args (== 1) $ do
        bs <- liftMaybe "signtx: Invalid HEX encoding" $ hexToBS $ head args
        tx <- liftEither $ decodeToEither bs
        cmdImportTx tx
    "removetx" -> whenArgs args (== 1) $ cmdRemoveTx $ head args
    "decodetx" -> whenArgs args (== 1) $ cmdDecodeTx $ head args
    "buildrawtx" -> whenArgs args (== 2) $ cmdBuildRawTx (head args) (args !! 1)
    "signrawtx"    -> whenArgs args (== 3) $ do 
        bs <- liftMaybe "signtx: Invalid HEX encoding" $ hexToBS $ head args
        tx <- liftEither $ decodeToEither bs
        cmdSignRawTx tx (args !! 1) (args !! 2) (optSigHash opts)
    _ -> left $ unwords ["Invalid command:", cmd]

