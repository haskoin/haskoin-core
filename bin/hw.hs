module Main where

import System.IO
import System.Directory
import qualified System.Environment as E
import System.Console.GetOpt

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Exception

import Data.Maybe
import Data.Char
import Data.Word
import Data.List.Split
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.Store
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

data Options = Options
    { optCount    :: Int
    , optSigHash  :: SigHash
    , optHelp     :: Bool
    , optVersion  :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optHelp     = False
    , optVersion  = False
    } 

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Address count. Implies list or genaddr command."
    , Option ['s'] ["sighash"] (ReqArg parseSigHash "SIGHASH") $
        "Type of signature. Can be ALL|NONE|SINGLE"
    , Option ['a'] ["anyonecanpay"]
        (NoArg $ \opts -> do
            let sh = optSigHash opts
            return opts{ optSigHash = sh{ anyoneCanPay = True } }
        ) $ "Sign a transaction with the AnyoneCanPay flag set"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> return opts{ optHelp = True }) $
        "Display this help message"
    , Option ['v'] ["version"]
        (NoArg $ \opts -> return opts{ optVersion = True }) $
        "Display wallet version information"
    ]

parseCount :: String -> Options -> IO Options
parseCount s opts 
    | res > 0   = return opts{ optCount = res }
    | otherwise = error $ "Invalid count option: " ++ s
    where res = read s

parseSigHash :: String -> Options -> IO Options
parseSigHash s opts = return opts{ optSigHash = res }
    where acp = anyoneCanPay $ optSigHash opts
          res | s == "ALL" = SigAll acp
              | s == "NONE" = SigNone acp
              | s == "SINGLE" = SigSingle acp
              | otherwise = error "Invalid SigHash. Has to be ALL|NONE|SINGLE"

usageHeader :: String
usageHeader = "Usage: hw [<options>] <command> [<args>]"

cmdHelp :: String
cmdHelp = 
    "Valid hw commands: \n" 
 ++ "  init      <seed>                      " 
 ++ "Initialize a wallet\n"
 ++ "  list      [acc]                       "
 ++ "Display most recent addresses\n" 
 ++ "  listfrom  <from> [acc]                "
 ++ "Display addresses from an index\n" 
 ++ "  listall   [acc]                       "
 ++ "Display all addresses\n" 
 ++ "  new       <label> [acc]               "
 ++ "Generate an address with a label\n"
 ++ "  genaddr   [acc]                       "
 ++ "Generate new addresses\n"
 ++ "  label     <index> <label> [acc]       "
 ++ "Add a label to an address\n"
 ++ "  focus     <acc>                       "
 ++ "Set the focused account\n"
 ++ "  newacc    <name>                      "
 ++ "Create a new account\n"
 ++ "  newms     <name> <M> {pubkeys...}     "
 ++ "Create a new multisig account\n"
 ++ "  listacc                               "
 ++ "List all accounts\n"
 ++ "  dumpkey   [acc]                       "
 ++ "Dump pubkey to stdout\n"
 ++ "  importtx  <tx>                        "
 ++ "Import transaction\n"
 ++ "  listcoins [acc]                       "
 ++ "List transaction outputs\n"
 ++ "  decodetx  <tx>                        "
 ++ "Decode HEX transaction\n"
 ++ "  buildtx   {txid:id...} {addr:amnt...} "
 ++ "Build a new transaction\n"
 ++ "  signtx    <tx> {txid:id:script...}    "
 ++ "Sign a transaction\n"

warningMsg :: String
warningMsg = "\n**This software is experimental. " 
    ++ "Use only small amounts of Bitcoins**\n"

versionMsg :: String
versionMsg = "haskoin wallet version 0.1.1.0"

usage :: String
usage = usageInfo usageHeader options ++ cmdHelp

main :: IO ()
main = do
    putStrLn warningMsg
    args <- E.getArgs
    case getOpt Permute options args of
        (o,n,[]) -> do
            opts <- foldl (>>=) (return defaultOptions) o
            process opts n
        (_,_,msgs) ->
            putStrLn $ concat msgs ++ usageInfo usageHeader options

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return dir

process :: Options -> [String] -> IO ()
process opts cs 
    -- -h and -v can be called without a command
    | optHelp opts = putStrLn usage
    | optVersion opts = putStrLn versionMsg
    -- otherwise require a command
    | null cs = putStrLn usage
    | otherwise = getWorkDir >>= \dir -> do
        let (c,args) = (head cs, tail cs)
        runResourceT $ runWalletDB dir $ checkInit c >> case c of
            "init"      -> cmdInit opts args
            "list"      -> cmdList opts args
            "listfrom"  -> cmdListFrom opts args
            "listall"   -> cmdListAll opts args
            "new"       -> cmdNew opts args
            "genaddr"   -> cmdGenAddr opts args
            "label"     -> cmdLabel args
            "focus"     -> cmdFocus opts args
            "newacc"    -> cmdNewAcc opts args
            "newms"     -> cmdNewMS opts args
            "listacc"   -> cmdListAcc 
            "dumpkey"   -> cmdDumpKey args
            "importtx"  -> cmdImportTx args
            "listcoins" -> cmdListCoins opts args
            "decodetx"  -> cmdDecodeTx opts args
            "buildtx"   -> cmdBuildTx opts args
            "signtx"    -> cmdSignTx opts args
            _           -> error $ "Invalid command: " ++ c
        putStrLn ""

type Args = [String]
type CmdAction = WalletDB (ResourceT IO) ()

checkInit :: String -> CmdAction
checkInit "init" = return ()
checkInit _ = isDBInit >>= \init -> unless init $ error $
    "Database is not initialized. You must call 'init' first."

-- Return the account from the arguments, or get the current focused account
getArgsAcc :: Args -> WalletDB (ResourceT IO) String
getArgsAcc args = case args of
    [] -> fromJust <$> dbGetFocus
    acc:[] -> return acc

formatAddr :: WAddr -> IO ()
formatAddr (WAddr a l _ p _ _ _)
    | null l    = putStrLn def
    | otherwise = putStrLn $ def ++ lab
    where def = (show $ p) ++ ") " ++ a
          lab = " (" ++ l ++ ")"

formatAcc :: WAccount -> String
formatAcc acc
    | isMSAcc acc = "[MultiSig Account " ++ 
        (show $ accMSReq acc) ++ " of " ++ 
        (show $ length (accMSKey acc) + 1) ++ "] " ++
        (accName acc)
    | otherwise = "[Regular Account] " ++ (accName acc)

formatPages :: Int -> Int -> WAccount -> IO ()
formatPages from count acc = do
    putStr $ formatAcc acc
    putStr $ " (Addresses " ++(show from) ++ " to " ++ (show $ from + count - 1) 
    putStrLn $ " of " ++ (show $ accExtCount acc) ++ ")"

formatCoin :: WCoin -> IO ()
formatCoin (WCoin (OutPoint tid i) (TxOut v s) _) = do
    putStrLn $ "{ TxID  : " ++ (show tid)
    putStrLn $ "  Index : " ++ (show i)
    putStrLn $ "  Value : " ++ (show v)
    putStrLn $ "  Script: " ++ (bsToHex $ runPut' $ putScriptOps $ runScript s)
    putStrLn $ "  Addr  : " ++ case decodeOutput s of
        Right (PayPKHash a)     -> addrToBase58 a
        Right (PayScriptHash a) -> addrToBase58 a
        _                       -> error "formatCoin: invalid script type"
    putStrLn "}"

cmdInit :: Options -> Args -> CmdAction
cmdInit opts args
    | length args /= 1 = liftIO $ putStr usage
    | otherwise = do
        dbInit $ head args
        cmdGenAddr opts [] -- generate some addresses

cmdList :: Options -> Args -> CmdAction
cmdList opts args 
    | length args > 1 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc args
        acc  <- fromJust <$> dbGetAcc name
        let total = accExtCount acc
            count = min (optCount opts) total
        if count <= 0 
            then do
                liftIO $ putStrLn $ formatAcc acc
                liftIO $ putStrLn "No addresses to display"
            else do
                addr <- dbListExtAddr name (total - count + 1) count
                liftIO $ formatPages (total - count + 1) (length addr) acc
                liftIO $ forM_ addr formatAddr

cmdListFrom :: Options -> Args -> CmdAction
cmdListFrom opts args 
    | length args > 2 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc $ drop 1 args
        acc  <- fromJust <$> dbGetAcc name
        addr <- dbListExtAddr name from $ optCount opts
        liftIO $ if null addr
            then putStrLn "No addresses to display"
            else do
                formatPages from (length addr) acc
                forM_ addr formatAddr
    where from = read $ args !! 0

cmdListAll :: Options -> Args -> CmdAction
cmdListAll opts args 
    | length args > 1 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc args
        acc <- fromJust <$> dbGetAcc name
        addr <- dbListExtAddr name 1 (accExtCount acc)
        liftIO $ formatPages 1 (length addr) acc
        liftIO $ forM_ addr formatAddr

cmdNew :: Options -> Args -> CmdAction
cmdNew opts args 
    | length args > 2 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc $ drop 1 args
        acc <- fromJust <$> dbGetAcc name
        waddr <- head <$> (dbGenExtAddr name 1)
        let newAddr = waddr{ wLabel = args !! 0 }
        dbPutExtAddr newAddr
        liftIO $ putStrLn $ formatAcc acc
        liftIO $ formatAddr newAddr

cmdGenAddr :: Options -> Args -> CmdAction
cmdGenAddr opts args 
    | length args > 1 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc args
        addr <- dbGenExtAddr name $ optCount opts
        cmdList opts args

cmdFocus :: Options -> Args -> CmdAction
cmdFocus opts args
    | length args /= 1 = liftIO $ putStr usage
    | otherwise = do
        dbPutFocus $ head args 
        cmdList opts args

cmdNewAcc :: Options -> Args -> CmdAction
cmdNewAcc opts args
    | length args /= 1 = liftIO $ putStr usage
    | otherwise = do
        dbNewAcc $ head args 
        cmdGenAddr opts args

cmdNewMS :: Options -> Args -> CmdAction
cmdNewMS opts args
    | length args < 3 = liftIO $ putStr usage
    | otherwise = do
        dbNewMSAcc name r keys
        cmdGenAddr opts [head args]
    where name = args !! 0
          r    = read $ args !! 1
          keys = map (fromJust . xPubImport) $ drop 2 args

cmdListAcc :: CmdAction
cmdListAcc = dbListAcc >>= \accs -> forM_ accs $ \acc -> liftIO $ do
    putStr $ formatAcc acc
    putStrLn $ " (" ++ (show $ accExtCount acc) ++ " addresses)"

cmdLabel :: Args -> CmdAction
cmdLabel args
    | length args > 3 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc $ drop 2 args
        acc  <- fromJust <$> (dbGetAcc name)
        prev <- dbGetExtAddrPos name p
        case prev of
            Nothing    -> error $ "Address index not in wallet: " ++ (show p)
            Just waddr -> do
                let newAddr = waddr{ wLabel = args !! 1 }
                dbPutExtAddr newAddr
                liftIO $ putStrLn $ formatAcc acc
                liftIO $ formatAddr newAddr
    where p = read (args !! 0) 

cmdDumpKey :: Args -> CmdAction
cmdDumpKey args 
    | length args > 1 = liftIO $ putStr usage
    | otherwise = do
        name <- getArgsAcc args
        acc  <- fromJust <$> dbGetAcc name
        liftIO $ putStrLn $ formatAcc acc
        liftIO $ putStrLn $ xPubExport $ runAccPubKey $ accKey acc

cmdImportTx :: Args -> CmdAction
cmdImportTx args
    | length args /= 1 = liftIO $ putStrLn usage
    | otherwise = case txM of
        Nothing -> error "Could not decode transactions"
        Just tx -> do
            coins <- dbImportTx tx
            liftIO $ if null coins
                then putStrLn "No coins imported"
                else forM_ coins formatCoin
        where txM = decodeToMaybe =<< (hexToBS $ head args)

cmdListCoins :: Options -> Args -> CmdAction
cmdListCoins opts args
    | length args > 1 = liftIO $ putStrLn usage
    | otherwise = do
        name <- getArgsAcc args
        acc  <- fromJust <$> dbGetAcc name
        coins <- dbListCoins name
        liftIO $ putStrLn $ formatAcc acc
        liftIO $ if null coins
            then putStrLn "No coins"
            else forM_ coins formatCoin

cmdDecodeTx :: Options -> Args -> CmdAction
cmdDecodeTx opts args
    | null args = liftIO $ putStrLn usage
    | isNothing bs = error "<tx>: Invalid HEX encoding"
    | otherwise = case eitherTx of
        Left err -> error err
        Right tx -> liftIO $ pp tx
    where bs       = hexToBS $ head args
          eitherTx = decodeToEither $ fromJust bs :: Either String Tx

cmdBuildTx :: Options -> Args -> CmdAction
cmdBuildTx opts args
    | length args < 2 = liftIO $ putStr usage
    | otherwise = case buildAddrTx (map f os) (map g as) of
        Right tx -> liftIO $ putStrLn $ bsToHex $ encode' tx
        Left err -> error err
    where xs      = map (splitOn ":") args
          (os,as) = span ((== 64) . length . head) xs
          f [t,i] = OutPoint (decode' $ BS.reverse $ fromJust $ hexToBS t) 
                             (read i)
          g [a,v] = (a,read v)

cmdSignTx :: Options -> Args -> CmdAction
cmdSignTx opts args
    | length args < 2 = liftIO $ putStr usage
    | otherwise = case txM of
        Nothing -> error "Could not decode transaction"
        Just tx -> do
            ys <- catMaybes <$> mapM (g . f) xs
            let sigTx = detSignTx tx (map fst ys) (map snd ys)
            liftIO $ print $ (bsToHex . encode') <$> sigTx
    where txM = decodeToMaybe =<< (hexToBS $ head args)
          xs  = map (splitOn ":") $ tail args
          f [t,i,s] = ( Script $ runGet' getScriptOps $ fromJust $ hexToBS s
                      , OutPoint (decode' $ BS.reverse $ fromJust $ hexToBS t)
                                 (read i)
                      )
          g (s,o) = dbSigData s o (optSigHash opts)

