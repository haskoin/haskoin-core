module Main where

import System.IO
import System.Posix.Env
import System.Posix.Files
import System.Posix.Directory
import qualified System.Environment as E
import System.Console.GetOpt

import Control.Monad
import Control.Applicative
import Control.Exception

import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Crypto
import Haskoin.Util

data Options = Options
    { optInit         :: Bool
    , optGetAddr      :: Bool
    , optCount        :: Int
    , optOffset       :: Int
    , optInternal     :: Bool
    , optAccount      :: Int
    , optFingerprint  :: Bool
    , optDumpMaster   :: Bool
    , optDumpAccPrv   :: Bool
    , optDumpAccPub   :: Bool
    , optHelp         :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optInit        = False
    , optGetAddr     = True
    , optCount       = 5
    , optOffset      = 0
    , optInternal    = False
    , optAccount     = 0
    , optFingerprint = False
    , optDumpMaster  = False
    , optDumpAccPrv  = False
    , optDumpAccPub  = False
    , optHelp        = False
    } 

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['i'] ["init"]  
        (NoArg $ \opts -> opts{ optInit = True }) $
        "Initialize a new wallet seeding from /dev/urandom"
    , Option ['g'] ["getaddr"] 
        (NoArg $ \opts -> opts{ optGetAddr = True }) $
        "Generate addresses"
    , Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "How many addresses to generate. Used with option -g"
    , Option ['o'] ["offset"] (ReqArg parseOffset "INT") $
        "Index offset for generating addresses. Used with option -g"
    , Option ['N'] ["internal"]
        (NoArg $ \opts -> opts{ optInternal = True }) $
        "Generate addresses from the internal chain. Used with option -g"
    , Option ['a'] ["account"] (ReqArg parseAccount "INT") $
        "Account index. Default account is 0"
    , Option ['f'] ["fingerprint"]
        (NoArg $ \opts -> opts{ optFingerprint = True }) $
        "Print master key and account key fingerprint. Used with option -a"
    , Option ['m'] ["dumpmaster"]
        (NoArg $ \opts -> opts{ optDumpMaster = True }) $
        "Dump master key to stdout"
    , Option ['k'] ["dumpaccprv"]
        (NoArg $ \opts -> opts{ optDumpAccPrv = True }) $
        "Dump specified account private key to stdout. Used with option -a"
    , Option ['K'] ["dumpaccpub"]
        (NoArg $ \opts -> opts{ optDumpAccPub = True }) $
        "Dump specified account public key to stdout. Used with option -a"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> opts{ optHelp = True }) $
        "Display wallet usage information"
    ]
 
usageHeader :: String
usageHeader = "Usage: hw [OPTION...]"

parseCount :: String -> Options -> Options
parseCount s opts 
    | res > 0   = opts{ optCount = res }
    | otherwise = error $ "Invalid count option: " ++ s
    where res = read s

parseOffset :: String -> Options -> Options
parseOffset s opts 
    | res >= 0 && res < 0x80000000 = opts{ optOffset = res }
    | otherwise = error $ "Invalid offset option: " ++ s
    where res = read s

parseAccount :: String -> Options -> Options
parseAccount s opts 
    | res >= 0 && res < 0x80000000 = opts{ optAccount = res }
    | otherwise = error $ "Invalid account option: " ++ s
    where res = read s

main :: IO ()
main = do
    args <- E.getArgs
    case getOpt Permute options args of
        (o,[],[]) -> do
            when (null o) $ error $ usageInfo usageHeader options
            let opts = foldl (flip id) defaultOptions o
            if optHelp opts
                then putStrLn $ usageInfo usageHeader options
                else process opts
        (_,nonOpts,[]) ->
            error $ "Unrecognized arguments: " ++ unwords nonOpts
        (_,_,msgs) ->
            error $ concat msgs ++ usageInfo usageHeader options

printWarning :: IO ()
printWarning = putStrLn $ "**This software is experimental. " 
    ++ "Only use small amounts of Bitcoins you can afford to loose**"

-- Get Haskoin home directory
getHome :: IO FilePath
getHome = do
    haskoinHome <- getEnv "HASKOIN_HOME"
    if isJust haskoinHome
        then return $ fromJust haskoinHome
        else do
            home <- getEnv "HOME"
            unless (isJust home) $ error $
                "Please set $HASKOIN_HOME or $HOME environment variables" 
            return $ fromJust home

-- Create and get Haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    home <- getHome
    let haskoinDir = home ++ "/.haskoin"
        walletDir  = haskoinDir ++ "/wallet"
    e1 <- fileExist haskoinDir
    unless e1 $ createDirectory haskoinDir ownerModes
    e2 <- fileExist walletDir
    unless e2 $ do
        createDirectory walletDir ownerModes
        putStrLn $ "Haskoin working directory created: " ++ walletDir
    return walletDir

loadKey :: FilePath -> IO MasterKey
loadKey dir = do
    -- Load master key file
    let keyFile = dir ++ "/key"  
    exists <- fileExist keyFile
    unless exists $ error $
        "Wallet file does not exist: " ++ keyFile
    keyString <- readFile keyFile
    let keyM = loadMasterKey =<< (xPrvImport $ stringToBS keyString) 
    unless (isJust keyM) $ error $
        "Failed to parse master key from file: " ++ keyFile
    return $ fromJust keyM

process :: Options -> IO ()
process opts = do
    dir <- getWorkDir
    when (optInit opts) $ actionInit dir
    key <- loadKey dir
    actionDispatch key opts

-- We only want to execute one action-level command at a time
actionDispatch :: MasterKey -> Options -> IO ()
actionDispatch key opts
    | optFingerprint opts = printWarning >> actionFingerprint key opts
    | optDumpMaster opts  = actionDumpMaster key
    | optDumpAccPub opts  = actionDumpAccPub key opts
    | optDumpAccPrv opts  = actionDumpAccPrv key opts
    | optGetAddr opts     = printWarning >> actionGetAddr key opts

actionDumpMaster :: MasterKey -> IO ()
actionDumpMaster key = putStr $ bsToString $ xPrvExport $ runMasterKey key 

actionDumpAccPub :: MasterKey -> Options -> IO ()
actionDumpAccPub key opts = do
    unless (isJust accKeyM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    putStr $ bsToString $ xPubExport $ runAccPubKey $ fromJust accKeyM
    where accKeyM = accPubKey key (fromIntegral $ optAccount opts)

actionDumpAccPrv :: MasterKey -> Options -> IO ()
actionDumpAccPrv key opts = do
    unless (isJust accKeyM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    putStr $ bsToString $ xPrvExport $ runAccPrvKey $ fromJust accKeyM
    where accKeyM = accPrvKey key (fromIntegral $ optAccount opts)

actionFingerprint :: MasterKey -> Options -> IO ()
actionFingerprint key opts = do
    let accM     = accPrvKey key (fromIntegral $ optAccount opts)
    unless (isJust accM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    let acc      = fromJust accM
        masterFP = bsToHex $ encode' $ xPrvFP $ runMasterKey key
        masterID = bsToHex $ encode' $ xPrvID $ runMasterKey key
        accFP    = bsToHex $ encode' $ xPrvFP $ runAccPrvKey acc
        accID    = bsToHex $ encode' $ xPrvID $ runAccPrvKey acc
    putStrLn $ "Master key fingerprint: " ++ (bsToString masterFP)
    putStrLn $ "Master key ID: " ++ (bsToString masterID)
    putStrLn $  "Account " ++ (show $ optAccount opts) 
             ++ " fingerprint: " ++ (bsToString accFP)
    putStrLn $  "Account " ++ (show $ optAccount opts) 
             ++ " ID: " ++ (bsToString accID)

actionGetAddr :: MasterKey -> Options -> IO ()
actionGetAddr key opts = do
    let accM = accPubKey key (fromIntegral $ optAccount opts)
    unless (isJust accM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    let f   = if optInternal opts then intPubKeys else extPubKeys
        acc = fromJust accM
        ps  = take (optCount opts) $ f acc (fromIntegral $ optOffset opts)
        as  = map (addrToBase58 . addr) ps
        beg = optOffset opts
        end = beg + (length as) - 1
    putStrLn $ "Account " ++ (show $ optAccount opts) ++ ": " ++
        "Addresses " ++ (show beg) ++ " to " ++ (show end)
    forM_ as (putStrLn . bsToString)

actionInit :: FilePath -> IO ()
actionInit dir = do
    let keyFile = dir ++ "/key"
    exists <- fileExist keyFile
    when exists $ error $
        "Wallet file already exists: " ++ keyFile
    seed <- devURandom 16 --128 bits of entropy
    let keyM = makeMasterKey seed
    -- This is very unlikely
    unless (isJust keyM) $ error $
        "Can't generate a wallet from your seed. Please try another one"
    let key = xPrvExport $ runMasterKey $ fromJust keyM
    writeFile keyFile (bsToString key)
    setFileMode keyFile $ unionFileModes ownerReadMode ownerWriteMode
    putStrLn $ "Wallet initialized in: " ++ dir ++ "/key"

