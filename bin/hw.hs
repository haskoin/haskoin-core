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
import Data.Char
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Crypto
import Haskoin.Util

data Options = Options
    { optCount    :: Int
    , optIndex    :: Int
    , optInternal :: Bool
    , optAccount  :: Int , optMaster   :: Bool
    , optHelp     :: Bool
    , optVersion  :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optCount    = 5
    , optIndex    = 0
    , optInternal = False
    , optAccount  = 0
    , optMaster   = False
    , optHelp     = False
    , optVersion  = False
    } 

data Command = CmdInit
             | CmdAddress
             | CmdFingerprint
             | CmdDumpKey
             | CmdDumpWIF
             deriving (Eq, Show)

strToCmd :: String -> Command
strToCmd str = case str of
    "init"        -> CmdInit
    "address"     -> CmdAddress
    "fingerprint" -> CmdFingerprint
    "dumpkey"     -> CmdDumpKey
    "dumpwif"     -> CmdDumpWIF
    _ -> error $ "Invalid command: " ++ str

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Address generation count. Implies address command"
    , Option ['i'] ["index"] (ReqArg parseIndex "INT") $
        "Address key index. Implies address ot dumpwif command"
    , Option ['N'] ["internal"]
        (NoArg $ \opts -> opts{ optInternal = True }) $
        "Use internal address chain. Implies address or dumpwif command"
    , Option ['a'] ["account"] (ReqArg parseAccount "INT") $
        "Account index to use in your command"
    , Option ['m'] ["master"]
        (NoArg $ \opts -> opts{ optMaster = True }) $
        "Use the master key. Implies dumpkey or fingerprint command"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> opts{ optHelp = True }) $
        "Display this help message"
    , Option ['v'] ["version"]
        (NoArg $ \opts -> opts{ optVersion = True }) $
        "Display wallet version information"
    ]

parseCount :: String -> Options -> Options
parseCount s opts 
    | res > 0   = opts{ optCount = res }
    | otherwise = error $ "Invalid count option: " ++ s
    where res = read s

parseIndex :: String -> Options -> Options
parseIndex s opts 
    | res >= 0 && res < 0x80000000 = opts{ optIndex = res }
    | otherwise = error $ "Invalid index option: " ++ s
    where res = read s

parseAccount :: String -> Options -> Options
parseAccount s opts 
    | res >= 0 && res < 0x80000000 = opts{ optAccount = res }
    | otherwise = error $ "Invalid account option: " ++ s
    where res = read s
 
usageHeader :: String
usageHeader = "Usage: hw COMMAND [OPTIONS...]"

cmdHelp :: String
cmdHelp = 
    "Valid COMMANDS: \n" 
 ++ "  init          Initialize a new wallet (seeding from /dev/urandom)\n" 
 ++ "  address       Prints a list of addresses for the chosen account\n" 
 ++ "  fingerprint   Prints key fingerprint and ID\n"
 ++ "  dumpkey       Dump master or account keys to stdout\n"
 ++ "  dumpwif       Dump private keys in WIF format to stdout"

warningMsg :: String
warningMsg = "**This software is experimental. " 
    ++ "Only use small amounts of Bitcoins you can afford to loose**"

versionMsg :: String
versionMsg = "haskoin wallet version 0.1.1.0"

usage :: String
usage = warningMsg ++ "\n" ++ usageInfo usageHeader options ++ cmdHelp

main :: IO ()
main = do
    args <- E.getArgs
    case getOpt Permute options args of
        (o,n,[]) -> do
            when (null o && null n) $ error usage
            let opts = foldl (flip id) defaultOptions o
            process opts (map strToCmd n)
        (_,_,msgs) ->
            error $ concat msgs ++ usageInfo usageHeader options

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
        "Wallet file does not exist: " ++ keyFile ++ "\n" ++
        "You should run the init command"
    keyString <- rstrip <$> readFile keyFile
    let keyM = loadMasterKey =<< (xPrvImport $ stringToBS keyString) 
    unless (isJust keyM) $ error $
        "Failed to parse master key from file: " ++ keyFile
    return $ fromJust keyM
    where rstrip = reverse . dropWhile isSpace . reverse

process :: Options -> [Command] -> IO ()
process opts cs 
    -- -h and -v can be called without a command
    | optHelp opts = putStrLn usage
    | optVersion opts = putStrLn versionMsg
    -- otherwise require a command
    | length cs /= 1 = error usage
    | otherwise = do
        dir <- getWorkDir
        case head cs of
            CmdInit -> do
                cmdInit dir
            CmdFingerprint -> do
                key <- loadKey dir
                cmdFingerprint key opts
            CmdDumpKey -> do
                key <- loadKey dir
                cmdDumpKey key opts
            CmdDumpWIF -> do
                key <- loadKey dir
                cmdDumpWIF key opts
            CmdAddress -> do
                key <- loadKey dir
                cmdGetAddr key opts
            
cmdDumpKey :: MasterKey -> Options -> IO ()
cmdDumpKey key opts
    | optMaster opts = do
        putStrLn "Master key"
        putStrLn $ bsToString $ xPrvExport $ runMasterKey key
    | otherwise      = do
        unless (isJust accPrvM) $ error $
            "Index produced an invalid account: " ++ (show $ optAccount opts)
        putStrLn $ "Account: " ++ (show $ optAccount opts)
        putStrLn $ bsToString $ xPrvExport accPrv
        putStrLn $ bsToString $ xPubExport accPub
    where accPrvM = accPrvKey key (fromIntegral $ optAccount opts)
          accPrv  = runAccPrvKey $ fromJust $ accPrvM
          accPub  = deriveXPubKey accPrv

cmdDumpWIF :: MasterKey -> Options -> IO ()
cmdDumpWIF key opts = do
    let accM     = accPrvKey key (fromIntegral $ optAccount opts)
    unless (isJust accM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    let acc   = fromJust $ accM
        f     = if optInternal opts then intPrvKey else extPrvKey
        addrM = f acc (fromIntegral $ optIndex opts)
    unless (isJust addrM) $ error $
        "Index produced an invalid address: " ++ (show $ optIndex opts)
    when (optInternal opts) $ putStr "(Internal Chain) "
    putStr $ "Account: " ++ (show $ optAccount opts) ++ ", "
    putStrLn $ "Key Index: " ++ (show $ optIndex opts)
    putStrLn $ bsToString $ xPrvWIF $ runAddrPrvKey $ fromJust addrM 

cmdFingerprint :: MasterKey -> Options -> IO ()
cmdFingerprint key opts
    | optMaster opts = do
         putStrLn "Master Key"
         let masterFP = bsToHex $ encode' $ xPrvFP $ runMasterKey key
             masterID = bsToHex $ encode' $ xPrvID $ runMasterKey key
         putStrLn $ "fingerprint: " ++ (bsToString masterFP)
         putStrLn $ "ID: " ++ (bsToString masterID)
    | otherwise = do
        let accM     = accPrvKey key (fromIntegral $ optAccount opts)
        unless (isJust accM) $ error $
            "Index produced an invalid account: " ++ (show $ optAccount opts)
        let acc      = fromJust accM
            accFP    = bsToHex $ encode' $ xPrvFP $ runAccPrvKey acc
            accID    = bsToHex $ encode' $ xPrvID $ runAccPrvKey acc
        putStrLn $ "Account private key: " ++ (show $ optAccount opts)
        putStrLn $ "fingerprint: " ++ (bsToString accFP)
        putStrLn $ "ID: " ++ (bsToString accID)

cmdGetAddr :: MasterKey -> Options -> IO ()
cmdGetAddr key opts = do
    let accM = accPubKey key (fromIntegral $ optAccount opts)
    unless (isJust accM) $ error $
        "Index produced an invalid account: " ++ (show $ optAccount opts)
    let f   = if optInternal opts then intPubKeys else extPubKeys
        acc = fromJust accM
        ps  = take (optCount opts) $ f acc (fromIntegral $ optIndex opts)
        as  = map (addrToBase58 . addr) ps
        beg = xPubChild $ runAddrPubKey $ head $ ps
        end = xPubChild $ runAddrPubKey $ last $ ps
    when (optInternal opts) $ putStr "(Internal Chain) "
    putStr $ "Account: " ++ (show $ optAccount opts) ++ ", "
    putStr $ "First index: " ++ (show beg) ++ ", "
    putStrLn $ "Last index: " ++ (show end)
    forM_ as (putStrLn . bsToString)

cmdInit :: FilePath -> IO ()
cmdInit dir = do
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
    writeFile keyFile (bsToString key ++ "\n")
    setFileMode keyFile $ unionFileModes ownerReadMode ownerWriteMode
    putStrLn $ "Wallet initialized in: " ++ dir ++ "/key"

