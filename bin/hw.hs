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
import Data.Word
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

data Options = Options
    { optCount    :: Int
    , optIndex    :: Word32
    , optInternal :: Bool
    , optAccount  :: Int
    , optRequire  :: Int
    , optMaster   :: Bool
    , optHelp     :: Bool
    , optVersion  :: Bool
    } deriving (Eq, Show)

defaultOptions = Options
    { optCount    = 5
    , optIndex    = 0
    , optInternal = False
    , optAccount  = 0
    , optRequire  = 2
    , optMaster   = False
    , optHelp     = False
    , optVersion  = False
    } 

data Command = CmdInit
             | CmdAddress
             | CmdFingerprint
             | CmdDumpKey
             | CmdDumpWIF
             | CmdDecodeTx
             deriving (Eq, Show)

strToCmd :: String -> Command
strToCmd str = case str of
    "init"        -> CmdInit
    "address"     -> CmdAddress
    "fingerprint" -> CmdFingerprint
    "dumpkey"     -> CmdDumpKey
    "dumpwif"     -> CmdDumpWIF
    "decodetx"    -> CmdDecodeTx
    _ -> error $ "Invalid command: " ++ str

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Address generation count. Implies address command"
    , Option ['i'] ["index"] (ReqArg parseIndex "INT") $
        "Address key index. Implies address ot dumpwif command"
    , Option ['N'] ["internal"]
        (NoArg $ \opts -> return opts{ optInternal = True }) $
        "Use internal address chain. Implies address or dumpwif command"
    , Option ['a'] ["account"] (ReqArg parseAccount "INT") $
        "Account index to use in your command"
    , Option ['r'] ["require"] (ReqArg parseRequire "INT") $
        "Number of required keys (M) when generating M of N addresses"
    , Option ['m'] ["master"]
        (NoArg $ \opts -> return opts{ optMaster = True }) $
        "Use the master key. Implies dumpkey or fingerprint command"
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

parseIndex :: String -> Options -> IO Options
parseIndex s opts 
    | res >= 0 && res < 0x80000000 = return opts{ optIndex = res }
    | otherwise = error $ "Invalid index option: " ++ s
    where res = read s

parseAccount :: String -> Options -> IO Options
parseAccount s opts 
    | res >= 0 && res < 0x80000000 = return opts{ optAccount = res }
    | otherwise = error $ "Invalid account option: " ++ s
    where res = read s

parseRequire :: String -> Options -> IO Options
parseRequire s opts 
    | res >= 1 && res <= 16 = return opts{ optRequire = res }
    | otherwise = error $ "Invalid require option (between 1 and 16): " ++ s
    where res = read s

usageHeader :: String
usageHeader = "Usage: hw [<options>] <command> [<args>]"

cmdHelp :: String
cmdHelp = 
    "Valid <command>: \n" 
 ++ "  init             Initialize a new wallet (seeding from /dev/urandom)\n" 
 ++ "  address [<keys>] Prints a list of addresses. <keys> are in xpub format\n" 
 ++ "  fingerprint      Prints key fingerprint and ID\n"
 ++ "  dumpkey          Dump master or account keys to stdout\n"
 ++ "  dumpwif          Dump private keys in WIF format to stdout\n"
 ++ "  decodetx <tx>    Decode a transaction in HEX format"

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
            opts <- foldl (>>=) (return defaultOptions) o
            process opts n
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
    let keyM = loadMasterKey =<< (xPrvImport keyString) 
    unless (isJust keyM) $ error $
        "Failed to parse master key from file: " ++ keyFile
    return $ fromJust keyM
    where rstrip = reverse . dropWhile isSpace . reverse

process :: Options -> [String] -> IO ()
process opts cs 
    -- -h and -v can be called without a command
    | optHelp opts = putStrLn usage
    | optVersion opts = putStrLn versionMsg
    -- otherwise require a command
    | null cs = error usage
    | otherwise = do
        dir <- getWorkDir
        let (c,args) = (strToCmd $ head cs, tail cs)
        case c of
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
                cmdAddress key opts args
            CmdDecodeTx -> do
                cmdDecodeTx opts args
            
cmdDumpKey :: MasterKey -> Options -> IO ()
cmdDumpKey key opts
    | optMaster opts = do
        putStrLn "Master key"
        putStrLn $ xPrvExport $ runMasterKey key
    | otherwise      = do
        unless (isJust accPrvM) $ error $
            "Index produced an invalid account: " ++ (show $ optAccount opts)
        putStrLn $ "Account: " ++ (show $ optAccount opts)
        putStrLn $ xPrvExport accPrv
        putStrLn $ xPubExport accPub
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
    putStrLn $ xPrvWIF $ runAddrPrvKey $ fromJust addrM 

cmdFingerprint :: MasterKey -> Options -> IO ()
cmdFingerprint key opts
    | optMaster opts = do
         putStrLn "Master Key"
         let masterFP = bsToHex $ encode' $ xPrvFP $ runMasterKey key
             masterID = bsToHex $ encode' $ xPrvID $ runMasterKey key
         putStrLn $ "fingerprint: " ++ masterFP
         putStrLn $ "ID: " ++ masterID
    | otherwise = do
        let accM     = accPrvKey key (fromIntegral $ optAccount opts)
        unless (isJust accM) $ error $
            "Index produced an invalid account: " ++ (show $ optAccount opts)
        let acc      = fromJust accM
            accFP    = bsToHex $ encode' $ xPrvFP $ runAccPrvKey acc
            accID    = bsToHex $ encode' $ xPrvID $ runAccPrvKey acc
        putStrLn $ "Account private key: " ++ (show $ optAccount opts)
        putStrLn $ "fingerprint: " ++ accFP
        putStrLn $ "ID: " ++ accID

parseKey :: FilePath -> IO XPubKey
parseKey f = do
    exists <- fileExist f 
    unless exists $ error $ "File does not exist: " ++ f
    keyString <- rstrip <$> readFile f
    let keyM = xPubImport keyString
    unless (isJust keyM) $ error $
        "Failed to parse multisig key from file: " ++ f
    return $ fromJust keyM
    where rstrip = reverse . dropWhile isSpace . reverse

cmdAddress :: MasterKey -> Options -> [String] -> IO ()
cmdAddress key opts args = do
    when (optInternal opts) $ putStr "(Internal Chain) "
    addr <- (take $ optCount opts) <$> if null args
        then return $ f1 acc $ optIndex opts
        else do
            let k = map (fromMaybe (error $ "Invalid key")) keys
            putStr $ "(" ++ (show r) ++ " of " ++ 
                (show $ length k + 1) ++ " multisig) "
            return $ f2 acc k r $ optIndex opts
    putStrLn $ "Account " ++ (show ai)
    forM_ addr (\(s,i) -> putStrLn $ (show i) ++ ") " ++ s )
    where ai  = fromIntegral $ optAccount opts
          acc = fromMaybe (error $ "Invalid account index") $ accPubKey key ai
          r   = optRequire opts
          f1  = if (optInternal opts) then intAddrs else extAddrs
          f2  = if (optInternal opts) then intMulSigAddrs else extMulSigAddrs
          keys = map xPubImport args

cmdDecodeTx :: Options -> [String] -> IO ()
cmdDecodeTx opts args
    | null args = error usage
    | isNothing bs = error "Invalid HEX encoding"
    | otherwise = case eitherTx of
        Left err -> putStrLn err
        Right tx -> pp tx
    where bs       = hexToBS $ head args
          eitherTx = decodeToEither $ fromJust bs :: Either String Tx
        
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
    writeFile keyFile $ key ++ "\n"
    setFileMode keyFile $ unionFileModes ownerReadMode ownerWriteMode
    putStrLn $ "Wallet initialized in: " ++ dir ++ "/key"

