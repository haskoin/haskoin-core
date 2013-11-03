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
import Data.List.Split
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
    (_,_,msgs) -> formatStr $ unlines $ msgs ++ [usage]

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
    | optVersion opts = formatStr versionMsg
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
            "label"        -> cmdLabel args
            "balance"      -> cmdBalance opts args
            "totalbalance" -> cmdTotalBalance opts args
            "focus"        -> cmdFocus opts args
            "newacc"       -> cmdNewAcc opts args
            "newms"        -> cmdNewMS opts args
            "listacc"      -> cmdListAcc 
            "dumpkey"      -> cmdDumpKey args
            "importtx"     -> cmdImportTx args
            "coins"        -> cmdCoins opts args
            "allcoins"     -> cmdAllCoins opts 
            "decodetx"     -> cmdDecodeTx opts args
            "buildtx"      -> cmdBuildTx opts args
            "signtx"       -> cmdSignTx opts args
            _              -> left $ unwords ["Invalid command:", c]
        case res of
            Left  err -> formatStr err
            Right str -> formatStr str

type Args = [String]
type CmdAction = WalletDB (ResourceT IO) String

checkInit :: String -> WalletDB (ResourceT IO) ()
checkInit str 
    -- Commands that can be called without an initialized database
    | str `elem` [ "init", "decodetx", "buildtx" ] = return ()
    | otherwise = dbExists "config" >>= \exists -> if exists
        then return ()
        else left "Wallet not initialized. Call init first"

-- Return the account from the arguments, or get the current focused account
focusedAcc :: Args -> WalletDB (ResourceT IO) DBAccount
focusedAcc args = case args of
    []      -> (dbGetAcc . AccPos) =<< dbGetConfig cfgFocus
    name:[] -> dbGetAcc $ AccName name

formatAddr :: DBAddress -> String
formatAddr addr = unwords $ [ (show $ addrPos addr) ++ ")"
                            , addrBase58 addr
                            ] ++ label
    where label | null $ addrLabel addr = [] 
                | otherwise             = ["(", addrLabel addr, ")"]

formatAcc :: DBAccount -> String
formatAcc acc
    | isMSAcc acc = unwords [ "[", "MultiSig Account"
                            , show $ msReq acc, "of" 
                            , (show $ length (msKeys acc) + 1) 
                            , "]", name
                            ]
    | otherwise   = unwords [ "[ Regular Account ]", name ]
    where name = accName $ runAccData acc

formatPage :: Int -> Int -> DBAccount -> String
formatPage from count acc = 
    unwords [ formatAcc acc
            , "(" , "Addresses", a, "to", b, "of", c , ")"
            ]
    where a = (show from) 
          b = (show $ from + count - 1)
          c = (show $ accExtCount $ runAccData acc )

formatCoin :: DBCoin -> DBAccount -> String
formatCoin (DBCoin (OutPoint tid i) (TxOut v s) _ _ p) acc = 
    unlines 
        [ "{ TxID  : " ++ (show tid) 
        , "  Index : " ++ (show i) 
        , "  Value : " ++ (show v) 
        , "  Script: " ++ (bsToHex $ encodeScriptOps s) 
        , "  Addr  : " ++ fAddr
        , "  Acc   : " ++ (accName $ runAccData acc)
        , "}"
        ]
    where fAddr = either (const "-") addrToBase58 $ scriptRecipient s

cmdInit :: Options -> Args -> CmdAction
cmdInit opts args = if length args /= 1 then left usage else do
    dbInit $ head args
    cmdGenAddr opts [] -- generate some addresses

cmdList :: Options -> Args -> CmdAction
cmdList opts args = if length args > 1 then left usage else do
    acc <- focusedAcc args
    let total = accExtCount $ runAccData acc
        count = min (optCount opts) total
        from  = total - count + 1
    addr <- dbAddrList (accPos $ runAccData acc) from count False
    return $ if null addr
        then unlines [formatAcc acc, "No addresses to display"]
        else unlines $ (formatPage from (length addr) acc):(map formatAddr addr)

cmdListFrom :: Options -> Args -> CmdAction
cmdListFrom opts args = if length args > 2 then left usage else do
    acc <- focusedAcc $ drop 1 args
    let from = read $ args !! 0
    addr <- dbAddrList (accPos $ runAccData acc) from (optCount opts) False
    return $ if null addr
        then unlines [formatAcc acc, "No addresses to display"]
        else unlines $ (formatPage from (length addr) acc):(map formatAddr addr)

cmdListAll :: Options -> Args -> CmdAction
cmdListAll opts args = if length args > 1 then left usage else do
    acc <- focusedAcc args
    let aData = runAccData acc
    addr <- dbAddrList (accPos aData) 1 (accExtCount aData) False
    return $ if null addr
        then unlines [formatAcc acc, "No addresses to display"]
        else unlines $ (formatPage 1 (length addr) acc):(map formatAddr addr)

cmdNew :: Options -> Args -> CmdAction
cmdNew opts args = if length args > 2 then left usage else do
    acc  <- focusedAcc $ drop 1 args
    addr <- head <$> (dbGenAddr (accPos $ runAccData acc) 1 False)
    let newAddr = addr{ addrLabel = args !! 0 }
    dbPutAddr newAddr
    return $ unlines [formatAcc acc, formatAddr newAddr]

cmdGenAddr :: Options -> Args -> CmdAction
cmdGenAddr opts args = if length args > 1 then left usage else do
    acc  <- focusedAcc args
    addr <- dbGenAddr (accPos $ runAccData acc) (optCount opts) False
    cmdList opts args

cmdFocus :: Options -> Args -> CmdAction
cmdFocus opts args = if length args /= 1 then left usage else do
    acc <- dbGetAcc $ AccName $ head args
    dbPutConfig $ \cfg -> cfg{ cfgFocus = accPos $ runAccData acc }
    cmdList opts args

cmdNewAcc :: Options -> Args -> CmdAction
cmdNewAcc opts args = if length args /= 1 then left usage else do
    dbNewAcc $ head args
    cmdGenAddr opts args

cmdNewMS :: Options -> Args -> CmdAction
cmdNewMS opts args = if length args < 3 then left usage else do
    keys <- mapM ((liftMaybe errKey) . xPubImport) $ drop 2 args
    dbNewMSAcc (head args) (read $ args !! 1) keys
    cmdGenAddr opts [head args]
    where errKey = "cmdNewMS: Error importing extended public key"

cmdListAcc :: CmdAction
cmdListAcc = unlines . (map (unwords . f)) <$> dbAccList 
    where f acc = [ formatAcc acc
                  , "(", show $ accExtCount $ runAccData acc
                  , "addresses"
                  , ")"
                  ]

cmdLabel :: Args -> CmdAction
cmdLabel args = if length args > 3 then left usage else do
    acc  <- focusedAcc $ drop 2 args
    addr <- dbGetAddr $ AddrExt (accPos $ runAccData acc) (read $ head args)
    let newAddr = addr{ addrLabel = args !! 1 }
    dbPutAddr newAddr
    return $ unlines [formatAcc acc, formatAddr newAddr]

cmdBalance :: Options -> Args -> CmdAction
cmdBalance opts args = if length args > 1 then left usage else do
    acc   <- focusedAcc args
    coins <- dbCoinList $ accPos $ runAccData acc
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins
    return $ unlines [formatAcc acc, unwords ["Balance:", show balance]]

cmdTotalBalance :: Options -> Args -> CmdAction
cmdTotalBalance opts args = do
    coins <- dbCoinListAll 
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins
    return $ unwords ["Total Balance:", show balance]

cmdDumpKey :: Args -> CmdAction
cmdDumpKey args = if length args > 1 then left usage else do
    acc <- focusedAcc args
    return $ unlines [ formatAcc acc
                     , xPubExport $ runAccPubKey $ accKey $ runAccData acc
                     ]

cmdImportTx :: Args -> CmdAction
cmdImportTx args = if length args /= 1 then left usage else do
    tx    <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args)
    coins <- dbImportTx tx
    if null coins then return "No coins imported" else unlines <$> forM coins f
    where txErr = "cmdImportTx: Could not decode transaction"
          f c = (formatCoin c) <$> (dbGetAcc $ AccPos $ coinAccPos c)

cmdCoins :: Options -> Args -> CmdAction
cmdCoins opts args = if length args > 1 then left usage else do
    acc   <- focusedAcc args
    coins <- dbCoinList $ accPos $ runAccData acc
    return $ unlines $ (formatAcc acc): 
        if null coins then ["No coins"] else map (flip formatCoin acc) coins

cmdAllCoins :: Options -> CmdAction
cmdAllCoins opts = do
    coins <- dbCoinListAll
    if null coins then return "No coins" else unlines <$> forM coins f
    where f c = (formatCoin c) <$> (dbGetAcc $ AccPos $ coinAccPos c)

cmdDecodeTx :: Options -> Args -> CmdAction
cmdDecodeTx opts args = if length args /= 1 then left usage else do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args) 
    return $ ppShow (tx :: Tx)
    where txErr = "cmdDecodeTx: Could not decode transaction"

cmdBuildTx :: Options -> Args -> CmdAction
cmdBuildTx opts args = if length args < 2 then left usage else do
    ops <- mapM f os
    ads <- mapM g as
    tx  <- liftEither $ buildAddrTx ops ads
    return $ bsToHex $ encode' tx
    where xs      = map (splitOn ":") args
          (os,as) = span ((== 64) . length . head) xs
          f [t,i] = do
            tid <- liftMaybe tidErr $ (decodeToMaybe . BS.reverse) =<< hexToBS t
            return $ OutPoint tid $ read i
          f _     = left "Invalid syntax for txid:index"
          g [a,v] = return (a,read v)
          g _     = left "Invalid syntax for address:amount"
          tidErr  = "cmdBuildTx: Could not decode outpoint txid"

cmdSignTx :: Options -> Args -> CmdAction
cmdSignTx opts args = if length args < 2 then left usage else do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args)
    ys <- mapRights f (map (splitOn ":") $ tail args)
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    return $ show $ (bsToHex . encode') <$> sigTx
    where f [t,i,s] = do
            sBS <- liftMaybe "Invalid script HEX encoding" $ hexToBS s
            tBS <- liftMaybe "Invalid txid HEX encoding" $ hexToBS t
            scp <- liftEither $ decodeScriptOps sBS
            tid <- liftEither $ decodeToEither $ BS.reverse tBS
            dbGetSigData scp (OutPoint tid $ read i) $ optSigHash opts
          f _ = left "Invalid syntax for txid:index:script"
          txErr = "cmdSignTx: Could not decode transaction"

