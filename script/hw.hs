{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import System.Directory (removeFile, doesFileExist)
import System.IO.Error (ioeGetErrorString)
import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    , ArgOrder (Permute)
    )
import qualified System.Environment as E (getArgs)
import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, forM_, when, mzero, liftM2)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (throwIO, throw)

import Data.Default (def)
import Data.String (fromString)
import Data.Word (Word32, Word64)
import Data.List (intersperse)
import Data.Maybe (listToMaybe, fromJust, isNothing, isJust, fromMaybe)
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Vector as V (toList)
import qualified Data.Text as T (pack, unpack, splitOn)
import qualified Data.Yaml as YAML (encode)
import Data.Conduit (($$), ($=))
import Data.Conduit.Network (clientSettings)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Conduit.List as CL
import Data.Aeson 
    ( Value (String)
    , FromJSON
    , ToJSON
    , object
    , toJSON
    , encode
    , decode
    , withObject
    , withArray
    , eitherDecode
    , (.=), (.:), (.:?)
    )
import Data.Aeson.Types
    ( Parser
    , parseJSON
    )
import qualified Data.Aeson.Encode.Pretty as JSON
    ( encodePretty'
    , defConfig
    , confIndent
    )

import Network.HTTP.Conduit 
    ( RequestBody(..)
    , Request(..)
    , Response(..)
    , httpLbs
    , parseUrl
    , withManager
    , setQueryString
    )

import Network.Haskoin.REST
import Network.Haskoin.REST.Types

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto 
import Network.Haskoin.Transaction
import Network.Haskoin.Util
import Network.Haskoin.Constants

type Args = [String]

data Options = Options
    { optCount    :: Int
    , optSigHash  :: SigHash
    , optFee      :: Word64
    , optJson     :: Bool
    , optYaml     :: Bool
    , optPass     :: String
    , optDetach   :: Bool
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optFee      = 10000
    , optJson     = False
    , optYaml     = False
    , optPass     = ""
    , optDetach   = False
    } 

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["count"] (ReqArg parseCount "INT") $
        "Count: see commands for details"
    , Option ['s'] ["sighash"] (ReqArg parseSigHash "SIGHASH") $
        "(disabled) Signature type = ALL|NONE|SINGLE"
    , Option ['a'] ["anyonecanpay"]
        (NoArg $ \opts -> do
            let sh = optSigHash opts
            return opts{ optSigHash = sh{ anyoneCanPay = True } }
        ) $ "(disabled) Set signature flag AnyoneCanPay"
    , Option ['f'] ["fee"] (ReqArg parseCount "INT") $
        "Transaction fee (default: 10000)"
    , Option ['j'] ["json"]
        (NoArg $ \opts -> return opts{ optJson = True }) $
        "Format result as JSON"
    , Option ['y'] ["yaml"]
        (NoArg $ \opts -> return opts{ optYaml = True }) $
        "Format result as YAML"
    , Option ['d'] ["detach"]
        (NoArg $ \opts -> return opts{ optDetach = True }) $
        "Detach the process from the terminal"
    , Option ['p'] ["passphrase"]
        (ReqArg (\s opts -> return opts{ optPass = s }) "PASSPHRASE") $
        "Optional Passphrase for mnemonic"
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
    [ "Server commands:" 
    , "  start [--detach]                  Start the haskoin daemon"
    , "  stop                              Stop the haskoin daemon"
    , ""
    , "Wallet commands:" 
    , "  newwallet name [mnemonic]         Create a new wallet"
    , "  getwallet name                    Display a wallet by name"
    , "  walletlist                        List all wallets"
    , "  rescan [timestamp]                Rescan the wallet"
    , ""
    , "Account commands:" 
    , "  newacc    wallet name             Create a new account"
    , "  newms     wallet name M N [pubkey...]"
    , "                                    Create a new multisig account"
    , "  newread   name pubkey             Create a new read-only account"
    , "  newreadms name [pubkey...]        Create a new read-only ms account"
    , "  addkeys   acc  {pubkey...}        Add pubkeys to a multisig account"
    , "  acclist                           List all accounts"
    , "  getacc    acc                     Display an account by name"
    , ""
    , "Address commands:" 
    , "  new       acc labels              Generate an address with a label"
    , "  (disabled) genaddr   acc [-c count]          Generate new addresses"
    , "  list      acc                     Display all addresses of an account"
    , "  page      acc page [-c addr/page] Display addresses by page"
    , "  label     acc index label         Add a label to an address"
    , ""
    , "Transaction commands:" 
    , "  txlist    acc                     Display transactions in an account"
    , "  txpage    acc page [-c tx/page]   Display transactions by page"
    , "  send      acc addr amount         Send coins to an address"
    , "  sendmany  acc {addr:amount...}    Send coins to many addresses"
    , "  signtx    acc tx                  Sign a transaction"
    , "  gettx     hash                    Get a raw transaction"
    , "  balance   acc                     Display account balance"
    , "  (disabled) coins     acc          List coins"
    , ""
    , "Offline tx commands:" 
    , "  getblob   acc txhash              Get data to sign a tx offline"
    , "  signblob  acc blob                Sign an offline tx"
    , ""
    , "Utility commands: "
    , "  decodetx  tx                      Decode HEX transaction"
    , "  (disabled) buildrawtx"
    , "      '[{\"txid\":txid,\"vout\":n},...]' '{addr:amnt,...}'"
    , "  (disabled) signrawtx "  
    , "      tx" 
    , "      " ++ sigdata
    , "      '[prvkey,...]' [-s SigHash]" 
    , ""
    , "Other commands: "
    , "  version                           Display version information"
    , "  help                              Display this help information"
    ]
  where 
    sigdata = concat
        [ "'[{"
        , "\"txid\":txid,"
        , "\"vout\":n,"
        , "\"scriptPubKey\":hex,"
        , "\"redeemScript\":hex"
        , "},...]'"
        ]

warningMsg :: String
warningMsg = unwords [ "***"
                     , "This software is experimental."
                     , "Use only small amounts of Bitcoins"
                     , "***"
                     ]

usage :: String
usage = unlines $ [warningMsg, usageInfo usageHeader options] ++ cmdHelp

main :: IO ()
main = E.getArgs >>= \args -> case getOpt Permute options args of
    (o,xs,[]) -> do
        opts <- foldl (>>=) (return defaultOptions) o
        processCommand opts xs
    (_,_,msgs) -> print $ unlines $ msgs ++ [usage]

catchEx :: IOError -> Maybe String
catchEx = return . ioeGetErrorString

invalidErr :: String
invalidErr = "Invalid request"

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

printJSONOr :: (FromJSON a, ToJSON a)
            => Options 
            -> BL.ByteString
            -> (a -> IO ()) 
            -> IO ()
printJSONOr opts bs action
    | isLeft resE = error $ fromLeft resE
    | optJson opts = do
        let bs = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } res
        formatStr $ bsToString $ toStrictBS bs
    | optYaml opts = formatStr $ bsToString $ YAML.encode res
    | otherwise = action res
  where
    resE = eitherDecode bs
    res  = fromRight resE

-- TODO: Rename existing log files to prevent overriding them
processCommand :: Options -> Args -> IO ()
processCommand opts args = getWorkDir >>= \dir -> case args of
    ["start"] -> do
        prevLog <- doesFileExist $ logFile dir
        -- TODO: Should we move the log file to an archive directory?
        when prevLog $ removeFile $ logFile dir
        if optDetach opts
            then runDetached 
                    (Just $ pidFile dir) (ToFile $ logFile dir) runServer
            else runServer
        putStrLn "Haskoin daemon started"
        putStrLn $ unwords [ "Configuration file:", configFile dir ]
    ["stop"] -> do
        -- TODO: Should we send a message instead of killing the process ?
        killAndWait $ pidFile dir
        putStrLn "Haskoin daemon stopped"
    "newwallet" : name : mnemonic -> do
        let req = Just $ encode $ case mnemonic of
                []  -> NewWallet name (optPass opts) Nothing
                [m] -> NewWallet name (optPass opts) (Just m)
                _   -> error invalidErr
        res <- sendRequest "/api/wallets" "POST" [] req
        printJSONOr opts res $ \(MnemonicRes m) -> do
            putStrLn "Write down your seed:"
            putStrLn m
    ["getwallet", name] -> do
        let url = stringToBS $ concat [ "/api/wallets/", name ]
        res <- sendRequest url "GET" [] Nothing
        printJSONOr opts res $ putStr . printWallet
    ["walletlist"] -> do
        res <- sendRequest "/api/wallets" "GET" [] Nothing
        printJSONOr opts res $ \ws -> do
            let xs = map (putStr . printWallet) ws
            sequence_ $ intersperse (putStrLn "-") xs
    ["newacc", wname, name] -> do
        let req = Just $ encode $ NewAccount wname name
        res <- sendRequest "/api/accounts" "POST" [] req
        printJSONOr opts res $ putStr . printAccount
    "newms" : wname : name : m : n : ks -> do
        let keysM = mapM xPubImport ks
            m'    = read m
            n'    = read n
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let req = Just $ encode $ NewMSAccount wname name m' n' $ fromJust keysM
        res <- sendRequest "/api/accounts" "POST" [] req
        printJSONOr opts res $ putStr . printAccount
    ["newread", name, key] -> do
        let keyM = xPubImport key
        when (isNothing keyM) $ throwIO $ 
            WalletException "Could not parse key"
        let req = Just $ encode $ NewReadAccount name $ fromJust keyM
        res <- sendRequest "/api/accounts" "POST" [] req
        printJSONOr opts res $ putStr . printAccount
    "newreadms" : name : m : n : ks -> do
        let keysM = mapM xPubImport ks
            m'    = read m
            n'    = read n
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let req = Just $ encode $ NewReadMSAccount name m' n' $ fromJust keysM
        res <- sendRequest "/api/accounts" "POST" [] req
        printJSONOr opts res $ putStr . printAccount
    "addkeys" : name : ks -> do
        let keysM = mapM xPubImport ks
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        let req = encode <$> keysM
            url = stringToBS $ concat [ "/api/accounts/", name, "/keys" ]
        res <- sendRequest url "POST" [] req
        printJSONOr opts res $ putStr . printAccount
    ["getacc", name] -> do
        let url = stringToBS $ concat [ "/api/accounts/", name ]
        res <- sendRequest url "GET" [] Nothing
        printJSONOr opts res $ putStr . printAccount
    ["acclist"] -> do
        res <- sendRequest "/api/accounts" "GET" [] Nothing
        printJSONOr opts res $ \as -> do
            let xs = map (putStr . printAccount) as
            sequence_ $ intersperse (putStrLn "-") xs
    ["list", name] -> do
        let url = stringToBS $ concat [ "/api/accounts/", name, "/addresses" ]
        res <- sendRequest url "GET" [] Nothing
        printJSONOr opts res $ mapM_ (putStrLn . printAddress)
    ["page", name, page] -> do
        let p   = read page :: Int
            url = stringToBS $ concat [ "/api/accounts/", name, "/addresses" ]
            qs  = [ ("page",Just $ stringToBS $ show p)
                  , ("elemperpage",Just $ stringToBS $ show $ optCount opts)
                  ]
        res <- sendRequest url "GET" qs Nothing
        printJSONOr opts res $ \(AddressPageRes as m) -> do
            -- page 0 is the last page
            let x = if p == 0 then m else p
            putStrLn $ unwords [ "Page", show x, "of", show m ]
            forM_ as $ putStrLn . printAddress
    ["new", name, label] -> do
        let url = stringToBS $ concat [ "/api/accounts/", name, "/addresses" ]
            req = Just $ encode $ AddressData label
        res <- sendRequest url "POST" [] req
        printJSONOr opts res $ putStrLn . printAddress
    {-
    ["genaddr", name] -> do
        res <- sendRequest $ GenAddress name $ optCount opts
        printJSONOr opts res $ \r -> case r of
            ResAddressList as -> forM_ as $ putStrLn . printAddress
            _ -> error "Received an invalid response"
    ["label", name, index, label] -> do
        let i = read index
        res <- sendRequest $ AddressLabel name i label
        printJSONOr opts res $ \r -> case r of
            ResAddress a -> putStrLn $ printAddress a
            _ -> error "Received an invalid response"
    ["txlist", name] -> do
        res <- sendRequest $ TxList name
        printJSONOr opts res $ \r -> case r of
            ResAccTxList ts -> do
                let xs = map (putStr . printAccTx) ts
                sequence_ $ intersperse (putStrLn "-") xs
            _ -> error "Received an invalid response"
    ["txpage", name, page] -> do
        let p = read page
        res <- sendRequest $ TxPage name p $ optCount opts 
        printJSONOr opts res $ \r -> case r of
            ResAccTxPage ts m -> do
                -- page 0 is the last page
                let x = if p == 0 then m else p
                putStrLn $ unwords [ "Page", show x, "of", show m ]
                let xs = map (putStr . printAccTx) ts
                sequence_ $ intersperse (putStrLn "-") xs
            _ -> error "Received an invalid response"
    ["send", name, add, amount] -> do
        let a = base58ToAddr add
            v = read amount
        when (isNothing a) $ throwIO $ 
            WalletException "Could not parse address"
        res <- sendRequest $ TxSend name [(fromJust a, v)] $ optFee opts
        printJSONOr opts res $ \r -> case r of
            ResTxHashStatus h c -> do
                putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
            _ -> error "Received an invalid response"
    "sendmany" : name : xs -> do
        let g str   = map T.unpack $ T.splitOn ":" (T.pack str)
            f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read v)
            f _     = throw $ WalletException "Could not parse recipient list"
            recipients = mapM (f . g) xs
        when (isNothing recipients) $ throwIO $
            WalletException "Could not parse recipient list"
        res <- sendRequest $ TxSend name (fromJust recipients) $ optFee opts
        printJSONOr opts res $ \r -> case r of
            ResTxHashStatus h c -> do
                putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
            _ -> error "Received an invalid response"
    ["signtx", name, tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        res <- sendRequest $ TxSign name $ fromJust txM
        printJSONOr opts res $ \r -> case r of
            ResTxHashStatus h c -> do
                putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
            _ -> error "Received an invalid response"
    ["getblob", name, tid] -> do
        let h = decodeTxHashLE tid
        when (isNothing h) $ throwIO $
            WalletException "Could not parse hash"
        res <- sendRequest $ GetSigBlob name $ fromJust h
        printJSONOr opts res $ \r -> case r of
            ResSigBlob blob -> putStrLn $ bsToHex $ toStrictBS $ encode blob
            _ -> error "Received an invalid response"
    ["signblob", name, blob] -> do
        let blobM = decode . toLazyBS =<< hexToBS blob
        when (isNothing blobM) $ throwIO $
            WalletException "Could not parse sig blob"
        res <- sendRequest $ SignSigBlob name $ fromJust blobM
        printJSONOr opts res $ \r -> case r of
            ResTxStatus tx c -> do
                putStrLn $ unwords [ "Tx  :", bsToHex $ encode' tx]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
            _ -> error "Received an invalid response"
    ["gettx", hash] -> do
        let h = decodeTxHashLE hash
        when (isNothing h) $ throwIO $
            WalletException "Could not parse hash"
        res <- sendRequest $ TxGet $ fromJust h
        printJSONOr opts res $ \r -> case r of
            ResTx t -> putStrLn $ bsToHex $ encode' t
            _ -> error "Received an invalid response"
    ["balance", name] -> do
        res <- sendRequest $ Balance name
        printJSONOr opts res $ \r -> case r of
            ResBalance b -> putStrLn $ unwords [ "Balance:", show b ]
            _ -> error "Received an invalid response"
    "rescan" : rescantime -> do
        let t = read <$> listToMaybe rescantime
        res <- sendRequest $ Rescan t
        printJSONOr opts res $ \r -> case r of
            ResRescan ts -> putStrLn $ unwords [ "Timestamp:", show ts]
            _ -> error "Received an invalid response"
    -}
    ["decodetx", tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        let res = encodeTxJSON $ fromJust txM
            bs = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } res
        if optJson opts
            then formatStr $ bsToString $ toStrictBS bs
            else formatStr $ bsToString $ YAML.encode res
    [] -> formatStr usage
    ["help"] -> formatStr usage
    ["version"] -> putStrLn haskoinUserAgent
    _ -> error invalidErr
  where
    pidFile dir    = concat [dir, "/hwnode.pid"]
    logFile dir    = concat [dir, "/stdout.log"]
    configFile dir = concat [dir, "/config"]

sendRequest :: BS.ByteString       -- Path
            -> BS.ByteString       -- Method
            -> [(BS.ByteString, Maybe BS.ByteString)] -- Query String
            -> Maybe BL.ByteString -- Body 
            -> IO BL.ByteString    -- Response
sendRequest p m qs bodyM = withManager $ \manager -> do
    let req = setQueryString qs $ def
                { host        = "localhost"
                , port        = 8555
                , path        = p
                , method      = m
                , requestHeaders = [("accept", "application/json")]
                }
    res <- flip httpLbs manager $ case bodyM of
        Just b -> req{ requestBody = RequestBodyLBS b }
        _      -> req
    return $ responseBody res

{-
sendRequest :: WalletRequest
            -> IO (Either String WalletResponse)
sendRequest req = do
    dir <- getWorkDir
    let configFile = concat [dir, "/config"]
    configExists <- doesFileExist configFile
    unless configExists $ throwIO $ WalletException $ unwords
        [ "Config file does not exist. Call 'hw start' to generate one in:"
        , configFile
        ]
    configM <- decodeFile configFile
    unless (isJust configM) $ throwIO $ WalletException $ unwords
        [ "Could node parse config file:"
        , configFile
        ]
    let host       = fromString $ configBind $ fromJust configM
        port       = configPort $ fromJust configM
    head <$> tcpClient V2 True (clientSettings port host) go
  where
    source = CL.sourceList [MsgRequest (buildRequest V2 req)]
    go :: AppConduits WalletRequest () () () () WalletResponse IO
       -> IO [Either String WalletResponse]
    go (src, snk) = do
        source $$ snk
        src $= CL.map f $$ CL.consume
    f (IncomingError (ErrorObj _ m _ _ _)) = Left m
    f (IncomingMsg (MsgResponse (Response _ rs _)) _) = Right rs
    f (IncomingMsg (MsgError (ErrorObj _ m _ _ _)) _) = Left m
    f _ = undefined
-}

encodeTxJSON :: Tx -> Value
encodeTxJSON tx@(Tx v is os i) = object
    [ "txid"     .= encodeTxHashLE (txHash tx)
    , "version"  .= v
    , "inputs"   .= map input (zip is [0..])
    , "outputs"  .= map output (zip os [0..])
    , "locktime" .= i
    ]
  where 
    input (x,j) = object 
      [T.pack ("input " ++ show (j :: Int)) .= encodeTxInJSON x]
    output (x,j) = object 
      [T.pack ("output " ++ show (j :: Int)) .= encodeTxOutJSON x]

encodeTxInJSON :: TxIn -> Value
encodeTxInJSON (TxIn o s i) = object $ concat 
    [ [ "outpoint"   .= encodeOutPointJSON o
      , "sequence"   .= i
      , "raw-script" .= bsToHex s
      , "script"     .= encodeScriptJSON sp
      ] 
      , decoded
    ]
  where 
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const []) f $ decodeInputBS s
    f inp = ["decoded-script" .= encodeScriptInputJSON inp]

encodeTxOutJSON :: TxOut -> Value
encodeTxOutJSON (TxOut v s) = object $
    [ "value"      .= v
    , "raw-script" .= bsToHex s
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded 
  where 
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const [])
                 (\out -> ["decoded-script" .= encodeScriptOutputJSON out]) 
                 (decodeOutputBS s)

encodeOutPointJSON :: OutPoint -> Value
encodeOutPointJSON (OutPoint h i) = object
    [ "txid" .= encodeTxHashLE h
    , "pos"  .= toJSON i
    ]

encodeScriptJSON :: Script -> Value
encodeScriptJSON (Script ops) = 
    toJSON $ map f ops
  where
    f (OP_PUSHDATA bs _) = String $ T.pack $ unwords 
        ["OP_PUSHDATA", bsToHex bs]
    f x = String $ T.pack $ show x

encodeScriptInputJSON :: ScriptInput -> Value
encodeScriptInputJSON si = case si of
    RegularInput (SpendPK s) -> object 
        [ "spendpubkey" .= object [ "sig" .= encodeSigJSON s ] ]
    RegularInput (SpendPKHash s p) -> object 
        [ "spendpubkeyhash" .= object
            [ "sig"            .= encodeSigJSON s
            , "pubkey"         .= bsToHex (encode' p)
            , "sender-address" .= addrToBase58 (pubKeyAddr p)
            ]
        ]
    RegularInput (SpendMulSig sigs) -> object 
        [ "spendmulsig" .= object [ "sigs" .= map encodeSigJSON sigs ] ]
    ScriptHashInput s r -> object
        [ "spendscripthash" .= object
            [ "scriptinput" .= encodeScriptInputJSON (RegularInput s)
            , "redeem" .= encodeScriptOutputJSON r
            , "raw-redeem" .= bsToHex (encodeOutputBS r)
            , "sender-address" .= addrToBase58 (scriptAddr r)
            ]
        ]

encodeScriptOutputJSON :: ScriptOutput -> Value
encodeScriptOutputJSON so = case so of
    PayPK p -> object
        [ "pay2pubkey" .= object [ "pubkey" .= bsToHex (encode' p) ] ]
    PayPKHash a -> object 
        [ "pay2pubkeyhash" .= object
            [ "address-base64" .= bsToHex (encode' $ getAddrHash a)
            , "address-base58" .= addrToBase58 a
            ]
        ]
    PayMulSig ks r -> object 
        [ "pay2mulsig" .= object
            [ "required-keys" .= r
            , "pubkeys"       .= map (bsToHex . encode') ks
            ]
        ]
    PayScriptHash a -> object 
        [ "pay2scripthash" .= object
            [ "address-base64" .= bsToHex (encode' $ getAddrHash a)
            , "address-base58" .= addrToBase58 a
            ]
        ]

encodeSigJSON :: TxSignature -> Value
encodeSigJSON ts@(TxSignature _ sh) = object
    [ "raw-sig" .= bsToHex (encodeSig ts)
    , "sighash" .= encodeSigHashJSON sh
    ]

encodeSigHashJSON :: SigHash -> Value
encodeSigHashJSON sh = case sh of
    SigAll acp -> object
        [ "type" .= String "SigAll"
        , "acp"  .= acp
        ]
    SigNone acp -> object
        [ "type" .= String "SigNone"
        , "acp"  .= acp
        ]
    SigSingle acp -> object
        [ "type" .= String "SigSingle"
        , "acp"  .= acp
        ]
    SigUnknown acp v -> object
        [ "type"  .= String "SigUnknown"
        , "acp"   .= acp
        , "value" .= v
        ]

{- Utility Commands -}

-- | Build a raw transaction from a list of outpoints and recipients encoded
-- in JSON.
--
-- Outpoint format as JSON:
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       },...
-- >   ] 
--
--  Recipient list as JSON:
--
-- >   { addr: amnt,... }
--
buildRawTx :: MonadIO m 
           => String  -- ^ List of JSON encoded Outpoints.
           -> String  -- ^ List of JSON encoded Recipients.
           -> m Tx    -- ^ Transaction result.
buildRawTx i o 
    | isJust opsM && isJust destsM = do
        when (isLeft txE) $ liftIO $ throwIO $
            WalletException $ fromLeft txE
        return tx
    | otherwise = liftIO $ throwIO $
        WalletException "Could not parse input values"
  where
    opsM   = decode $ toLazyBS $ stringToBS i
    destsM = decode $ toLazyBS $ stringToBS o
    (RawTxOutPoints ops) = fromJust opsM
    (RawTxDests dests)   = fromJust destsM
    txE = buildAddrTx ops dests
    tx  = fromRight txE

-- | Sign a raw transaction by providing the signing parameters and private
-- keys manually. None of the keys in the wallet will be used for signing.
--
-- Signing data as JSON (redeemScript is optional):
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       , "scriptPubKey": hex
-- >       , "redeemScript": hex
-- >       },...
-- >    ]
--
-- Private keys in JSON foramt:
--
-- >   [ WIF,... ]
signRawTx :: MonadIO m
          => Tx       -- ^ Transaction to sign.
          -> String   -- ^ List of JSON encoded signing parameters.
          -> String   -- ^ List of JSON encoded WIF private keys.
          -> SigHash  -- ^ Signature type. 
          -> m (Tx, Bool)
signRawTx tx strSigi strKeys sh 
    | isJust fsM && isJust keysM = do
        let resE = detSignTx tx (map (\f -> f sh) fs) keys
        when (isLeft resE) $ liftIO $ throwIO $
            WalletException $ fromLeft resE
        return $ fromRight resE
    | otherwise = liftIO $ throwIO $
        WalletException "Could not parse input values"
  where
    fsM   = decode $ toLazyBS $ stringToBS strSigi
    keysM = decode $ toLazyBS $ stringToBS strKeys
    (RawSigInput fs) = fromJust fsM
    (RawPrvKey keys) = fromJust keysM

data RawTxOutPoints = RawTxOutPoints [OutPoint] 
    deriving (Eq, Show)

instance FromJSON RawTxOutPoints where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawTxOutPoints <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            let i = maybeToEither ("Failed to decode txid" :: String)
                                  (decodeTxHashLE tid)
                o = OutPoint <$> i <*> (return vout)
            either (const mzero) return o

data RawTxDests = RawTxDests [(String,Word64)]
    deriving (Eq, Show)

instance FromJSON RawTxDests where
    parseJSON = withObject "Expected: Object" $ \obj ->
        RawTxDests <$> (mapM f $ H.toList obj)
      where
        f (add,v) = do
            amnt <- parseJSON v :: Parser Word64
            return (T.unpack add, amnt)

data RawSigInput = RawSigInput [(SigHash -> SigInput)]

instance FromJSON RawSigInput where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawSigInput <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            scp  <- obj .: "scriptPubKey" :: Parser String
            rdm  <- obj .:? "redeemScript" :: Parser (Maybe String)
            let s = decodeOutputBS =<< maybeToEither "Hex parsing failed" 
                        (hexToBS scp)
                i = maybeToEither "Failed to decode txid" (decodeTxHashLE tid)
                o = OutPoint <$> i <*> (return vout)
                r = decodeOutputBS =<< maybeToEither "Hex parsing failed" 
                        (hexToBS $ fromJust rdm)
                res | isJust rdm = 
                        (flip <$> (SigInput <$> s <*> o)) <*> (Just <$> r)
                    | otherwise  = 
                        (flip <$> (SigInput <$> s <*> o)) <*> (return Nothing)
            either (const mzero) return res

data RawPrvKey = RawPrvKey [PrvKey]
    deriving (Eq, Show)

instance FromJSON RawPrvKey where
    parseJSON = withArray "Expected: Array" $ \arr ->
        RawPrvKey <$> (mapM f $ V.toList arr)
      where
        f v = do
            str <- parseJSON v :: Parser String  
            maybe mzero return $ fromWIF str

