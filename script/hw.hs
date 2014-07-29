{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    )
import System.IO.Error (ioeGetErrorString)
import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    , ArgOrder (Permute)
    )
import qualified System.Environment as E (getArgs)
import System.Posix.Daemon

import Control.Applicative ((<$>), (<*>))
import Control.Monad 
    ( join
    , void
    , forM
    , forM_
    , when
    , liftM
    , unless
    , mzero
    , liftM2
    )
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Exception (tryJust, throwIO, throw)
import Control.Monad.Logger 
    ( NoLoggingT
    , runNoLoggingT
    , LoggingT
    , MonadLogger
    , runStderrLoggingT
    )
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM

import Database.Persist
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , entityVal
    )
import Database.Persist.Sql 
    ( SqlBackend
    , ConnectionPool
    , SqlPersistT
    , runSqlPool
    , runMigrationSilent
    )
import Database.Persist.Sqlite (createSqlitePool)

import Data.Default
import Data.Word (Word32, Word64)
import Data.List (sortBy, intersperse)
import Data.Maybe (listToMaybe, fromJust, isNothing, isJust, fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T (pack, unpack, splitOn)
import qualified Data.Yaml as YAML (encode)
import Data.Conduit.TMChan
import Data.Conduit 
    ( Sink
    , awaitForever
    , ($$), (=$) 
    , yield
    , await
    )
import Data.Conduit.Network 
    ( ClientSettings(..)
    , runTCPClient
    , clientSettings
    , appSink
    , appSource
    , AppData
    )
import qualified Data.Conduit.Binary as CB
import Data.Aeson (Value (Null), (.=), object, toJSON, decode, encode)
import Data.Aeson.Types
    ( Value (Object, String)
    , FromJSON
    , ToJSON
    , Parser
    , (.=)
    , (.:)
    , (.:?)
    , object
    , parseJSON
    , toJSON
    , withArray
    , withObject
    )
import qualified Data.Aeson.Encode.Pretty as JSON
    ( encodePretty'
    , defConfig
    , confIndent
    )

import Network.Haskoin.Server
import Network.Haskoin.Server.Types
import Network.Haskoin.Node.PeerManager

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Tx

import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto 
import Network.Haskoin.Transaction
import Network.Haskoin.Stratum 
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
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optFee      = 10000
    , optJson     = False
    , optYaml     = False
    , optPass     = ""
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
        "Format result as JSON"
    , Option ['y'] ["yaml"]
        (NoArg $ \opts -> return opts{ optYaml = True }) $
        "Format result as YAML"
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
    , "  start                             Start the haskoin daemon"
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
    , "  addkeys   acc {pubkey...}         Add pubkeys to a multisig account"
    , "  acclist                           List all accounts"
    , "  getacc    acc                     Display an account by name"
    , ""
    , "Address commands:" 
    , "  new       acc labels              Generate an address with a label"
    , "  genaddr   acc [-c count]          Generate new addresses"
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
    , "Utility commands: "
    , "  (disable) decodetx   tx           Decode HEX transaction"
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

printJSONOr :: Options 
            -> Either String WalletResponse 
            -> (WalletResponse -> IO ()) 
            -> IO ()
printJSONOr opts resE def
    | isLeft resE = error $ fromLeft resE
    | optJson opts = do
        let bs = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } res
        formatStr $ bsToString $ toStrictBS bs
    | optYaml opts = formatStr $ bsToString $ YAML.encode res
    | otherwise = def res
  where
    res = fromRight resE

-- TODO: Rename existing log files to prevent overriding them
processCommand :: Options -> Args -> IO ()
processCommand opts args = getWorkDir >>= \dir -> case args of
    ["start"] -> do
        runDetached (Just $ pidFile dir) (ToFile $ logFile dir) runServer
        putStrLn "Haskoin daemon started"
        putStrLn $ unwords [ "Configuration file:", configFile dir ]
    ["stop"] -> do
        -- TODO: Should we send a message instead of killing the process ?
        killAndWait $ pidFile dir
        putStrLn "Haskoin daemon stopped"
    "newwallet" : name : mnemonic -> do
        let req = case mnemonic of
                []  -> NewFullWallet name (optPass opts) Nothing
                [m] -> NewFullWallet name (optPass opts) (Just m)
                _   -> error invalidErr
        res <- sendRequest req 
        printJSONOr opts res $ \r -> case r of
            ResMnemonic m -> do
                putStrLn "Write down your seed:"
                putStrLn m
            _ -> error "Received an invalid response"
    ["getwallet", name] -> do
        res <- sendRequest $ GetWallet name
        printJSONOr opts res $ \r -> case r of
            ResWallet w -> putStr $ printWallet w
            _ -> error "Received an invalid response"
    ["walletlist"] -> do
        res <- sendRequest WalletList
        printJSONOr opts res $ \r -> case r of
            ResWalletList ws -> do
                let xs = map (putStr . printWallet) ws
                sequence_ $ intersperse (putStrLn "-") xs
            _ -> error "Received an invalid response"
    ["newacc", wname, name] -> do
        res <- sendRequest $ NewAccount wname name
        printJSONOr opts res $ \r -> case r of
            ResAccount a -> putStr $ printAccount a
            _ -> error "Received an invalid response"
    "newms" : wname : name : r : t : ks -> do
        let keysM = mapM xPubImport ks
            r'    = read r
            t'    = read t
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        res <- sendRequest $ NewMSAccount wname name r' t' $ fromJust keysM
        printJSONOr opts res $ \r -> case r of
            ResAccount a -> putStr $ printAccount a
            _ -> error "Received an invalid response"
    "addkeys" : name : ks -> do
        let keysM = mapM xPubImport ks
        when (isNothing keysM) $ throwIO $ 
            WalletException "Could not parse keys"
        res <- sendRequest $ AddAccountKeys name $ fromJust keysM
        printJSONOr opts res $ \r -> case r of
            ResAccount a -> putStr $ printAccount a
            _ -> error "Received an invalid response"
    ["getacc", name] -> do
        res <- sendRequest $ GetAccount name
        printJSONOr opts res $ \r -> case r of
            ResAccount a -> putStr $ printAccount a
            _ -> error "Received an invalid response"
    ["acclist"] -> do
        res <- sendRequest AccountList
        printJSONOr opts res $ \r -> case r of
            ResAccountList as -> do
                let xs = map (putStr . printAccount) as
                sequence_ $ intersperse (putStrLn "-") xs
            _ -> error "Received an invalid response"
    ["new", name, label] -> do
        addE <- sendRequest $ GenAddress name 1
        case addE of
            Right (ResAddressList (a:_)) -> do
                res <- sendRequest $ AddressLabel name (addressIndex a) label
                printJSONOr opts res $ \r -> case r of
                    ResAddress a -> putStrLn $ printAddress a
                    _ -> error "Received an invalid response"
            Right _ -> throwIO $ WalletException "Invalid response"
            Left err -> throwIO $ WalletException err
    ["genaddr", name] -> do
        res <- sendRequest $ GenAddress name $ optCount opts
        printJSONOr opts res $ \r -> case r of
            ResAddressList as -> forM_ as $ putStrLn . printAddress
            _ -> error "Received an invalid response"
    ["list", name] -> do
        res <- sendRequest $ AddressList name
        printJSONOr opts res $ \r -> case r of
            ResAddressList as -> forM_ as $ putStrLn . printAddress
            _ -> error "Received an invalid response"
    ["page", name, page] -> do
        let p = read page
        res <- sendRequest $ AddressPage name p $ optCount opts
        printJSONOr opts res $ \r -> case r of
            ResAddressPage as m -> do
                -- page 0 is the last page
                let x = if p == 0 then m else p
                putStrLn $ unwords [ "Page", show x, "of", show m ]
                forM_ as $ putStrLn . printAddress
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
            ResTxStatus h c -> do
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
            ResTxStatus h c -> do
                putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
            _ -> error "Received an invalid response"
    ["signtx", name, tx] -> do
        let txM = decodeToMaybe =<< hexToBS tx
        when (isNothing txM) $ throwIO $
            WalletException "Could not parse transaction"
        res <- sendRequest $ TxSign name $ fromJust txM
        printJSONOr opts res $ \r -> case r of
            ResTxStatus h c -> do
                putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
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
    "rescan" : timestamp -> do
        let t = read <$> listToMaybe timestamp
        res <- sendRequest $ Rescan t
        printJSONOr opts res $ \r -> case r of
            ResRescan t -> putStrLn $ unwords [ "Timestamp:", show t]
            _ -> error "Received an invalid response"
    [] -> formatStr usage
    ["help"] -> formatStr usage
    ["version"] -> putStrLn haskoinUserAgent
    _ -> error invalidErr
  where
    pidFile dir    = concat [dir, "/hwnode.pid"]
    logFile dir    = concat [dir, "/stdout.log"]
    configFile dir = concat [dir, "/config"]

sendRequest :: WalletRequest -> IO (Either String WalletResponse)
sendRequest req = runTCPClient (clientSettings 4000 "127.0.0.1") go
  where
    source = CB.sourceLbs $ toLazyBS $ encodeWalletRequest req 0
    go server = do
        source $$ appSink server
        appSource server $$ decodeResult req

decodeResult :: Monad m 
             => WalletRequest
             -> Sink BS.ByteString m (Either String WalletResponse)
decodeResult req = do
    bs <- await 
    if isJust bs
        then return $ join $ fst <$> decodeWalletResponse req (fromJust bs) 
        else decodeResult req

{- Utility Commands -}

-- | Decodes a transaction, providing structural information on the inputs
-- and the outputs of the transaction.
decodeRawTx :: MonadIO m
            => String  -- ^ HEX encoded transaction
            -> m Tx    -- ^ Decoded transaction
decodeRawTx str 
    | isJust txM = return tx
    | otherwise  = liftIO $ throwIO $
        WalletException "Could not parse transaction"
  where 
    txM = decodeToMaybe =<< (hexToBS str)
    tx  = fromJust txM

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

-- instance ToJSON CoinStatus where
--     toJSON (Spent tid) = 
--         object [ "Status".= String "Spent"
--                , "Txid"  .= encodeTxHashLE tid
--                ]
--     toJSON (Reserved tid) = 
--         object [ "Status".= String "Reserved"
--                , "Txid"  .= encodeTxHashLE tid
--                ]
--     toJSON Unspent = object [ "Status".= String "Unspent" ]
-- 
-- instance FromJSON CoinStatus where
--     parseJSON (Object obj) = obj .: "Status" >>= \status -> case status of
--         (String "Spent")    -> 
--             (Spent . fromJust . decodeTxHashLE)    <$> obj .: "Txid"
--         (String "Reserved") -> 
--             (Reserved . fromJust . decodeTxHashLE) <$> obj .: "Txid"
--         (String "Unspent")  -> return Unspent
--         _                   -> mzero
--     parseJSON _ = mzero
-- 
-- instance ToJSON OutPoint where
--     toJSON (OutPoint h i) = object
--         [ "TxID" .= encodeTxHashLE h
--         , "Index" .= toJSON i
--         ]
-- 
-- instance ToJSON TxOut where
--     toJSON (TxOut v s) = object $
--         [ "Value" .= v
--         , "Raw Script" .= bsToHex s
--         , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
--         ] ++ scptPair 
--       where scptPair = 
--               either (const [])
--                      (\out -> ["Decoded Script" .= out]) 
--                      (decodeOutputBS s)
-- 
-- instance ToJSON TxIn where
--     toJSON (TxIn o s i) = object $ concat
--         [ [ "OutPoint" .= o
--           , "Sequence" .= i
--           , "Raw Script" .= bsToHex s
--           , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
--           ] 
--           , decoded
--         ]
--       where 
--         decoded = either (const []) f $ decodeInputBS s
--         f inp = ["Decoded Script" .= inp]
--               
-- instance ToJSON Tx where
--     toJSON tx@(Tx v is os i) = object
--         [ "TxID" .= encodeTxHashLE (txHash tx)
--         , "Version" .= v
--         , "Inputs" .= map input (zip is [0..])
--         , "Outputs" .= map output (zip os [0..])
--         , "LockTime" .= i
--         ]
--       where input (x,j) = object 
--               [T.pack ("Input " ++ show (j :: Int)) .= x]
--             output (x,j) = object 
--               [T.pack ("Output " ++ show (j :: Int)) .= x]
-- 
-- instance ToJSON Script where
--     toJSON (Script ops) = toJSON $ map f ops
--       where
--         f (OP_PUSHDATA bs _) = String $ T.pack $ unwords 
--             ["OP_PUSHDATA", bsToHex bs]
--         f x = String $ T.pack $ show x
-- 
-- instance ToJSON ScriptOutput where
--     toJSON (PayPK p) = object 
--         [ "PayToPublicKey" .= object [ "Public Key" .= bsToHex (encode' p) ] ]
--     toJSON (PayPKHash a) = object 
--         [ "PayToPublicKeyHash" .= object
--             [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
--             , "Address Base58" .= addrToBase58 a
--             ]
--         ]
--     toJSON (PayMulSig ks r) = object 
--         [ "PayToMultiSig" .= object
--             [ "Required Keys (M)" .= toJSON r
--             , "Public Keys" .= map (bsToHex . encode') ks
--             ]
--         ]
--     toJSON (PayScriptHash a) = object 
--         [ "PayToScriptHash" .= object
--             [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
--             , "Address Base58" .= addrToBase58 a
--             ]
--         ]
-- 
-- instance ToJSON ScriptInput where
--     toJSON (RegularInput (SpendPK s)) = object 
--         [ "SpendPublicKey" .= object [ "Signature" .= s ] ]
--     toJSON (RegularInput (SpendPKHash s p)) = object 
--         [ "SpendPublicKeyHash" .= object
--             [ "Signature" .= s
--             , "Public Key" .= bsToHex (encode' p)
--             , "Sender Addr" .= addrToBase58 (pubKeyAddr p)
--             ]
--         ]
--     toJSON (RegularInput (SpendMulSig sigs)) = object 
--         [ "SpendMultiSig" .= object
--             [ "Signatures" .= sigs ]
--         ]
--     toJSON (ScriptHashInput s r) = object
--         [ "SpendScriptHash" .= object
--             [ "ScriptInput" .= (RegularInput s)
--             , "RedeemScript" .= r
--             , "Raw Redeem Script" .= bsToHex (encodeOutputBS r)
--             , "Sender Addr" .= addrToBase58 (scriptAddr  r)
--             ]
--         ]
-- 
-- instance ToJSON TxSignature where
--     toJSON ts@(TxSignature _ h) = object
--         [ "Raw Sig" .= bsToHex (encodeSig ts)
--         , "SigHash" .= h
--         ]
-- 
-- instance ToJSON SigHash where
--     toJSON sh = case sh of
--         (SigAll acp) -> object
--             [ "Type" .= String "SigAll"
--             , "AnyoneCanPay" .= acp
--             ]
--         (SigNone acp) -> object
--             [ "Type" .= String "SigNone"
--             , "AnyoneCanPay" .= acp
--             ]
--         (SigSingle acp) -> object
--             [ "Type" .= String "SigSingle"
--             , "AnyoneCanPay" .= acp
--             ]
--         (SigUnknown acp v) -> object
--             [ "Type" .= String "SigUnknown"
--             , "AnyoneCanPay" .= acp
--             , "Value" .= v
--             ]
--
