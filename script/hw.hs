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

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, forM, forM_, when, liftM, unless, mzero)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Exception (tryJust, throwIO)
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

import Data.Word (Word32, Word64)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, fromJust, isNothing, isJust, fromMaybe)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T (pack, unpack, splitOn)
import qualified Data.Yaml as YAML (encode)
import Data.Conduit.TMChan
import Data.Conduit 
    ( Sink
    , awaitForever
    , ($$) 
    )
import Data.Aeson (Value (Null), (.=), object, toJSON, decode)
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

import Network.Haskoin.Node.PeerManager

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Coin
import Network.Haskoin.Wallet.Tx

import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto 
import Network.Haskoin.Transaction
import Network.Haskoin.Util

data Options = Options
    { optCount    :: Int
    , optSigHash  :: SigHash
    , optFee      :: Word64
    , optJson     :: Bool
    , optHelp     :: Bool
    , optVersion  :: Bool
    , optPass     :: String
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optCount    = 5
    , optSigHash  = SigAll False
    , optFee      = 10000
    , optJson     = False
    , optHelp     = False
    , optVersion  = False
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
        "Format result as JSON (default: YAML)"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> return opts{ optHelp = True }) $
        "Display this help message"
    , Option ['v'] ["version"]
        (NoArg $ \opts -> return opts{ optVersion = True }) $
        "Show version information"
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
    [ "Wallet commands:" 
    , "  newwallet name [mnemonic]         Create a new wallet"
    , "  walletlist                        List all wallets"
    , ""
    , "Account commands:" 
    , "  newacc    wallet name             Create a new account"
    , "  newms     wallet name M N [pubkey...]"
    , "                                    Create a new multisig account"
    , "  addkeys   acc {pubkey...}         Add pubkeys to a multisig account"
    , "  acclist                           List all accounts"
    , "  accinfo   acc                     Display account information"
    , "  dumpkeys  acc                     Dump account keys to stdout"
    , ""
    , "Address commands:" 
    , "  new       acc {labels...}         Generate address with labels"
    , "  genaddr   acc [-c count]          Generate new addresses"
    , "  list      acc                     Display last page of addresses"
    , "  page      acc page [-c res/page]  Display addresses by page"
    , "  label     acc index label         Add a label to an address"
    , "  wif       acc index               Dump prvkey as WIF to stdout"
    , ""
    , "Transaction commands:" 
    , "  tx        acc                     Display transactions"
    , "  send      acc addr amount         Send coins to an address"
    , "  sendmany  acc {addr:amount...}    Send coins to many addresses"
    , "  balance   acc                     Display account balance"
    , "  balances                          Display all balances"
    , "  signtx    acc tx                  Sign a transaction"
    , "  importtx  tx                      Import offline transaction"
    , "  removetx  txid                    Remove transaction"
    , "  coins     acc                     List coins"
    , "  allcoins                          List all coins per account"
    , ""
    , "Utility commands: "
    , "  daemon                            Run haskoin as an SPV node"
    , "  decodetx   tx                     Decode HEX transaction"
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
    return dir

catchEx :: IOError -> Maybe String
catchEx = return . ioeGetErrorString

runDB :: ConnectionPool -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB pool m = runNoLoggingT $ runResourceT $ runSqlPool m pool

process :: Options -> [String] -> IO ()
process opts xs 
    -- -h and -v can be called without a command
    | optHelp opts = formatStr usage
    | optVersion opts = print haskoinUserAgent
    -- otherwise require a command
    | null xs = formatStr usage
    | otherwise = getWorkDir >>= \dir -> do
        let (cmd,args) = (head xs, tail xs)
            walletFile = T.pack $ concat [dir, "/wallet"]
            headerFile = concat [dir, "/headerchain"]

        pool <- createSqlitePool walletFile 10
        runDB pool $ runMigrationSilent migrateWallet >> initWalletDB

        if cmd == "daemon" 
            then do
                (eChan, rChan) <- startNode headerFile 
                bloom <- runDB pool walletBloomFilter
                atomically $ writeTBMChan rChan $ BloomFilterUpdate bloom
                sourceTBMChan eChan $$ processNodeEvents pool
            else do
                valE <- tryJust catchEx $ runDB pool $ 
                            dispatchCommand cmd opts args 

                -- TODO: Handle the exceptions
                when (isRight valE) $ do
                    let val = fromRight valE
                    if val == Null then return () else if optJson opts 
                        then formatStr $ bsToString $ toStrictBS $ 
                            JSON.encodePretty' 
                            JSON.defConfig{ JSON.confIndent = 2 } val
                        else formatStr $ bsToString $ YAML.encode val

processNodeEvents :: ConnectionPool -> Sink NodeEvent IO ()
processNodeEvents pool = awaitForever $ \e -> lift $ runDB pool $ case e of
    MerkleBlockEvent xs -> void $ importBlocks xs
    TxEvent tx          -> void $ importTx tx False

type Command m = m Value
type Args = [String]

whenArgs :: (MonadLogger m, MonadIO m)
         => Args -> (Int -> Bool) -> Command m -> Command m
whenArgs args f cmd 
    | f $ length args = cmd
    | otherwise = liftIO $ throwIO $
        InvalidCommandException "Invalid number of arguments"

-- TODO: Split this in individual functions ?
dispatchCommand :: ( MonadLogger m
                   , PersistStore m
                   , PersistUnique m
                   , PersistQuery m
                   , PersistMonadBackend m ~ SqlBackend
                   ) 
                => String -> Options -> Args -> Command m
dispatchCommand cmd opts args = case cmd of
    "newwallet" -> whenArgs args (<= 2) $ do
        ms <- newWalletMnemo 
                (head args) (optPass opts) (listToMaybe $ drop 1 args)
        return $ object ["Seed" .= ms]
    "walletlist" -> whenArgs args (== 0) $ do
        ws <- walletList
        return $ toJSON $ map yamlWallet ws
    "list" -> whenArgs args (== 1) $ do
        (as, m) <- addressPage (head args) 0 (optCount opts)
        return $ yamlAddrList as m (optCount opts) m
    "page" -> whenArgs args (== 2) $ do
        let pageNum = read $ args !! 1
        (as, m) <- addressPage (head args) pageNum (optCount opts)
        return $ yamlAddrList as pageNum (optCount opts) m
    "new" -> whenArgs args (>= 2) $ do
        let labels = drop 1 args
        addrs  <- newAddrs (head args) $ length labels
        lAddrs <- forM (zip labels addrs) $ \(l,a) -> 
            addressLabel (head args) (dbAddressIndex a) l
        return $ toJSON $ map yamlAddr lAddrs
    "genaddr" -> whenArgs args (== 1) $ do
        addrs <- newAddrs (head args) (optCount opts)
        return $ toJSON $ map yamlAddr addrs
    "label" -> whenArgs args (== 3) $ do
        a <- addressLabel (head args) (read $ args !! 1) (args !! 2)
        return $ yamlAddr a
    "balance" -> whenArgs args (== 1) $ do
        bal <- balance $ head args
        return $ object [ "Balance" .= bal ]
    "balances" -> whenArgs args (== 0) $ do
        accs <- accountList
        bals <- forM accs $ \a -> balance $ dbAccountName a
        return $ toJSON $ flip map (zip accs bals) $ \(a, b) -> object
            [ "Account" .= (dbAccountName a)
            , "Balance" .= b
            ]
    "tx" -> whenArgs args (== 1) $ do
        accTxs <- txList $ head args
        return $ toJSON $ map yamlTx accTxs
    "send" -> whenArgs args (== 3) $ do
        (tx, status) <- 
            sendTx (head args) [(args !! 1, read $ args !! 2)] (optFee opts)
        return $ object [ "Tx" .= bsToHex (encode' tx)
                        , "Status" .= status
                        ]
    "sendmany" -> whenArgs args (>= 2) $ do
        let f [a,b] = (T.unpack a,read $ T.unpack b)
            f _     = error "sendmany: Invalid format addr:amount"
            dests   = map (f . (T.splitOn (T.pack ":")) . T.pack) $ drop 1 args
        (tx, status) <- sendTx (head args) dests (optFee opts)
        return $ object [ "Tx" .= bsToHex (encode' tx)
                        , "Status" .= status
                        ]
    "newacc" -> whenArgs args (== 2) $ do
        acc <- newAccount (head args) (args !! 1)
        setLookAhead (head args) 30 
        return $ yamlAcc acc
    "newms" -> whenArgs args (>= 4) $ do
        let keysM = mapM xPubImport $ drop 4 args
            keys  = fromJust keysM
        when (isNothing keysM) $ liftIO $ throwIO $ 
            ParsingException "Could not parse keys"
        let wname = head args
            name  = args !! 1
            m     = read $ args !! 2
            n     = read $ args !! 3
        acc <- newMSAccount wname name m n keys
        when (length (dbAccountMsKeys acc) == n - 1) $ do
            setLookAhead name 30 
        return $ yamlAcc acc
    "addkeys" -> whenArgs args (>= 2) $ do
        let keysM = mapM xPubImport $ drop 1 args
            keys  = fromJust keysM
        when (isNothing keysM) $ liftIO $ throwIO $
            ParsingException "Could not parse keys"
        acc <- addAccountKeys (head args) keys
        let n = fromJust $ dbAccountMsTotal acc
        when (length (dbAccountMsKeys acc) == n - 1) $ do
            setLookAhead (head args) 30 
        return $ yamlAcc acc
    "accinfo" -> whenArgs args (== 1) $ do
        acc <- getAccount $ head args
        return $ yamlAcc acc
    "acclist" -> whenArgs args (== 0) $ do
        accs <- accountList 
        return $ toJSON $ map yamlAcc accs
    "dumpkeys" -> whenArgs args (== 1) $ do
        acc <- getAccount $ head args
        prv <- accountPrvKey $ head args
        let ms = if isMSAccount acc then Just $ dbAccountMsKeys acc else Nothing
        return $ object $
            [ "Account" .= yamlAcc acc
            , "PubKey" .= (xPubExport $ getAccPubKey $ dbAccountKey acc)
            , "PrvKey" .= (xPrvExport $ getAccPrvKey prv)
            ] ++ maybe [] (\ks -> [ "MSKeys" .= map xPubExport ks ]) ms
    "wif" -> whenArgs args (== 2) $ do
        prvKey <- addressPrvKey (head args) (read $ args !! 1)
        return $ object [ "WIF" .= T.pack (toWIF prvKey) ]
    "coins" -> whenArgs args (== 1) $ do
        coins <- unspentCoins $ head args
        return $ toJSON $ map yamlCoin coins
    "allcoins" -> whenArgs args (== 0) $ do
        accs <- accountList 
        coins <- forM accs $ \a -> unspentCoins $ dbAccountName a
        return $ toJSON $ flip map (zip accs coins) $ \(acc, cs) -> object
            [ "Account" .= dbAccountName acc
            , "Coins" .= map yamlCoin cs
            ]
    "signtx" -> whenArgs args (== 2) $ do
        let txM = decodeToMaybe =<< (hexToBS $ args !! 1)
            tx  = fromJust txM
        when (isNothing txM) $ liftIO $ throwIO $
            ParsingException "Could not parse transaction"
        (t, s) <- signWalletTx (head args) tx (optSigHash opts)
        return $ object [ "Tx"       .= bsToHex (encode' t)
                        , "Status"   .= s
                        ]
    "importtx" -> whenArgs args (== 1) $ do
        let txM = decodeToMaybe =<< (hexToBS $ head args)
            tx  = fromJust txM
        when (isNothing txM) $ liftIO $ throwIO $
            ParsingException "Could not parse transaction"
        accTxs <- importTx tx True
        return $ toJSON $ map yamlTx accTxs
    "removetx" -> whenArgs args (== 1) $ do
        let idM = decodeTxHashLE $ head args
        when (isNothing idM) $ liftIO $ throwIO $
            ParsingException "Could not parse transaction id"
        hashes <- removeTx $ fromJust idM
        return $ toJSON $ map encodeTxHashLE hashes
    "decodetx" -> whenArgs args (== 1) $ do
        tx <- decodeRawTx $ head args
        return $ toJSON tx
    "buildrawtx" -> whenArgs args (== 2) $ do
        tx <- buildRawTx (head args) (args !! 1)
        return $ object [ "Tx" .= bsToHex (encode' tx) ]
    "signrawtx"    -> whenArgs args (== 3) $ do 
        let txM = decodeToMaybe =<< (hexToBS $ head args)
            tx  = fromJust txM
        when (isNothing txM) $ liftIO $ throwIO $ 
            ParsingException "Could not parse transaction"
        (t, s) <- signRawTx tx (args !! 1) (args !! 2) (optSigHash opts)
        return $ object [ "Tx"     .= bsToHex (encode' t)
                        , "Status" .= s
                        ]
    _ -> do
        liftIO $ throwIO $ InvalidCommandException $ 
            unwords ["Invalid command:", cmd]

{- Utility Commands -}

-- | Decodes a transaction, providing structural information on the inputs
-- and the outputs of the transaction.
decodeRawTx :: MonadIO m
            => String  -- ^ HEX encoded transaction
            -> m Tx    -- ^ Decoded transaction
decodeRawTx str 
    | isJust txM = return tx
    | otherwise  = liftIO $ throwIO $
        ParsingException "Could not parse transaction"
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
            TransactionBuildingException $ fromLeft txE
        return tx
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
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
            TransactionSigningException $ fromLeft resE
        return $ fromRight resE
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
  where
    fsM   = decode $ toLazyBS $ stringToBS strSigi
    keysM = decode $ toLazyBS $ stringToBS strKeys
    (RawSigInput fs) = fromJust fsM
    (RawPrvKey keys) = fromJust keysM

{- Instances for JSON -}

yamlAcc :: DbAccountGeneric b -> Value
yamlAcc acc = object $ concat
    [ [ "Name" .= dbAccountName acc
      , "Tree" .= dbAccountTree acc
      ]
    , datType, datWarn
    ]
    where msReq = fromJust $ dbAccountMsRequired acc
          msTot = fromJust $ dbAccountMsTotal acc
          ms    = unwords [show msReq,"of",show msTot]
          miss  = msTot - length (dbAccountMsKeys acc) - 1
          datType | isMSAccount acc = ["Type" .= unwords [ "Multisig", ms ]]
                  | otherwise   = ["Type" .= ("Regular" :: String)]
          datWarn | isMSAccount acc && miss > 0 =
                      [ "Warning" .=
                        unwords [show miss,"multisig keys missing"]
                      ]
                  | otherwise = []

yamlWallet :: DbWalletGeneric b -> Value
yamlWallet w = object $ 
    [ "Name" .= dbWalletName w
    , "Type" .= dbWalletType w
    ]

yamlAddr :: DbAddressGeneric b -> Value
yamlAddr a
    | null $ dbAddressLabel a = object base
    | otherwise = object $ label:base
  where 
    base  = [ "Addr" .= (addrToBase58 $ dbAddressValue a)
            , "Key"  .= dbAddressIndex a
            , "Tree" .= dbAddressTree a
            ]
    label = "Label" .= dbAddressLabel a

yamlAddrList :: [DbAddressGeneric b] -> Int -> Int -> Int -> Value
yamlAddrList addrs pageNum resPerPage totPages = object
    [ "Addresses" .= (toJSON $ map yamlAddr addrs)
    , "Page results" .= object
        [ "Current page"     .= pageNum
        , "Total pages"      .= totPages
        , "Results per page" .= resPerPage
        ]
    ]

-- TODO: Change this to an instance
yamlTx :: AccTx -> Value
yamlTx accTx = object $ concat
    [ [ "Recipients" .= accTxRecipients accTx
      , "Value" .= accTxValue accTx
      , "Confirmations" .= accTxConfirmations accTx
      ]
    , if accTxOffline accTx then ["Offline" .= True] else []
    ]

yamlCoin :: DbCoinGeneric b -> Value
yamlCoin coin = object $ concat
    [ [ "TxID"    .= (encodeTxHashLE $ dbCoinHash coin)
      , "Index"   .= dbCoinPos coin
      , "Value"   .= (coinValue $ dbCoinValue coin)
      , "Script"  .= (coinScript $ dbCoinValue coin)
      , "Address" .= dbCoinAddress coin
      ] 
    , if isJust $ coinRedeem $ dbCoinValue coin 
        then ["Redeem" .= fromJust (coinRedeem $ dbCoinValue coin)] 
        else []
    ]

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

instance ToJSON CoinStatus where
    toJSON (Spent tid) = 
        object [ "Status".= String "Spent"
               , "Txid"  .= encodeTxHashLE tid
               ]
    toJSON (Reserved tid) = 
        object [ "Status".= String "Reserved"
               , "Txid"  .= encodeTxHashLE tid
               ]
    toJSON Unspent = object [ "Status".= String "Unspent" ]

instance FromJSON CoinStatus where
    parseJSON (Object obj) = obj .: "Status" >>= \status -> case status of
        (String "Spent")    -> 
            (Spent . fromJust . decodeTxHashLE)    <$> obj .: "Txid"
        (String "Reserved") -> 
            (Reserved . fromJust . decodeTxHashLE) <$> obj .: "Txid"
        (String "Unspent")  -> return Unspent
        _                   -> mzero
    parseJSON _ = mzero

instance ToJSON WalletType where
    toJSON WalletFull = String "WalletFull"
    toJSON WalletRead = String "WalletRead"

instance FromJSON WalletType where
    parseJSON (String "WalletFull")  = return WalletFull
    parseJSON (String "WalletRead")  = return WalletRead
    parseJSON _ = mzero

instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object
        [ "TxID" .= encodeTxHashLE h
        , "Index" .= toJSON i
        ]

instance ToJSON TxOut where
    toJSON (TxOut v s) = object $
        [ "Value" .= v
        , "Raw Script" .= bsToHex s
        , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
        ] ++ scptPair 
      where scptPair = 
              either (const [])
                     (\out -> ["Decoded Script" .= out]) 
                     (decodeOutputBS s)

instance ToJSON TxIn where
    toJSON (TxIn o s i) = object $ concat
        [ [ "OutPoint" .= o
          , "Sequence" .= i
          , "Raw Script" .= bsToHex s
          , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
          ] 
          , decoded
        ]
      where 
        decoded = either (const []) f $ decodeInputBS s
        f inp = ["Decoded Script" .= inp]
              
instance ToJSON Tx where
    toJSON tx@(Tx v is os i) = object
        [ "TxID" .= encodeTxHashLE (txHash tx)
        , "Version" .= v
        , "Inputs" .= map input (zip is [0..])
        , "Outputs" .= map output (zip os [0..])
        , "LockTime" .= i
        ]
      where input (x,j) = object 
              [T.pack ("Input " ++ show (j :: Int)) .= x]
            output (x,j) = object 
              [T.pack ("Output " ++ show (j :: Int)) .= x]

instance ToJSON Script where
    toJSON (Script ops) = toJSON $ map f ops
      where
        f (OP_PUSHDATA bs _) = String $ T.pack $ unwords 
            ["OP_PUSHDATA", bsToHex bs]
        f x = String $ T.pack $ show x

instance ToJSON ScriptOutput where
    toJSON (PayPK p) = object 
        [ "PayToPublicKey" .= object [ "Public Key" .= bsToHex (encode' p) ] ]
    toJSON (PayPKHash a) = object 
        [ "PayToPublicKeyHash" .= object
            [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
            , "Address Base58" .= addrToBase58 a
            ]
        ]
    toJSON (PayMulSig ks r) = object 
        [ "PayToMultiSig" .= object
            [ "Required Keys (M)" .= toJSON r
            , "Public Keys" .= map (bsToHex . encode') ks
            ]
        ]
    toJSON (PayScriptHash a) = object 
        [ "PayToScriptHash" .= object
            [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
            , "Address Base58" .= addrToBase58 a
            ]
        ]

instance ToJSON ScriptInput where
    toJSON (RegularInput (SpendPK s)) = object 
        [ "SpendPublicKey" .= object [ "Signature" .= s ] ]
    toJSON (RegularInput (SpendPKHash s p)) = object 
        [ "SpendPublicKeyHash" .= object
            [ "Signature" .= s
            , "Public Key" .= bsToHex (encode' p)
            , "Sender Addr" .= addrToBase58 (pubKeyAddr p)
            ]
        ]
    toJSON (RegularInput (SpendMulSig sigs)) = object 
        [ "SpendMultiSig" .= object
            [ "Signatures" .= sigs ]
        ]
    toJSON (ScriptHashInput s r) = object
        [ "SpendScriptHash" .= object
            [ "ScriptInput" .= (RegularInput s)
            , "RedeemScript" .= r
            , "Raw Redeem Script" .= bsToHex (encodeOutputBS r)
            , "Sender Addr" .= addrToBase58 (scriptAddr  r)
            ]
        ]

instance ToJSON TxSignature where
    toJSON ts@(TxSignature _ h) = object
        [ "Raw Sig" .= bsToHex (encodeSig ts)
        , "SigHash" .= h
        ]

instance ToJSON SigHash where
    toJSON sh = case sh of
        (SigAll acp) -> object
            [ "Type" .= String "SigAll"
            , "AnyoneCanPay" .= acp
            ]
        (SigNone acp) -> object
            [ "Type" .= String "SigNone"
            , "AnyoneCanPay" .= acp
            ]
        (SigSingle acp) -> object
            [ "Type" .= String "SigSingle"
            , "AnyoneCanPay" .= acp
            ]
        (SigUnknown acp v) -> object
            [ "Type" .= String "SigUnknown"
            , "AnyoneCanPay" .= acp
            , "Value" .= v
            ]

