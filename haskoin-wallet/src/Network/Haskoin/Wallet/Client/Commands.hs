module Network.Haskoin.Wallet.Client.Commands
( cmdStart
, cmdStop
, cmdNewAcc
, cmdAddKey
, cmdSetGap
, cmdAccount
, cmdRenameAcc
, cmdAccounts
, cmdList
, cmdUnused
, cmdLabel
, cmdURI
, cmdTxs
, cmdAddrTxs
, cmdGetIndex
, cmdGenAddrs
, cmdSend
, cmdSendMany
, cmdImport
, cmdSign
, cmdBalance
, cmdGetTx
, cmdGetOffline
, cmdSignOffline
, cmdRescan
, cmdDecodeTx
, cmdVersion
, cmdStatus
, cmdBlockInfo
, cmdMonitor
, cmdSync
, cmdKeyPair
, cmdDeleteTx
, cmdPending
, cmdDead
, cmdDice
, decodeBase6
, diceToEntropy
, diceToMnemonic
, mixEntropy
)
where

import           Control.Concurrent.Async                (async, wait)
import           Control.Monad
import qualified Control.Monad.Reader                     as R
import           Control.Monad.Trans                      (liftIO)
import           Data.Aeson
import           Data.Bits                                (xor)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Base64                   as B64
import qualified Data.ByteString.Char8                    as B8
import           Data.Either                              (fromRight)
import           Data.List                                (intercalate,
                                                           intersperse)
import           Data.Maybe
import           Data.Monoid                              ((<>))
import           Data.Restricted                          (rvalue)
import qualified Data.Serialize                           as S
import           Data.String                              (fromString)
import           Data.String.Conversions                  (cs)
import qualified Data.Text                                as T
import qualified Data.Time.Format                         as Time
import           Data.Word                                (Word32, Word64)
import qualified Data.Yaml                                as YAML
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.STM
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Accounts          (rootToAccKey)
import qualified Network.Haskoin.Wallet.Client.PrettyJson as JSON
import           Network.Haskoin.Wallet.Server
import           Network.Haskoin.Wallet.Settings
import           Network.Haskoin.Wallet.Types
import qualified Network.URI.Encode                       as URI
import           Numeric                                  (readInt)
import qualified System.Console.Haskeline                 as Haskeline
import           System.Entropy                           (getEntropy)
import           System.IO                                (stderr)
import           System.ZMQ4
import           Text.Read                                (readMaybe)

type Handler = R.ReaderT Config IO

accountExists :: String -> Handler Bool
accountExists name = do
    resE <- sendZmq (AccountReq $ T.pack name)
    case (resE :: Either String (WalletResponse JsonAccount)) of
        Right (ResponseValid _) -> return True
        Right (ResponseError _) -> return False
        Left err                -> error err

accountKeyExists :: String -> Handler Bool
accountKeyExists name = do
    accM <- parseResponse <$> sendZmq (AccountReq $ T.pack name)
    return $ isJust $ jsonAccountMaster =<< accM

data ParsedKey = ParsedXPrvKey  !XPrvKey
               | ParsedXPubKey  !XPubKey
               | ParsedMnemonic !Mnemonic !Passphrase !XPrvKey
               | ParsedNothing

askMnemonicOrKey :: String -> Handler ParsedKey
askMnemonicOrKey msg =
    go . cs =<< askInput msg
  where
    go "" = return ParsedNothing
    go str = case xPrvImport str of
        Just k -> return $ ParsedXPrvKey k
        _ -> case xPubImport str of
            Just p -> return $ ParsedXPubKey p
            -- This first check is just to verify if the mnemonic parses
            _ -> case mnemonicToSeed "" str of
                Right _ -> do
                    pass <- cs <$> askPassword
                    let seed = fromRight (error "Could not decode mnemonic seed") $
                               mnemonicToSeed (cs pass) str
                    return $ ParsedMnemonic str pass (makeXPrvKey seed)
                _ -> error "Could not parse mnemonic or extended key"

askInput :: String -> Handler String
askInput msg = do
    inputM <- liftIO $
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getPassword (Just '*') msg
    return $ fromMaybe err inputM
  where
    err = error "No action due to EOF"

askPassword :: Handler String
askPassword = do
    pass <- askInput "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInput "Enter your mnemonic password again: "
        when (pass /= pass2) $ error "Passwords do not match"
    return pass

askSigningKeys :: String -> Handler (Maybe XPrvKey)
askSigningKeys name =
    -- Only ask for signing keys if the account doesn't have one already
    go =<< accountKeyExists name
  where
    go True = return Nothing
    go _ = do
        input <- askMnemonicOrKey "Enter mnemonic or extended private key: "
        case input of
            ParsedXPrvKey k      -> return $ Just k
            ParsedMnemonic _ _ k -> return $ Just k
            _                    -> error "Need a private key to sign"

-- hw start [config] [--detach]
cmdStart :: Handler ()
cmdStart = do
    cfg <- R.ask
    liftIO $ runSPVServer cfg

-- hw stop [config]
cmdStop :: Handler ()
cmdStop = do
    sendZmq StopServerReq >>= (`handleResponse` (\() -> return ()))
    liftIO $ putStrLn "Process stopped"

-- First argument: is account read-only?
cmdNewAcc :: Bool -> String -> [String] -> Handler ()
cmdNewAcc readOnly name ls = do
    e <- R.asks configEntropy
    d <- R.asks configDerivIndex
    _ <- return $! typ
    accountExists name >>= (`when` error "Account exists")
    (masterM, keyM, mnemonicM, passM) <- go =<< askMnemonicOrKey
        "Enter mnemonic, extended key or leave empty to generate: "
    let newAcc = NewAccount
            { newAccountName     = T.pack name
            , newAccountType     = typ
            , newAccountMnemonic = cs <$> mnemonicM
            , newAccountPassword = cs <$> passM
            , newAccountEntropy  = Just e
            , newAccountMaster   = masterM
            , newAccountDeriv    = Just d
            , newAccountKeys     = maybeToList keyM
            , newAccountReadOnly = readOnly
            }
    resE <- sendZmq $ NewAccountReq newAcc
    handleResponse resE $ liftIO . putStr . printAccount
  where
    go (ParsedXPrvKey k)      = return (Just k, Nothing, Nothing, Nothing)
    go (ParsedXPubKey p)      = return (Nothing, Just p, Nothing, Nothing)
    go (ParsedMnemonic m x _) = return (Nothing, Nothing, Just m, Just x)
    go ParsedNothing = do
        pass <- cs <$> askPassword
        return (Nothing, Nothing, Nothing, Just pass)
    typ = case ls of
        [] -> AccountRegular
        [mS, nS] -> fromMaybe (error "Account information incorrect") $ do
            m <- readMaybe mS
            n <- readMaybe nS
            return $ AccountMultisig m n
        _ -> error "Number of parametres incorrect"

cmdAddKey :: String -> Handler ()
cmdAddKey name = do
    d <- R.asks configDerivIndex
    accountExists name >>= (`unless` error "Account does not exist")
    let msg = "Enter mnemonic or extended private key: "
    key <- askMnemonicOrKey msg >>= \pk -> return $ case pk of
        ParsedXPrvKey k      -> deriveXPubKey $ rootToAccKey k d
        ParsedMnemonic _ _ k -> deriveXPubKey $ rootToAccKey k d
        ParsedXPubKey p      -> p
        ParsedNothing        -> error "Invalid empty input"
    resE <- sendZmq (AddPubKeysReq (T.pack name) [key])
    handleResponse resE $ liftIO . putStr . printAccount

cmdSetGap :: String -> String -> Handler ()
cmdSetGap name gap = do
    resE <- sendZmq (SetAccountGapReq (T.pack name) newGap)
    handleResponse resE $ liftIO . putStr . printAccount
  where
    newGap = read gap

cmdAccount :: String -> Handler ()
cmdAccount name = do
    resE <- sendZmq (AccountReq $ T.pack name)
    handleResponse resE $ liftIO . putStr . printAccount

cmdAccounts :: [String] -> Handler ()
cmdAccounts ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    listAction page AccountsReq $ \ts -> do
        let xs = map (liftIO . putStr . printAccount) ts
        sequence_ $ intersperse (liftIO $ putStrLn "-") xs

cmdRenameAcc :: String -> String -> Handler ()
cmdRenameAcc oldName newName = do
    resE <- sendZmq $ RenameAccountReq (T.pack oldName) (T.pack newName)
    handleResponse resE $ liftIO . putStr . printAccount

listAction :: (FromJSON a, ToJSON a)
            => Word32
            -> (ListRequest -> WalletRequest)
            -> ([a] -> Handler ())
            -> Handler ()
listAction page requestBuilder action = do
    c <- R.asks configCount
    r <- R.asks configReversePaging
    case c of
        0 -> do
            let listReq = ListRequest 0 0 r
            resE <- sendZmq (requestBuilder listReq)
            handleResponse resE $ \(ListResult a _) -> action a
        _ -> do
            when (page < 1) $ error "Page cannot be less than 1"
            let listReq = ListRequest ((page - 1) * c) c r
            resE <- sendZmq (requestBuilder listReq)
            handleResponse resE $ \(ListResult a m) -> case m of
                0 -> liftIO . putStrLn $ "No elements"
                _ -> do
                    liftIO . putStrLn $
                        "Page " ++ show page ++ " of " ++ show (pages m c) ++
                        " (" ++ show m ++ " elements)"
                    action a
  where
    pages m c | m `mod` c == 0 = m `div` c
              | otherwise = m `div` c + 1

cmdList :: String -> [String] -> Handler ()
cmdList name ls = do
    t <- R.asks configAddrType
    m <- R.asks configMinConf
    o <- R.asks configOffline
    p <- R.asks configDisplayPubKeys
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
        f = AddrsReq (T.pack name) t m o
    listAction page f $ \as -> forM_ as (liftIO . putStrLn . printAddress p)

cmdUnused :: String -> [String] -> Handler ()
cmdUnused name ls = do
    t <- R.asks configAddrType
    p <- R.asks configDisplayPubKeys
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
        f = UnusedAddrsReq (T.pack name) t
    listAction page f $ \as -> forM_ (as :: [JsonAddr]) $
        liftIO . putStrLn . printAddress p

cmdLabel :: String -> String -> String -> Handler ()
cmdLabel name iStr label = do
    t <- R.asks configAddrType
    p <- R.asks configDisplayPubKeys
    resE <- sendZmq (SetAddrLabelReq (T.pack name) i t addrLabel)
    handleResponse resE $ liftIO . putStrLn . printAddress p
  where
    i         = read iStr
    addrLabel = T.pack label

cmdTxs :: String -> [String] -> Handler ()
cmdTxs name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- R.asks configReversePaging
    listAction page (TxsReq (T.pack name)) $ \ts -> do
        let xs = map (liftIO . putStr . printTx Nothing) ts
            xs' = if r then xs else reverse xs
        sequence_ $ intersperse (liftIO $ putStrLn "-") xs'

cmdPending :: String -> [String] -> Handler ()
cmdPending name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- R.asks configReversePaging
    listAction page (PendingTxsReq (T.pack name)) $ \ts -> do
        let xs = map (liftIO . putStr . printTx Nothing) ts
            xs' = if r then xs else reverse xs
        sequence_ $ intersperse (liftIO $ putStrLn "-") xs'

cmdDead :: String -> [String] -> Handler ()
cmdDead name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- R.asks configReversePaging
    listAction page (DeadTxsReq (T.pack name)) $ \ts -> do
        let xs = map (liftIO . putStr . printTx Nothing) ts
            xs' = if r then xs else reverse xs
        sequence_ $ intersperse (liftIO $ putStrLn "-") xs'

cmdAddrTxs :: String -> String -> [String] -> Handler ()
cmdAddrTxs name i ls = do
    t <- R.asks configAddrType
    m <- R.asks configMinConf
    o <- R.asks configOffline
    r <- R.asks configReversePaging
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
        f = AddrTxsReq (T.pack name) index t
    resE <- sendZmq (AddressReq (T.pack name) index t m o)
    handleResponse resE $ \JsonAddr{..} -> listAction page f $ \ts -> do
        let xs = map (liftIO . putStr . printTx (Just jsonAddrAddress)) ts
            xs' = if r then xs else reverse xs
        sequence_ $ intersperse (liftIO $ putStrLn "-") xs'
  where
    index = fromMaybe (error "Could not read index") $ readMaybe i

cmdGetIndex :: String -> String -> Handler ()
cmdGetIndex name k = do
    t <- R.asks configAddrType
    resE <- sendZmq $ PubKeyIndexReq (T.pack name) (fromString k) t
    handleResponse resE go
  where
    go :: [JsonAddr] -> Handler ()
    go [] = liftIO $ putStrLn $ "No matching pubkey found"
    go as = mapM_ (liftIO . putStrLn . printAddress True) as

cmdGenAddrs :: String -> String -> Handler ()
cmdGenAddrs name i = do
    t <- R.asks configAddrType
    let req = GenerateAddrsReq (T.pack name) index t
    resE <- sendZmq req
    handleResponse resE $ \cnt -> liftIO . putStrLn $
        unwords [ "Generated", show (cnt :: Int), "addresses" ]
  where
    index = read i

-- Build a bitcoin payment request URI
cmdURI :: String -> String -> [String] -> Handler ()
cmdURI name iStr ls = do
    t <- R.asks configAddrType
    resE <- sendZmq (AddressReq (T.pack name) i t 0 False)
    case parseResponse resE of
        Just a  -> do
            let uri = buildPaymentRequest (jsonAddrAddress a) ls
            liftIO $ putStrLn $ cs uri
        Nothing -> error "No address found"
  where
    i = read iStr

buildPaymentRequest :: Address -> [String] -> BS.ByteString
buildPaymentRequest a ls =
    "bitcoin:" <> addrToBase58 a <> cs params
  where
    params = if null paramStr then "" else "?" <> paramStr
    paramStr = concat $ intersperse "&" $ zipWith ($)
        [ ("amount" `buildParam`) . parseAmount
        , ("message" `buildParam`) . URI.encode
        ] ls
    parseAmount str = show (read str :: Double)
    buildParam str val = str <> "=" <> val

cmdSend :: String -> String -> String -> Handler ()
cmdSend name addrStr amntStr = cmdSendMany name [addrStr ++ ":" ++ amntStr]

cmdSendMany :: String -> [String] -> Handler ()
cmdSendMany name xs = case rcpsM of
    Just rcps -> do
        fee     <- R.asks configFee
        rcptFee <- R.asks configRcptFee
        minconf <- R.asks configMinConf
        sign    <- R.asks configSignTx
        masterM <- if sign then askSigningKeys name else return Nothing
        let action = CreateTx rcps fee minconf rcptFee sign masterM
        resE <- sendZmq (CreateTxReq (T.pack name) action)
        handleResponse resE $ liftIO . putStr . printTx Nothing
    _ -> error "Could not parse recipient information"
  where
    g str   = map cs $ T.splitOn ":" (T.pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (readMaybe $ cs v)
    f _     = Nothing
    rcpsM   = mapM (f . g) xs

getHexTx :: Handler Tx
getHexTx = do
    hexM <- Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine ""
    let txM = case hexM of
            Nothing  -> error "No action due to EOF"
            Just hex -> eitherToMaybe . S.decode =<< decodeHex (cs hex)
    case txM of
        Just tx -> return tx
        Nothing -> error "Could not parse transaction"

cmdImport :: String -> Handler ()
cmdImport name = do
    tx <- getHexTx
    resE <- sendZmq (ImportTxReq (T.pack name) tx)
    handleResponse resE $ liftIO . putStr . printTx Nothing

cmdSign :: String -> String -> Handler ()
cmdSign name txidStr = case txidM of
    Just txid -> do
        masterM <- askSigningKeys name
        resE <- sendZmq (SignTxReq (T.pack name) $ SignTx txid masterM)
        handleResponse resE $ liftIO . putStr . printTx Nothing
    _ -> error "Could not parse txid"
  where
    txidM = hexToTxHash $ cs txidStr

cmdGetOffline :: String -> String -> Handler ()
cmdGetOffline name tidStr = case tidM of
    Just tid -> do
        resE <- sendZmq (OfflineTxReq (T.pack name) tid)
        handleResponse resE $ \otd ->
            liftIO $ putStrLn $ cs $
                B64.encode $ S.encode (otd :: OfflineTxData)
    _ -> error "Could not parse txid"
  where
    tidM = hexToTxHash $ cs tidStr

cmdSignOffline :: String -> Handler ()
cmdSignOffline name = do
    inputM <- Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine ""
    case S.decode =<< B64.decode . cs =<< maybeToEither "" inputM of
        Right (OfflineTxData tx dat) -> do
            masterM <- askSigningKeys name
            resE <- sendZmq (SignOfflineTxReq (T.pack name) masterM tx dat)
            handleResponse resE $ \(TxCompleteRes sTx _) ->
                liftIO $ putStrLn $ cs $ encodeHex $ S.encode sTx
        _ -> error "Could not decode input data"

cmdBalance :: String -> Handler ()
cmdBalance name = do
    m <- R.asks configMinConf
    o <- R.asks configOffline
    resE <- sendZmq (BalanceReq (T.pack name) m o)
    handleResponse resE $ \bal ->
        liftIO $ putStrLn $ unwords [ "Balance:", show (bal :: Word64) ]

cmdGetTx :: String -> String -> Handler ()
cmdGetTx name tidStr = case tidM of
    Just tid -> do
        resE <- sendZmq (TxReq (T.pack name) tid)
        handleResponse resE $ liftIO . putStr . printTx Nothing
    _ -> error "Could not parse txid"
  where
    tidM = hexToTxHash $ cs tidStr

cmdRescan :: [String] -> Handler ()
cmdRescan timeLs = do
    let timeM = case timeLs of
            [] -> Nothing
            str:_ -> case readMaybe str of
                Nothing -> error "Could not decode time"
                Just t  -> Just t
    resE <- sendZmq (NodeActionReq $ NodeActionRescan timeM)
    handleResponse resE $ \(RescanRes ts) ->
        liftIO $ putStrLn $ unwords [ "Timestamp:", show ts]

cmdDeleteTx :: String -> Handler ()
cmdDeleteTx tidStr = case tidM of
    Just tid -> do
        resE <- sendZmq (DeleteTxReq tid)
        handleResponse resE $ \() -> return ()
    Nothing -> error "Could not parse txid"
  where
    tidM = hexToTxHash $ cs tidStr

cmdMonitor :: [String] -> Handler ()
cmdMonitor ls = do
    cfg@Config{..} <- R.ask
    -- TODO: I can do this in the same thread without ^C twice (see sendZmq)
    liftIO $ withContext $ \ctx -> withSocket ctx Sub $ \sock -> do
        setLinger (restrict (0 :: Int)) sock
        setupAuth cfg sock
        connect sock configConnectNotif
        subscribe sock "[block]"
        forM_ ls $ \name -> subscribe sock $ "{" <> cs name <> "}"
        forever $ do
            [_,m] <- receiveMulti sock
            handleNotif configFormat $ eitherDecode $ cs m

cmdSync :: String -> String -> [String] -> Handler ()
cmdSync acc block ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
        f = case length block of
            64 -> SyncReq (cs acc) $
                fromMaybe (error "Could not decode block id") $
                hexToBlockHash $ cs block
            _  -> SyncHeightReq (cs acc) $
                fromMaybe (error "Could not decode block height") $
                readMaybe block
    r <- R.asks configReversePaging
    listAction page f $ \blocks -> do
        let blocks' = if r then reverse blocks else blocks
        forM_ (blocks' :: [JsonSyncBlock]) $ liftIO . putStrLn . printSyncBlock

cmdDecodeTx :: Handler ()
cmdDecodeTx = do
    tx <- getHexTx
    format <- R.asks configFormat
    liftIO $ formatStr $ cs $ case format of
        OutputJSON -> cs $ jsn tx
        _          -> YAML.encode $ val tx
  where
    val = encodeTxJSON
    jsn = JSON.encodePretty . val

cmdVersion :: Handler ()
cmdVersion = liftIO $ do
    putStrLn $ unwords [ "network   :", cs networkName ]
    putStrLn $ unwords [ "user-agent:", cs haskoinUserAgent ]

cmdStatus :: Handler ()
cmdStatus = do
    v <- R.asks configVerbose
    resE <- sendZmq (NodeActionReq NodeActionStatus)
    handleResponse resE $ mapM_ (liftIO . putStrLn) . printNodeStatus v

cmdKeyPair :: Handler ()
cmdKeyPair = do
    (pub, sec) <- curveKeyPair
    liftIO $ do
        B8.putStrLn $ B8.unwords [ "public :", rvalue pub ]
        B8.putStrLn $ B8.unwords [ "private:", rvalue sec ]

cmdBlockInfo :: [String] -> Handler ()
cmdBlockInfo headers = do
    -- Show best block if no arguments are provided
    hashL <- if null headers then
            -- Fetch best block hash from status msg, and return as list
            (: []) . parseRes <$> sendZmq (NodeActionReq NodeActionStatus)
        else
            return (map fromString headers)
    sendZmq (BlockInfoReq hashL) >>=
        \resE -> handleResponse resE (liftIO . printResults)
  where
    printResults :: [BlockInfo] -> IO ()
    printResults = mapM_ $ putStrLn . unlines . printBlockInfo
    parseRes :: Either String (WalletResponse NodeStatus) -> BlockHash
    parseRes = nodeStatusBestHeader . fromMaybe
        (error "No response to NodeActionStatus msg") . parseResponse

-- Do not reuse a dice roll to generate mnemonics because:
-- (D xor B) xor (D xor C) = (D xor D) xor (B xor C) = (B xor C)
-- You will leak (B xor C) which is the xor of your computer entropy.
-- In other words, don't reuse any part of a one time pad.
cmdDice :: String -> Handler ()
cmdDice rolls = case diceToEntropy rolls of
    Right ent1 -> do
        -- Get more entropy from /dev/urandom
        ent2 <- liftIO $ getEntropy 32
        -- Mix the entropy using xor and generate a mnemonic
        case toMnemonic $ mixEntropy ent1 ent2 of
            Right ms -> liftIO $ putStrLn $ cs ms
            Left err -> error err
    Left err -> error err

-- Mix entropy of same length by xoring them
mixEntropy :: BS.ByteString -> BS.ByteString -> BS.ByteString
mixEntropy ent1 ent2
    | BS.length ent1 == BS.length ent2 = BS.pack $ BS.zipWith xor ent1 ent2
    | otherwise = error "Entropy is not of the same length"

diceToMnemonic :: String -> Either String BS.ByteString
diceToMnemonic = toMnemonic <=< diceToEntropy

-- Transform 99 dice rolls (255.9 bits of entropy) into zero padded 32 bytes
diceToEntropy :: String -> Either String BS.ByteString
diceToEntropy rolls
    | length rolls /= 99 = Left "99 dice rolls are required"
    | otherwise = do
        ent <- maybeToEither "Could not decode base6" $ decodeBase6 $ cs rolls
        -- This check should probably never trigger
        when (BS.length ent > 32) $ Left "Invalid entropy length"
        let z = BS.replicate (32 - BS.length ent) 0x00
        return $ BS.append z ent

b6Data :: BS.ByteString
b6Data = "612345"

b6' :: Char -> Maybe Int
b6' = flip B8.elemIndex b6Data

decodeBase6 :: BS.ByteString -> Maybe BS.ByteString
decodeBase6 t
    | BS.null t = Just BS.empty
    | otherwise = integerToBS <$> decodeBase6I t

decodeBase6I :: BS.ByteString -> Maybe Integer
decodeBase6I bs = case resM of
    Just (i,[]) -> return i
    _           -> Nothing
  where
    resM = listToMaybe $ readInt 6 (isJust . b6') f $ cs bs
    f    = fromMaybe (error "Could not decode base6") . b6'

{- Helpers -}

handleNotif :: OutputFormat -> Either String Notif -> IO ()
handleNotif _   (Left e) = error e
handleNotif fmt (Right notif) = case fmt of
    OutputJSON -> formatStr $ cs $
        JSON.encodePretty notif
    OutputYAML -> do
        putStrLn "---"
        formatStr $ cs $ YAML.encode notif
        putStrLn "..."
    OutputNormal ->
        putStrLn $ printNotif notif

parseResponse
    :: Either String (WalletResponse a)
    -> Maybe a
parseResponse resE = case resE of
    Right (ResponseValid resM) -> resM
    Right (ResponseError err)  -> error $ T.unpack err
    Left err                   -> error err

handleResponse
    :: ToJSON a
    => Either String (WalletResponse a)
    -> (a -> Handler ())
    -> Handler ()
handleResponse resE handle = case parseResponse resE of
    Just a  -> formatOutput a =<< R.asks configFormat
    Nothing -> return ()
  where
    formatOutput a format = case format of
        OutputJSON   -> liftIO . formatStr $ cs $
            JSON.encodePretty a
        OutputYAML   -> liftIO . formatStr $ cs $ YAML.encode a
        OutputNormal -> handle a

sendZmq :: FromJSON a
        => WalletRequest
        -> Handler (Either String (WalletResponse a))
sendZmq req = do
    cfg <- R.ask
    let msg = cs $ encode req
    when (configVerbose cfg) $ liftIO $
        B8.hPutStrLn stderr $ "Outgoing JSON: " `mappend` msg
    -- TODO: If I do this in the same thread I have to ^C twice to exit
    a <- liftIO $ async $ withContext $ \ctx ->
        withSocket ctx Req $ \sock -> do
            setLinger (restrict (0 :: Int)) sock
            setupAuth cfg sock
            connect sock (configConnect cfg)
            send sock [] (cs $ encode req)
            eitherDecode . cs <$> receive sock
    liftIO $ wait a

setupAuth :: Config
          -> Socket t
          -> IO ()
setupAuth cfg sock = do
    let clientKeyM    = configClientKey    cfg
        clientKeyPubM = configClientKeyPub cfg
        serverKeyPubM = configServerKeyPub cfg
    forM_ clientKeyM $ \clientKey -> do
        let serverKeyPub = fromMaybe
              (error "Server public key not provided")
              serverKeyPubM
            clientKeyPub = fromMaybe
              (error "Client public key not provided")
              clientKeyPubM
        setCurveServerKey TextFormat serverKeyPub sock
        setCurvePublicKey TextFormat clientKeyPub sock
        setCurveSecretKey TextFormat clientKey sock

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

encodeTxJSON :: Tx -> Value
encodeTxJSON tx = object
    [ "txid"     .= (cs $ txHashToHex (txHash tx) :: T.Text)
    , "version"  .= txVersion tx
    , "inputs"   .= map encodeTxInJSON (txIn tx)
    , "outputs"  .= map encodeTxOutJSON (txOut tx)
    , "locktime" .= txLockTime tx
    ]

encodeTxInJSON :: TxIn -> Value
encodeTxInJSON (TxIn o s i) = object $
    [ "outpoint"   .= encodeOutPointJSON o
    , "sequence"   .= i
    , "raw-script" .= (cs $ encodeHex s :: T.Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = either (const $ Script []) id $ S.decode s
    decoded = either (const []) f $ decodeInputBS s
    f inp = ["decoded-script" .= encodeScriptInputJSON inp]

encodeTxOutJSON :: TxOut -> Value
encodeTxOutJSON (TxOut v s) = object $
    [ "value"      .= v
    , "raw-script" .= (cs $ encodeHex s :: T.Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = either (const $ Script []) id $ S.decode s
    decoded = either (const [])
                 (\out -> ["decoded-script" .= encodeScriptOutputJSON out])
                 (decodeOutputBS s)

encodeOutPointJSON :: OutPoint -> Value
encodeOutPointJSON (OutPoint h i) = object
    [ "txid" .= (cs $ txHashToHex h :: T.Text)
    , "pos"  .= i
    ]

encodeScriptJSON :: Script -> Value
encodeScriptJSON (Script ops) =
    toJSON $ map f ops
  where
    f (OP_PUSHDATA bs _) = String $ T.pack $ unwords
        ["OP_PUSHDATA", cs $ encodeHex bs]
    f x = String $ T.pack $ show x

encodeScriptInputJSON :: ScriptInput -> Value
encodeScriptInputJSON si = case si of
    RegularInput (SpendPK s) -> object
        [ "spendpubkey" .= object [ "sig" .= encodeSigJSON s ] ]
    RegularInput (SpendPKHash s p) -> object
        [ "spendpubkeyhash" .= object
            [ "sig"            .= encodeSigJSON s
            , "pubkey"         .= (cs $ encodeHex (S.encode p) :: T.Text)
            , "sender-address" .= (cs $ addrToBase58 (pubKeyAddr p) :: T.Text)
            ]
        ]
    RegularInput (SpendMulSig sigs) -> object
        [ "spendmulsig" .= object [ "sigs" .= map encodeSigJSON sigs ] ]
    ScriptHashInput s r -> object
        [ "spendscripthash" .= object
            [ "scriptinput" .= encodeScriptInputJSON (RegularInput s)
            , "redeem" .= encodeScriptOutputJSON r
            , "raw-redeem" .= (cs $ encodeHex (encodeOutputBS r) :: T.Text)
            , "sender-address" .= (cs $ addrToBase58 (p2shAddr r) :: T.Text)
            ]
        ]

encodeScriptOutputJSON :: ScriptOutput -> Value
encodeScriptOutputJSON so =
    case so of
        PayPK p ->
            object
                [ "pay2pubkey" .=
                  object ["pubkey" .= (cs $ encodeHex (S.encode p) :: T.Text)]
                ]
        PayPKHash h ->
            object
                [ "pay2pubkeyhash" .=
                  object
                      [ "address-hex" .= (cs $ encodeHex (S.encode h) :: T.Text)
                      , "address-base58" .=
                        (cs (addrToBase58 (PubKeyAddress h)) :: T.Text)
                      ]
                ]
        PayMulSig ks r ->
            object
                [ "pay2mulsig" .=
                  object
                      [ "required-keys" .= r
                      , "pubkeys" .=
                        (map (cs . encodeHex . S.encode) ks :: [T.Text])
                      ]
                ]
        PayScriptHash h ->
            object
                [ "pay2scripthash" .=
                  object
                      [ "address-hex" .= (cs $ encodeHex $ S.encode h :: T.Text)
                      , "address-base58" .=
                        (cs (addrToBase58 (ScriptAddress h)) :: T.Text)
                      ]
                ]
        DataCarrier bs ->
            object
                [ "op_return" .=
                  object ["data" .= (cs $ encodeHex bs :: T.Text)]
                ]

encodeSigJSON :: TxSignature -> Value
encodeSigJSON ts@(TxSignature _ sh) = object
    [ "raw-sig" .= (cs $ encodeHex (encodeSig ts) :: T.Text)
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

{- Print utilities -}

printAccount :: JsonAccount -> String
printAccount JsonAccount{..} = unlines $
    [ "Account : " ++ T.unpack jsonAccountName
    , "Type    : " ++ showType
    , "Gap     : " ++ show jsonAccountGap
    ]
    ++
    [ "Index   : " ++ show i | i <- childLs ]
    ++
    [ "Mnemonic: " ++ cs ms
    | ms <- maybeToList jsonAccountMnemonic
    ]
    ++
    concat [ printKeys | not (null jsonAccountKeys) ]
  where
    childLs = case jsonAccountType of
        AccountRegular -> map xPubChild jsonAccountKeys
        _              -> maybeToList $ xPrvChild <$> jsonAccountMaster
    printKeys =
        ("Keys    : " ++ cs (xPubExport (head jsonAccountKeys))) :
        map (("          " ++) . cs . xPubExport) (tail jsonAccountKeys)
    showType = case jsonAccountType of
        AccountRegular -> if isNothing jsonAccountMaster
                              then "Read-Only" else "Regular"
        AccountMultisig m n -> unwords
            [ if isNothing jsonAccountMaster
                 then "Read-Only Multisig" else "Multisig"
            , show m, "of", show n
            ]

printAddress :: Bool -> JsonAddr -> String
printAddress displayPubKey JsonAddr{..} = unwords $
    [ show jsonAddrIndex, ":", cs dat ]
    ++
    [ "(" ++ T.unpack jsonAddrLabel ++ ")"
    | not (null $ T.unpack jsonAddrLabel)
    ]
    ++ concat
    [ [ "[Received: "    ++ show (balanceInfoInBalance  bal) ++ "]"
      , "[Coins: "       ++ show (balanceInfoCoins      bal) ++ "]"
      , "[Spent Coins: " ++ show (balanceInfoSpentCoins bal) ++ "]"
      ]
    | isJust jsonAddrBalance && balanceInfoCoins bal > 0
    ]
  where
    dat | displayPubKey =
            maybe "<no pubkey available>" (encodeHex . S.encode) jsonAddrKey
        | otherwise = addrToBase58 jsonAddrAddress
    bal = fromMaybe (error "Could not get address balance") jsonAddrBalance

printNotif :: Notif -> String
printNotif (NotifTx   tx) = printTx Nothing tx
printNotif (NotifBlock b) = printBlock b

printTx :: Maybe Address -> JsonTx -> String
printTx aM tx@JsonTx{..} = unlines $
    [ "Id         : " ++ cs (txHashToHex jsonTxHash) ]
    ++
    [ "Value      : " ++ printTxType jsonTxType ++ " " ++ show jsonTxValue ]
    ++
    [ "Confidence : " ++ printTxConfidence tx ]
    ++ concat
    [ printAddrInfos "Inputs     : " jsonTxInputs
    | not (null jsonTxInputs)
    ]
    ++ concat
    [ printAddrInfos "Outputs    : " jsonTxOutputs
    | not (null jsonTxOutputs)
    ]
    ++ concat
    [ printAddrInfos "Change     : " jsonTxChange
    | not (null jsonTxChange)
    ]
  where
    printAddrInfos header xs =
        (header ++ f (head xs)) :
        map (("             " ++) . f) (tail xs)
    f (AddressInfo addr valM local) = unwords $
        cs (addrToBase58 addr) :
        [ show v | v <- maybeToList valM ]
        ++
        [ "<-" | maybe local (== addr) aM ]

printTxConfidence :: JsonTx -> String
printTxConfidence JsonTx{..} = case jsonTxConfidence of
    TxBuilding -> "Building" ++ confirmations
    TxPending  -> "Pending" ++ confirmations
    TxDead     -> "Dead" ++ confirmations
    TxOffline  -> "Offline"
  where
    confirmations = case jsonTxConfirmations of
        Just conf -> " (Confirmations: " ++ show conf ++ ")"
        _         -> ""

printTxType :: TxType -> String
printTxType t = case t of
    TxIncoming -> "Incoming"
    TxOutgoing -> "Outgoing"
    TxSelf     -> "Self"

printBlock :: JsonBlock -> String
printBlock JsonBlock{..} = unlines
    [ "Block Hash      : " ++ cs (blockHashToHex jsonBlockHash)
    , "Block Height    : " ++ show jsonBlockHeight
    , "Previous block  : " ++ cs (blockHashToHex jsonBlockPrev)
    ]

printSyncBlock :: JsonSyncBlock -> String
printSyncBlock JsonSyncBlock{..} = unlines
    [ "Block Hash      : " ++ cs (blockHashToHex jsonSyncBlockHash)
    , "Block Height    : " ++ show jsonSyncBlockHeight
    , "Previous block  : " ++ cs (blockHashToHex jsonSyncBlockPrev)
    , "Transactions    : " ++ show (length jsonSyncBlockTxs)
    ]

printNodeStatus :: Bool -> NodeStatus -> [String]
printNodeStatus verbose NodeStatus{..} =
    [ "Network Height    : " ++ show nodeStatusNetworkHeight
    , "Best Header       : " ++ cs (blockHashToHex nodeStatusBestHeader)
    , "Best Header Height: " ++ show nodeStatusBestHeaderHeight
    , "Best Block        : " ++ cs (blockHashToHex nodeStatusBestBlock)
    , "Best Block Height : " ++ show nodeStatusBestBlockHeight
    , "Bloom Filter Size : " ++ show nodeStatusBloomSize
    ] ++
    [ "Header Peer       : " ++ show h
    | h <- maybeToList nodeStatusHeaderPeer, verbose
    ] ++
    [ "Merkle Peer       : " ++ show m
    | m <- maybeToList nodeStatusMerklePeer, verbose
    ] ++
    [ "Pending Headers   : " ++ show nodeStatusHaveHeaders | verbose ] ++
    [ "Pending Tickles   : " ++ show nodeStatusHaveTickles | verbose ] ++
    [ "Pending Txs       : " ++ show nodeStatusHaveTxs | verbose ] ++
    [ "Pending GetData   : " ++ show (map txHashToHex nodeStatusGetData)
    | verbose
    ] ++
    [ "Pending Rescan    : " ++ show r
    | r <- maybeToList nodeStatusRescan, verbose
    ] ++
    [ "Synced Mempool    : " ++ show nodeStatusMempool | verbose ] ++
    [ "HeaderSync Lock   : " ++ show nodeStatusSyncLock | verbose ] ++
    [ "Peers: " ] ++
    intercalate ["-"] (map (printPeerStatus verbose) nodeStatusPeers)

printPeerStatus :: Bool -> PeerStatus -> [String]
printPeerStatus verbose PeerStatus{..} =
    [ "  Peer Id  : " ++ show peerStatusPeerId
    , "  Peer Host: " ++ peerHostString peerStatusHost
    , "  Connected: " ++ if peerStatusConnected then "yes" else "no"
    , "  Height   : " ++ show peerStatusHeight
    ] ++
    [ "  Protocol : " ++ show p | p <- maybeToList peerStatusProtocol
    ] ++
    [ "  UserAgent: " ++ ua | ua <- maybeToList peerStatusUserAgent
    ] ++
    [ "  Avg Ping : " ++ p | p <- maybeToList peerStatusPing
    ] ++
    [ "  DoS Score: " ++ show d | d <- maybeToList peerStatusDoSScore
    ] ++
    [ "  Merkles  : " ++ show peerStatusHaveMerkles | verbose ] ++
    [ "  Messages : " ++ show peerStatusHaveMessage | verbose ] ++
    [ "  Nonces   : " ++ show peerStatusPingNonces | verbose ] ++
    [ "  Reconnect: " ++ show t
    | t <- maybeToList peerStatusReconnectTimer, verbose
    ] ++
    [ "  Logs     : " | verbose ] ++
    [ "    - " ++ msg | msg <- fromMaybe [] peerStatusLog, verbose]

printBlockInfo :: BlockInfo -> [String]
printBlockInfo BlockInfo{..} =
    [ "Block Height     : " ++ show blockInfoHeight
    , "Block Hash       : " ++ cs (blockHashToHex blockInfoHash)
    , "Block Timestamp  : " ++ formatUTCTime blockInfoTimestamp
    , "Previous Block   : " ++ cs (blockHashToHex blockInfoPrevBlock)
    , "Merkle Root      : " ++ cs blockInfoMerkleRoot
    , "Block Version    : " ++ "0x" ++ cs (encodeHex versionData)
    , "Block Difficulty : " ++ show (blockDiff blockInfoBits)
    , "Chain Work       : " ++ show blockInfoChainWork
    ]
  where
    blockDiff :: Word32 -> Double
    blockDiff target = getTarget (blockBits genesisHeader) / getTarget target
    getTarget   = fromIntegral . fst . decodeCompact
    versionData = integerToBS (fromIntegral blockInfoVersion)
    formatUTCTime = Time.formatTime Time.defaultTimeLocale
        "%Y-%m-%d %H:%M:%S (UTC)"
