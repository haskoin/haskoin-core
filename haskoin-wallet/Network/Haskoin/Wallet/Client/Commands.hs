module Network.Haskoin.Wallet.Client.Commands
( cmdStart
, cmdStop
, cmdNewKeyRing
, cmdKeyRing
, cmdKeyRings
, cmdNewAcc
, cmdNewMS
, cmdNewRead
, cmdAddKeys
, cmdSetGap
, cmdAccount
, cmdAccounts
, cmdList
, cmdUnused
, cmdLabel
, cmdTxs
, cmdAddrTxs
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
)
where

import System.ZMQ4.Monadic
    ( Req(..)
    , runZMQ
    , socket
    , send
    , receive
    , connect
    )

import Control.Monad (forM_, when, liftM2)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.Reader as R (ReaderT, ask, asks)

import Data.Maybe
       (listToMaybe, isNothing, fromJust, fromMaybe, isJust, maybeToList)
import Data.List (intercalate, intersperse)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Word (Word64)
import qualified Data.Yaml as YAML (encode)
import qualified Data.Aeson.Encode.Pretty as JSON
    ( Config(..)
    , encodePretty'
    , defConfig
    )
import Data.Aeson
    ( Value(..)
    , FromJSON
    , ToJSON
    , toJSON
    , object
    , encode
    , decode
    , eitherDecode
    , (.=)
    )
import Data.String.Conversions (cs)

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Node.STM

import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server
import Network.Haskoin.Wallet.Database

type Handler = R.ReaderT Config IO

-- hw start [config] [--detach]
cmdStart :: Handler ()
cmdStart = do
    cfg <- R.ask
    liftIO $ runSPVServer cfg
    liftIO $ putStrLn "Process started"

-- hw stop [config]
cmdStop :: Handler ()
cmdStop = R.ask >>= \cfg -> liftIO $ do
    stopSPVServer cfg
    putStrLn "Process stopped"

cmdNewKeyRing :: [String] -> Handler ()
cmdNewKeyRing mnemonicLs = do
    keyRingName <- R.asks configKeyRing
    passphraseM <- R.asks configPass
    let mnemonicM  = pack <$> listToMaybe mnemonicLs
        newKeyRing = NewKeyRing keyRingName passphraseM mnemonicM
    sendZmq (PostKeyRingsR newKeyRing) $ putStr . printKeyRing

cmdKeyRing :: Handler ()
cmdKeyRing = do
    k <- R.asks configKeyRing
    sendZmq (GetKeyRingR k) $ putStr . printKeyRing

cmdKeyRings :: Handler ()
cmdKeyRings = sendZmq GetKeyRingsR $ \ks -> do
    let xs = map (putStr . printKeyRing) ks
    sequence_ $ intersperse (putStrLn "-") xs

cmdNewAcc :: String -> Handler ()
cmdNewAcc name = do
    k <- R.asks configKeyRing
    sendZmq (PostAccountsR k newAcc) $
        \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc
  where
    newAcc = NewAccount (pack name) (AccountRegular False) []

-- First argument: is account read-only?
cmdNewMS :: Bool -> String -> String -> String -> [String] -> Handler ()
cmdNewMS r name mStr nStr ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) (AccountMultisig r m n) keys
        sendZmq (PostAccountsR k newAcc) $
            \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc
    _ -> error "Could not parse key(s)"
  where
    m     = read mStr
    n     = read nStr
    keysM = mapM (xPubImport . cs) ks

cmdNewRead :: String -> String -> Handler ()
cmdNewRead name keyStr = case keyM of
    Just key -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) (AccountRegular True) [key]
        sendZmq (PostAccountsR k newAcc) $
            \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc
    _ -> error "Could not parse key"
  where
    keyM = xPubImport $ cs keyStr

cmdAddKeys :: String -> [String] -> Handler ()
cmdAddKeys name ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        sendZmq (PostAccountKeysR k (pack name) keys) $
            \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc
    _ -> error "Could not parse key(s)"
  where
    keysM = mapM (xPubImport . cs) ks

cmdSetGap :: String -> String -> Handler ()
cmdSetGap name gap = do
    k <- R.asks configKeyRing
    sendZmq (PostAccountGapR k (pack name) setGap) $
        \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc
  where
    setGap = SetAccountGap $ read gap

cmdAccount :: String -> Handler ()
cmdAccount name = do
    k <- R.asks configKeyRing
    sendZmq (GetAccountR k $ pack name) $
        \(JsonWithKeyRing _ acc) -> putStr $ printAccount acc

cmdAccounts :: Handler ()
cmdAccounts = do
    k <- R.asks configKeyRing
    sendZmq (GetAccountsR k) $ \(JsonWithKeyRing _ as) -> do
        let xs = map (putStr . printAccount) as
        sequence_ $ intersperse (putStrLn "-") xs

pagedAction :: (FromJSON a, ToJSON a)
            => [String]
            -> (PageRequest -> WalletRequest)
            -> ([a] -> IO ())
            -> Handler ()
pagedAction pageLs requestBuilder action = do
    c <- R.asks configCount
    r <- R.asks configReversePaging
    let pageReq = PageRequest page c r
    sendZmq (requestBuilder pageReq) $ \(JsonWithAccount _ _ (PageRes a m)) -> do
        putStrLn $ unwords [ "Page", show page, "of", show m ]
        action a
  where
    page = fromMaybe 1 (read <$> listToMaybe pageLs)

cmdList :: String -> [String] -> Handler ()
cmdList name pageLs = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    m <- R.asks configMinConf
    o <- R.asks configOffline
    let f = GetAddressesR k (pack name) t m o
    pagedAction pageLs f $ \as -> forM_ as (putStrLn . printAddress)

cmdUnused :: String -> Handler ()
cmdUnused name = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    sendZmq (GetAddressesUnusedR k (pack name) t) $
        \(JsonWithAccount _ _ as) -> forM_ (as :: [JsonAddr]) $ putStrLn . printAddress

cmdLabel :: String -> String -> String -> Handler ()
cmdLabel name iStr label = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    sendZmq (PutAddressR k (pack name) i t addrLabel) $
        \(JsonWithAccount _ _ a) -> putStrLn $ printAddress a
  where
    i         = read iStr
    addrLabel = AddressLabel $ pack label

cmdTxs :: String -> [String] -> Handler ()
cmdTxs name pageLs = do
    k <- R.asks configKeyRing
    pagedAction pageLs (GetTxsR k (pack name)) $ \ts -> do
        let xs = map (putStr . printTx) ts
        sequence_ $ intersperse (putStrLn "-") xs

cmdAddrTxs :: String -> String -> [String] -> Handler ()
cmdAddrTxs name i pageLs = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    c <- R.asks configCount
    r <- R.asks configReversePaging
    let req = GetAddrTxsR k (pack name) index t $ PageRequest page c r
    sendZmq req $ \(JsonWithAddr _ _ _ (PageRes ts m)) -> do
        putStrLn $ unwords [ "Page", show page, "of", show m ]
        let xs = map (putStr . printAddrTx) ts
        sequence_ $ intersperse (putStrLn "-") xs
  where
    page  = fromMaybe 1 (read <$> listToMaybe pageLs)
    index = read i

cmdGenAddrs :: String -> String -> Handler ()
cmdGenAddrs name i = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    let req = PostAddressesR k (pack name) index t
    sendZmq req $ \(JsonWithAccount _ _ cnt) ->
        putStrLn $ unwords [ "Generated", show (cnt :: Int), "addresses" ]
  where
    index = read i

cmdSend :: String -> String -> String -> Handler ()
cmdSend name addrStr amntStr = cmdSendMany name [addrStr ++ ":" ++ amntStr]

cmdSendMany :: String -> [String] -> Handler ()
cmdSendMany name xs = case rcpsM of
    Just rcps -> do
        k       <- R.asks configKeyRing
        fee     <- R.asks configFee
        rcptFee <- R.asks configRcptFee
        minconf <- R.asks configMinConf
        sign    <- R.asks configSignTx
        let action = CreateTx rcps fee minconf rcptFee sign
        sendZmq (PostTxsR k (pack name) action) $
            \(JsonWithAccount _ _ tx) -> putStr $ printTx tx
    _ -> error "Could not parse recipient information"
  where
    g str   = map cs $ splitOn ":" (pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read $ cs v)
    f _     = Nothing
    rcpsM   = mapM (f . g) xs

cmdImport :: String -> String -> Handler ()
cmdImport name txStr = case txM of
    Just tx -> do
        k <- R.asks configKeyRing
        let action = ImportTx tx
        sendZmq (PostTxsR k (pack name) action) $
            \(JsonWithAccount _ _ t) -> putStr $ printTx t
    _ -> error "Could not parse transaction"
  where
    txM = decodeToMaybe =<< decodeHex (cs txStr)

cmdSign :: String -> String -> Handler ()
cmdSign name txidStr = case txidM of
    Just txid -> do
        k <- R.asks configKeyRing
        let action = SignTx txid
        sendZmq (PostTxsR k (pack name) action) $
            \(JsonWithAccount _ _ tx) -> putStr $ printTx tx
    _ -> error "Could not parse txid"
  where
    txidM = hexToTxHash $ cs txidStr

cmdGetOffline :: String -> String -> Handler ()
cmdGetOffline name tidStr = case tidM of
    Just tid -> do
        k <- R.asks configKeyRing
        sendZmq (GetOfflineTxR k (pack name) tid) $
            \(OfflineTxData tx dat) -> do
                putStrLn $ unwords
                    [ "Tx      :", cs $ encodeHex $ encode' tx ]
                putStrLn $ unwords
                    [ "CoinData:", cs $ encodeHex $ cs $ encode dat ]
    _ -> error "Could not parse txid"
  where
    tidM = hexToTxHash $ cs tidStr

cmdSignOffline :: String -> String -> String -> Handler ()
cmdSignOffline name txStr datStr = case (txM, datM) of
    (Just tx, Just dat) -> do
        k <- R.asks configKeyRing
        sendZmq (PostOfflineTxR k (pack name) tx dat) $
            \(TxCompleteRes tx' c) -> do
                putStrLn $ unwords [ "Tx      :", cs $ encodeHex $ encode' tx' ]
                putStrLn $ unwords [ "Complete:", if c then "Yes" else "No" ]
    _ -> error "Could not decode input data"
  where
    datM = decode . cs =<< decodeHex (cs datStr)
    txM  = decodeToMaybe =<< decodeHex (cs txStr)

cmdBalance :: String -> Handler ()
cmdBalance name = do
    k <- R.asks configKeyRing
    m <- R.asks configMinConf
    o <- R.asks configOffline
    sendZmq (GetBalanceR k (pack name) m o) $
        \(JsonWithAccount _ _ bal) ->
            putStrLn $ unwords [ "Balance:", show (bal :: Word64) ]

cmdGetTx :: String -> String -> Handler ()
cmdGetTx name tidStr = case tidM of
    Just tid -> do
        k <- R.asks configKeyRing
        sendZmq (GetTxR k (pack name) tid) $
            \(JsonWithAccount _ _ tx) -> putStr $ printTx tx
    _ -> error "Could not parse txid"
  where
    tidM = hexToTxHash $ cs tidStr

cmdRescan :: [String] -> Handler ()
cmdRescan timeLs =
    sendZmq (PostNodeR $ NodeActionRescan timeM) $ \(RescanRes ts) ->
        putStrLn $ unwords [ "Timestamp:", show ts]
  where
    timeM = read <$> listToMaybe timeLs

cmdDecodeTx :: String -> Handler ()
cmdDecodeTx txStr = do
    when (isNothing txM) $ error "Could not parse transaction"
    format <- R.asks configFormat
    liftIO $ formatStr $ cs $ case format of
        OutputJSON -> cs jsn
        _          -> YAML.encode val
  where
    txM = decodeToMaybe =<< decodeHex (cs txStr)
    val = encodeTxJSON $ fromJust txM
    jsn = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } val

cmdVersion :: Handler ()
cmdVersion = liftIO $ do
    putStrLn $ unwords [ "network   :", cs networkName ]
    putStrLn $ unwords [ "user-agent:", cs haskoinUserAgent ]
    putStrLn $ unwords [ "database  :", cs databaseEngine ]

cmdStatus :: Handler ()
cmdStatus = do
    v <- R.asks configVerbose
    sendZmq (PostNodeR NodeActionStatus) $ mapM_ putStrLn . printNodeStatus v

{- Helpers -}

sendZmq :: (FromJSON a, ToJSON a)
        => WalletRequest -> (a -> IO ()) -> Handler ()
sendZmq req handle = do
    sockName <- R.asks configConnect
    resE <- liftIO $ runZMQ $ do
        sock <- socket Req
        connect sock sockName
        send sock [] (cs $ encode req)
        eitherDecode . cs <$> receive sock
    case resE of
        Right (ResponseValid (Just a)) -> formatOutput a =<< R.asks configFormat
        Right (ResponseValid Nothing)  -> return ()
        Right (ResponseError err)      -> error $ unpack err
        Left err                       -> error err
  where
    formatOutput a format = liftIO $ case format of
        OutputJSON   -> formatStr $ cs $
            JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } a
        OutputYAML   -> formatStr $
            cs $ YAML.encode a
        OutputNormal -> handle a

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

encodeTxJSON :: Tx -> Value
encodeTxJSON tx@(Tx v is os i) = object
    [ "txid"     .= (cs $ txHashToHex (txHash tx) :: Text)
    , "version"  .= v
    , "inputs"   .= zipWith input is [0..]
    , "outputs"  .= zipWith output os [0..]
    , "locktime" .= i
    ]
  where
    input x j = object
      [pack ("input " ++ show (j :: Int)) .= encodeTxInJSON x]
    output x j = object
      [pack ("output " ++ show (j :: Int)) .= encodeTxOutJSON x]

encodeTxInJSON :: TxIn -> Value
encodeTxInJSON (TxIn o s i) = object $
    [ "outpoint"   .= encodeOutPointJSON o
    , "sequence"   .= i
    , "raw-script" .= (cs $ encodeHex s :: Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const []) f $ decodeInputBS s
    f inp = ["decoded-script" .= encodeScriptInputJSON inp]

encodeTxOutJSON :: TxOut -> Value
encodeTxOutJSON (TxOut v s) = object $
    [ "value"      .= v
    , "raw-script" .= (cs $ encodeHex s :: Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const [])
                 (\out -> ["decoded-script" .= encodeScriptOutputJSON out])
                 (decodeOutputBS s)

encodeOutPointJSON :: OutPoint -> Value
encodeOutPointJSON (OutPoint h i) = object
    [ "txid" .= (cs $ txHashToHex h :: Text)
    , "pos"  .= i
    ]

encodeScriptJSON :: Script -> Value
encodeScriptJSON (Script ops) =
    toJSON $ map f ops
  where
    f (OP_PUSHDATA bs _) = String $ pack $ unwords
        ["OP_PUSHDATA", cs $ encodeHex bs]
    f x = String $ pack $ show x

encodeScriptInputJSON :: ScriptInput -> Value
encodeScriptInputJSON si = case si of
    RegularInput (SpendPK s) -> object
        [ "spendpubkey" .= object [ "sig" .= encodeSigJSON s ] ]
    RegularInput (SpendPKHash s p) -> object
        [ "spendpubkeyhash" .= object
            [ "sig"            .= encodeSigJSON s
            , "pubkey"         .= (cs $ encodeHex (encode' p) :: Text)
            , "sender-address" .= (cs $ addrToBase58 (pubKeyAddr p) :: Text)
            ]
        ]
    RegularInput (SpendMulSig sigs) -> object
        [ "spendmulsig" .= object [ "sigs" .= map encodeSigJSON sigs ] ]
    ScriptHashInput s r -> object
        [ "spendscripthash" .= object
            [ "scriptinput" .= encodeScriptInputJSON (RegularInput s)
            , "redeem" .= encodeScriptOutputJSON r
            , "raw-redeem" .= (cs $ encodeHex (encodeOutputBS r) :: Text)
            , "sender-address" .= (cs $ addrToBase58 (scriptAddr r) :: Text)
            ]
        ]

encodeScriptOutputJSON :: ScriptOutput -> Value
encodeScriptOutputJSON so = case so of
    PayPK p -> object
        [ "pay2pubkey" .= object
          [ "pubkey" .= (cs $ encodeHex (encode' p) :: Text) ]
        ]
    PayPKHash a -> object
        [ "pay2pubkeyhash" .= object
            [ "address-base64" .=
              (cs $ encodeHex (encode' $ getAddrHash a) :: Text)
            , "address-base58" .= (cs $ addrToBase58 a :: Text)
            ]
        ]
    PayMulSig ks r -> object
        [ "pay2mulsig" .= object
            [ "required-keys" .= r
            , "pubkeys"       .= (map (cs . encodeHex . encode') ks :: [Text])
            ]
        ]
    PayScriptHash a -> object
        [ "pay2scripthash" .= object
            [ "address-base64" .= (cs $ encodeHex $ encode' $ getAddrHash a :: Text)
            , "address-base58" .= (cs (addrToBase58 a) :: Text)
            ]
        ]

encodeSigJSON :: TxSignature -> Value
encodeSigJSON ts@(TxSignature _ sh) = object
    [ "raw-sig" .= (cs $ encodeHex (encodeSig ts) :: Text)
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

printKeyRing :: JsonKeyRing -> String
printKeyRing JsonKeyRing{..} = unlines $
    [ "KeyRing: " ++ unpack jsonKeyRingName ]
    ++
    [ "Master key: " ++ cs (xPrvExport m)
    | m <- maybeToList jsonKeyRingMaster
    ]
    ++
    [ "Mnemonic: " ++ cs m
    | m <- maybeToList jsonKeyRingMnemonic
    ]

printAccount :: JsonAccount -> String
printAccount JsonAccount{..} = unlines $
    [ "Account: " ++ unpack jsonAccountName
    , "Type   : " ++ showType
    , "Gap    : " ++ show jsonAccountGap
    ]
    ++
    [ "Deriv  : " ++ toHaskoinString d
    | d <- maybeToList jsonAccountDerivation
    ]
    ++
    concat [ printKeys | not (null jsonAccountKeys) ]
  where
    printKeys =
        ("Keys   : " ++ cs (xPubExport (head jsonAccountKeys))) :
        map (("         " ++) . cs . xPubExport) (tail jsonAccountKeys)
    showType = case jsonAccountType of
        AccountRegular r -> if r then "Read-Only" else "Regular"
        AccountMultisig r m n -> unwords
            [ if r then "Read-Only Multisig" else "Multisig"
            , show m, "of", show n
            ]

printAddress :: JsonAddr -> String
printAddress JsonAddr{..} = unwords $
    [ show jsonAddrIndex, ":", cs (addrToBase58 jsonAddrAddress) ]
    ++
    [ "(" ++ unpack jsonAddrLabel ++ ")" | not (null $ unpack jsonAddrLabel) ]
    ++ concat
    [ [ "[Received: " ++ show (balanceInfoInBalance bal)   ++ "]"
        , "[Coins: "  ++ show (balanceInfoCoins bal)  ++ "]"
        , "[Spent Coins: " ++ show (balanceInfoSpentCoins bal) ++ "]"
        ]
        | isJust jsonAddrBalance && balanceInfoCoins bal > 0
      ]
  where
    bal = fromMaybe (error "Could not get address balance") jsonAddrBalance

printTx :: JsonTx -> String
printTx tx@JsonTx{..} = unlines $
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
        [ if local then "<-" else "" ]

printAddrTx :: AddrTx -> String
printAddrTx (AddrTx tx BalanceInfo{..}) = unlines $
    concat
    [ [ "Incoming value: " ++ show balanceInfoInBalance
      , "Incoming coins: " ++ show balanceInfoCoins
      ]
      | balanceInfoInBalance > 0
    ] ++ concat
    [ [ "Outgoing value: " ++ show balanceInfoOutBalance
      , "Spent coins   : " ++ show balanceInfoSpentCoins
      ]
      | balanceInfoOutBalance > 0
    ] ++
    [   "Confidence    : " ++ printTxConfidence tx ]

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

printNodeStatus :: Bool -> NodeStatus -> [String]
printNodeStatus verbose NodeStatus{..} =
    [ "Network Height    : " ++ show nodeStatusNetworkHeight
    , "Best Header       : " ++ cs (blockHashToHex nodeStatusBestHeader)
    , "Best Header Height: " ++ show nodeStatusBestHeaderHeight
    , "Best Block        : " ++ cs (blockHashToHex nodeStatusBestBlock)
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
    [ "LevelDB Lock      : " ++ show nodeStatusLevelDBLock | verbose ] ++
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
    [ "  ThreadId : " ++ peerStatusThreadId | verbose ] ++
    [ "  Merkles  : " ++ show peerStatusHaveMerkles | verbose ] ++
    [ "  Messages : " ++ show peerStatusHaveMessage | verbose ] ++
    [ "  Nonces   : " ++ show peerStatusPingNonces | verbose ] ++
    [ "  Reconnect: " ++ show t
    | t <- maybeToList peerStatusReconnectTimer, verbose
    ] ++
    [ "  Logs     : " | verbose ] ++
    [ "    - " ++ msg | msg <- maybe [] id peerStatusLog, verbose]


