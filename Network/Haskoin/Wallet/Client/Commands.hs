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
, cmdSend
, cmdSendMany
, cmdImport
, cmdSign
, cmdBalance
, cmdOfflineBalance
, cmdGetTx
, cmdGetOffline
, cmdSignOffline
, cmdRescan
, cmdDecodeTx
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

import Control.Applicative ((<$>))
import Control.Monad (forM_, when, liftM2)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.Reader as R (ReaderT, ask, asks)

import Data.Maybe (listToMaybe, isNothing, fromJust, fromMaybe, isJust)
import Data.List (intersperse, intercalate)
import Data.Text (pack, unpack, splitOn)
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

import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server

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
    newKeyRingKeyRingName <- R.asks configKeyRing
    newKeyRingPassphrase  <- R.asks configPass
    sendZmq (PostKeyRingsR NewKeyRing{..}) $ \(MnemonicRes m) -> do
        putStrLn "Write down your seed:"
        putStrLn m
  where
    newKeyRingMnemonic = pack <$> listToMaybe mnemonicLs

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
    sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
  where
    newAcc = NewAccount (pack name) (AccountRegular False) []

-- First argument: is account read-only?
cmdNewMS :: Bool -> String -> String -> String -> [String] -> Handler ()
cmdNewMS r name mStr nStr ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) (AccountMultisig r m n) keys
        sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
    _ -> error "Could not parse key(s)"
  where
    m     = read mStr
    n     = read nStr
    keysM = mapM xPubImport ks

cmdNewRead :: String -> String -> Handler ()
cmdNewRead name keyStr = case keyM of
    Just key -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) (AccountRegular True) [key]
        sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
    _ -> error "Could not parse key"
  where
    keyM = xPubImport keyStr

cmdAddKeys :: String -> [String] -> Handler ()
cmdAddKeys name ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        sendZmq (PostAccountKeysR k (pack name) keys) $ putStr . printAccount
    _ -> error "Could not parse key(s)"
  where
    keysM = mapM xPubImport ks

cmdSetGap :: String -> String -> Handler ()
cmdSetGap name gap = do
    k <- R.asks configKeyRing
    sendZmq (PostAccountGapR k (pack name) setGap) $ putStr . printAccount
  where
    setGap = SetAccountGap $ read gap 

cmdAccount :: String -> Handler ()
cmdAccount name = do
    k <- R.asks configKeyRing
    sendZmq (GetAccountR k $ pack name) $ putStr . printAccount

cmdAccounts :: Handler ()
cmdAccounts = do
    k <- R.asks configKeyRing
    sendZmq (GetAccountsR k) $ \as -> do
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
    sendZmq (requestBuilder pageReq) $ \(PageRes a m) -> do
        putStrLn $ unwords [ "Page", show page, "of", show m ]
        action a
  where
    page = fromMaybe 1 (read <$> listToMaybe pageLs)


cmdList :: String -> [String] -> Handler ()
cmdList name pageLs = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    let f = GetAddressesR k (pack name) t
    pagedAction pageLs f $ \as -> forM_ as (putStrLn . printAddress)

cmdUnused :: String -> Handler ()
cmdUnused name = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    sendZmq (GetAddressesUnusedR k (pack name) t) $ 
        \as -> forM_ as $ putStrLn . printAddress

cmdLabel :: String -> String -> String -> Handler ()
cmdLabel name iStr label = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    sendZmq (PutAddressR k (pack name) i t addrLabel) $ putStrLn . printAddress
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
    pagedAction pageLs (GetAddrTxsR k (pack name) index t) $ \ts -> do
        let xs = map (putStr . printAddrTx) ts
        sequence_ $ intersperse (putStrLn "-") xs
  where
    page  = fromMaybe 1 (read <$> listToMaybe pageLs)
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
        let action = CreateTx rcps fee rcptFee minconf sign
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse recipient information"
  where
    g str   = map unpack $ splitOn ":" (pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read v)
    f _     = Nothing
    rcpsM   = mapM (f . g) xs

cmdImport :: String -> String -> Handler ()
cmdImport name txStr = case txM of
    Just tx -> do
        k <- R.asks configKeyRing
        let action = ImportTx tx 
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse transaction"
  where
    txM = decodeToMaybe =<< hexToBS txStr

cmdSign :: String -> String -> Handler ()
cmdSign name txidStr = case txidM of
    Just txid -> do
        k <- R.asks configKeyRing
        let action = SignTx txid
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse txid"
  where
    txidM = decodeTxHashLE txidStr

cmdGetOffline :: String -> String -> Handler ()
cmdGetOffline name tidStr = case tidM of
    Just tid -> do
        k <- R.asks configKeyRing
        sendZmq (GetOfflineTxR k (pack name) tid) $ 
            \(OfflineTxData tx dat) -> do
                putStrLn $ unwords 
                    [ "Tx      :", bsToHex $ encode' tx ]
                putStrLn $ unwords 
                    [ "CoinData:", bsToHex $ toStrictBS $ encode dat ]
    _ -> error "Could not parse txid"
  where
    tidM = decodeTxHashLE tidStr

cmdSignOffline :: String -> String -> String -> Handler ()
cmdSignOffline name txStr datStr = case (txM, datM) of
    (Just tx, Just dat) -> do
        k <- R.asks configKeyRing
        let action = SignOfflineTx tx dat
        sendZmq (PostTxsR k (pack name) action) $ \(TxCompleteRes tx' c) -> do
            putStrLn $ unwords [ "Tx      :", bsToHex $ encode' tx' ]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No" ]
    _ -> error "Could not decode input data"
  where
    datM = decode . toLazyBS =<< hexToBS datStr 
    txM  = decodeToMaybe =<< hexToBS txStr

cmdBalance :: String -> Handler ()
cmdBalance name = do
    k <- R.asks configKeyRing
    m <- R.asks configMinConf
    sendZmq (GetBalanceR k (pack name) m) $ \(BalanceRes b) ->
        putStrLn $ unwords [ "Balance:", show b ]

cmdOfflineBalance :: String -> Handler ()
cmdOfflineBalance name = do
    k <- R.asks configKeyRing
    sendZmq (GetOfflineBalanceR k $ pack name) $ \(BalanceRes b) ->
        putStrLn $ unwords [ "Offline Balance:", show b ]

cmdGetTx :: String -> String -> Handler ()
cmdGetTx name tidStr = case tidM of
    Just tid -> do
        k <- R.asks configKeyRing
        sendZmq (GetTxR k (pack name) tid) $ \tx@JsonTx{..} -> do
            putStr $ printTx tx 
            putStrLn $ unwords 
                [ "Tx           :", bsToHex $ encode' jsonTxTx ]
    _ -> error "Could not parse txid"
  where
    tidM = decodeTxHashLE tidStr

cmdRescan :: [String] -> Handler ()
cmdRescan timeLs =
    sendZmq (PostNodeR $ Rescan timeM) $ \(RescanRes ts) ->
        putStrLn $ unwords [ "Timestamp:", show ts]
  where
    timeM = read <$> listToMaybe timeLs

cmdDecodeTx :: String -> Handler ()
cmdDecodeTx txStr = do
    when (isNothing txM) $ error "Could not parse transaction"
    format <- R.asks configFormat
    liftIO $ formatStr $ bsToString $ case format of
        OutputJSON -> toStrictBS jsn
        _          -> YAML.encode val
  where
    txM = decodeToMaybe =<< hexToBS txStr
    val = encodeTxJSON $ fromJust txM
    jsn = JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } val

{- Helpers -}

sendZmq :: (FromJSON a, ToJSON a) 
        => WalletRequest -> (a -> IO ()) -> Handler ()
sendZmq req handle = do
    sockName <- R.asks configConnect
    resE <- liftIO $ runZMQ $ do
        sock <- socket Req
        connect sock sockName
        send sock [] (toStrictBS $ encode req)
        eitherDecode . toLazyBS <$> receive sock
    case resE of
        Right (ResponseValid (Just a)) -> formatOutput a =<< R.asks configFormat
        Right (ResponseValid Nothing)  -> return ()
        Right (ResponseError err)      -> error $ unpack err
        Left err                       -> error err
  where
    formatOutput a format = liftIO $ case format of
        OutputJSON   -> formatStr $ bsToString $ toStrictBS $
            JSON.encodePretty' JSON.defConfig{ JSON.confIndent = 2 } a
        OutputYAML   -> formatStr $ 
            bsToString $ YAML.encode a
        OutputNormal -> handle a

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

encodeTxJSON :: Tx -> Value
encodeTxJSON tx@(Tx v is os i) = object
    [ "txid"     .= encodeTxHashLE (txHash tx)
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
    , "raw-script" .= bsToHex s
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
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
    f (OP_PUSHDATA bs _) = String $ pack $ unwords 
        ["OP_PUSHDATA", bsToHex bs]
    f x = String $ pack $ show x

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

{- Print utilities -}
 
printKeyRing :: JsonKeyRing -> String
printKeyRing JsonKeyRing{..} = unlines $
    [ "KeyRing    : " ++ unpack jsonKeyRingName ]
    ++
    [ "Master key : " ++ xPrvExport (fromJust jsonKeyRingMaster) 
    | isJust jsonKeyRingMaster
    ]

printAccount :: JsonAccount -> String
printAccount JsonAccount{..} = unlines $
    [ "Account: " ++ unpack jsonAccountName
    , "Type   : " ++ showType
    , "Gap    : " ++ show jsonAccountGap
    ] 
    ++
    [ "Deriv  : " ++ show (fromJust jsonAccountDerivation) 
    | isJust jsonAccountDerivation
    ]
    ++
    [ "Keys   : " ++ unlines
        ( [] ++ map (\x -> "         " ++ xPubExport x) jsonAccountKeys )
    | not (null jsonAccountKeys)
    ]
  where
    showType = case jsonAccountType of
        AccountRegular r -> if r then "Read-Only" else "Regular"
        AccountMultisig r m n -> unwords
            [ if r then "Read-Only Multisig" else "Multisig"
            , show m, "of", show n
            ]

printAddress :: JsonAddr -> String
printAddress JsonAddr{..} = unwords $
    [ show jsonAddrIndex, ":", addrToBase58 jsonAddrAddress ]
    ++ 
    [ "(" ++ unpack jsonAddrLabel ++ ")" | not (null $ unpack jsonAddrLabel) ]
    ++ concat 
    ( [ [ "[Received: "     ++ show (addrBalanceInBalance bal)   ++ "]"
        , "[Funding Txs: "  ++ show (addrBalanceFundingTxs bal)  ++ "]"
        , "[Spending Txs: " ++ show (addrBalanceSpendingTxs bal) ++ "]"
        ] 
        | isJust jsonAddrOfflineBalance
      ]
    )
  where
    bal = fromJust jsonAddrOfflineBalance

printTx :: JsonTx -> String
printTx tx@JsonTx{..} = unlines $
    [ "Value      : " ++ printTxType jsonTxType ++ " " ++ show jsonTxValue ] 
    ++
    [ "Inputs     : " ++ printAddrInfos jsonTxInputs 
    | not (null jsonTxInputs) 
    ]
    ++
    [ "Outputs    : " ++ printAddrInfos jsonTxOutputs 
    | not (null jsonTxOutputs) 
    ]
    ++
    [ "Change     : " ++ printAddrInfos jsonTxChange
    | not (null jsonTxChange) 
    ]
    ++
    [ "Confidence : " ++ printTxConfidence tx ] 
  where
    printAddrInfos xs = unlines $ map f xs
    f (AddressInfo addr valM local) = unwords $
        [ if local then "   <-" else "     "
        , addrToBase58 addr 
        ] ++ [ show $ fromJust valM | isJust valM ]

printAddrTx :: JsonAddrTx -> String
printAddrTx JsonAddrTx{..} = unlines $
    [ "Value        : " 
        ++ printAddrTxType jsonAddrTxType ++ " " 
        ++ show jsonAddrTxValue 
    ] ++
    [ "Confidence   : " ++ printTxConfidence (fromJust jsonAddrTxTx)
    | isJust jsonAddrTxTx
    ] 

printConfidence :: TxConfidence -> String
printConfidence c = case c of
    TxBuilding -> "Building"
    TxPending  -> "Pending"
    TxDead     -> "Dead"
    TxOffline  -> "Offline"

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

printAddrTxType :: AddrTxType -> String
printAddrTxType t = case t of
    AddrTxIncoming -> "Incoming"
    AddrTxOutgoing -> "Outgoing"
    AddrTxChange   -> "Change"
    
