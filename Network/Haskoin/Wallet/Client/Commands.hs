module Network.Haskoin.Wallet.Client.Commands 
( cmdStart
, cmdStop
, cmdNewKeyRing
, cmdKeyRing
, cmdKeyRings
, cmdNewAcc
, cmdNewMS
, cmdNewRead
, cmdNewReadMS
, cmdAddKeys
, cmdSetGap
, cmdAccount
, cmdAccounts
, cmdList
, cmdUnused
, cmdLabel
, cmdTxs
, cmdSend
, cmdSendMany
, cmdImport
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
import Control.Monad (forM_, when, liftM2, unless)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.Reader as R (ReaderT, ask, asks)

import Data.Maybe (listToMaybe, isNothing, fromJust, fromMaybe)
import Data.List (intersperse)
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

import Network.Haskoin.Wallet.Model
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
    newKeyRingMnemonic = pack <$> (listToMaybe mnemonicLs)

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
    newAcc = NewAccount (pack name) AccountRegular [] Nothing Nothing

cmdNewMS :: String -> String -> String -> [String] -> Handler ()
cmdNewMS name mStr nStr ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) AccountMultisig keys m n
        sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
    _ -> error "Could not parse key(s)"
  where
    m     = Just $ read mStr
    n     = Just $ read nStr
    keysM = mapM xPubImport ks

cmdNewRead :: String -> String -> Handler ()
cmdNewRead name keyStr = case keyM of
    Just key -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) AccountRead [key] Nothing Nothing
        sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
    _ -> error "Could not parse key"
  where
    keyM = xPubImport keyStr

cmdNewReadMS :: String -> String -> String -> [String] -> Handler ()
cmdNewReadMS name mStr nStr ks = case keysM of
    Just keys -> do
        k <- R.asks configKeyRing
        let newAcc = NewAccount (pack name) AccountReadMultisig keys m n 
        sendZmq (PostAccountsR k newAcc) $ putStr . printAccount
    _ -> error "Could not parse key(s)"
  where
    m     = Just $ read mStr
    n     = Just $ read nStr
    keysM = mapM xPubImport ks

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

cmdList :: String -> [String] -> Handler ()
cmdList name pageLs = do
    k <- R.asks configKeyRing
    t <- R.asks configAddrType
    c <- R.asks configCount
    r <- R.asks configReversePaging
    let pageReq = PageRequest page c r
    sendZmq (GetAddressesR k (pack name) t pageReq) $ \(PageRes as m) -> do
        putStrLn $ unwords [ "Page", show page, "of", show m ]
        forM_ as $ putStrLn . printAddress
  where
    page = fromMaybe 1 (read <$> listToMaybe pageLs)

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
    c <- R.asks configCount
    r <- R.asks configReversePaging
    let pageReq = PageRequest page c r
    sendZmq (GetTxsR k (pack name) pageReq) $ \(PageRes ts m) -> do
        putStrLn $ unwords [ "Page", show page, "of", show m ]
        let xs = map (putStr . printTx) ts
        sequence_ $ intersperse (putStrLn "-") xs
  where
    page = fromMaybe 1 (read <$> listToMaybe pageLs)

cmdSend :: String -> String -> String -> Handler ()
cmdSend name addrStr amntStr = case addrM of
    Just addr -> do
        k       <- R.asks configKeyRing
        fee     <- R.asks configFee
        minconf <- R.asks configMinConf
        sign    <- R.asks configSignTx
        let action = CreateTx [(addr, amnt)] fee minconf sign
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse address"
  where
    addrM = base58ToAddr addrStr
    amnt  = read amntStr

cmdSendMany :: String -> [String] -> Handler ()
cmdSendMany name xs = case rcpsM of
    Just rcps -> do
        k       <- R.asks configKeyRing
        fee     <- R.asks configFee
        minconf <- R.asks configMinConf
        sign    <- R.asks configSignTx
        let action = CreateTx rcps fee minconf sign
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse recipient list"
  where
    g str   = map unpack $ splitOn ":" (pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read v)
    f _     = Nothing
    rcpsM   = mapM (f . g) xs

cmdImport :: String -> String -> Handler ()
cmdImport name txStr = case txM of
    Just tx -> do
        k <- R.asks configKeyRing
        sign <- R.asks configSignTx
        let action = ImportTx tx sign
        sendZmq (PostTxsR k (pack name) action) $ 
            \(TxHashConfidenceRes h c) -> do
                putStrLn $ unwords [ "TxHash    :", encodeTxHashLE h ]
                putStrLn $ unwords [ "Confidence:", printConfidence c ]
    _ -> error "Could not parse transaction"
  where
    txM = decodeToMaybe =<< hexToBS txStr

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
        sendZmq (PostTxsR k (pack name) action) $ \(TxCompleteRes tx c) -> do
            putStrLn $ unwords [ "Tx      :", bsToHex $ encode' tx ]
            putStrLn $ unwords [ "Complete:", if c then "Yes" else "No" ]
    _ -> error "Could not decode input data"
  where
    datM = decode . toLazyBS =<< hexToBS datStr 
    txM  = decodeToMaybe =<< hexToBS txStr

cmdBalance :: String -> Handler ()
cmdBalance name = do
    k <- R.asks configKeyRing
    m <- R.asks configMinConf
    sendZmq (GetBalanceR k (pack name) m) $ \(BalanceRes b) -> do
        putStrLn $ unwords [ "Balance:", show b ]

cmdOfflineBalance :: String -> Handler ()
cmdOfflineBalance name = do
    k <- R.asks configKeyRing
    sendZmq (GetOfflineBalanceR k $ pack name) $ \(BalanceRes b) -> do
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
cmdRescan timeLs = do
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
        Right (ResponseError err) -> error $ unpack err
        Right (ResponseValid a)   -> formatOutput a =<< R.asks configFormat
        Left err                  -> error err
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
    , "inputs"   .= map input (zip is [0..])
    , "outputs"  .= map output (zip os [0..])
    , "locktime" .= i
    ]
  where 
    input (x,j) = object 
      [pack ("input " ++ show (j :: Int)) .= encodeTxInJSON x]
    output (x,j) = object 
      [pack ("output " ++ show (j :: Int)) .= encodeTxOutJSON x]

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
printKeyRing JsonKeyRing{..} = unlines
    [ "KeyRing    : " ++ unpack jsonKeyRingName
    , "Master key : " ++ xPrvExport jsonKeyRingMaster
    ]

printAccount :: JsonAccount -> String
printAccount JsonAccount{..} = unlines $
    [ "Account: " ++ unpack jsonAccountName
    , "Keyring: " ++ unpack jsonAccountKeyRingName
    , "Type   : " ++ showType
    , "Gap    : " ++ show jsonAccountGap
    ] ++ maybe [] (\d -> ["Deriv  : " ++ show d]) jsonAccountDerivation 
      ++ if null jsonAccountKeys then [] else 
        ( "Keys   : " ++ xPubExport (head jsonAccountKeys) ) : 
        ( map (\x -> "         " ++ xPubExport x) $ tail jsonAccountKeys )
  where
    showType = case jsonAccountType of
        AccountRegular -> "Regular"
        AccountMultisig -> unwords
            [ "Multisig"
            , show $ fromJust jsonAccountRequiredSigs
            , "of"
            , show $ fromJust jsonAccountTotalKeys
            ]
        AccountRead -> "Read-only"
        AccountReadMultisig -> unwords
            [ "Read-only Multisig"
            , show $ fromJust jsonAccountRequiredSigs
            , "of"
            , show $ fromJust jsonAccountTotalKeys
            ]

printAddress :: JsonAddr -> String
printAddress JsonAddr{..} = unwords $
    [ show jsonAddrIndex, ":", addrToBase58 jsonAddrAddress ]
    ++ ( if null (unpack jsonAddrLabel) 
           then [] 
           else [ "(" ++ unpack jsonAddrLabel ++ ")" ]
       )
    ++ ( if jsonAddrInOfflineBalance == 0 
           then [] 
           else [ "[Received: " ++ show jsonAddrInOfflineBalance ++ "]" ]
       )

printTx :: JsonTx -> String
printTx JsonTx{..} = unlines $
    [ "Value        : " ++ printTxType jsonTxType ++ " " ++ show jsonTxValue ] 
    ++ ( if jsonTxType == TxIncoming
           then [ "Sender(s)    : " ++ printAddrList jsonTxFrom ]
           else []
       ) 
    ++
    [
      "Recipient(s) : " ++ printAddrList jsonTxTo
    , "Confidence   : " 
        ++ printConfidence jsonTxConfidence
        ++ if jsonTxConfidence == TxOffline then "" else
            " (Confirmations: " ++ show jsonTxConfirmations ++ ")"
    ] 
  where
    printAddrList xs = concat (intersperse ", " $ map addrToBase58 xs)

printConfidence :: TxConfidence -> String
printConfidence c = case c of
    TxBuilding -> "Building"
    TxPending  -> "Pending"
    TxDead     -> "Dead"
    TxOffline  -> "Offline"

printTxType :: TxType -> String
printTxType t = case t of
    TxIncoming -> "Incoming"
    TxOutgoing -> "Outgoing"
    TxSelf     -> "Self"
    
