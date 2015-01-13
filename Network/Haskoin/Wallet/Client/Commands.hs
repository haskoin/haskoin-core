module Network.Haskoin.Wallet.Client.Commands 
( cmdStart
, cmdStop
, cmdNewWallet
, cmdGetWallet
, cmdGetWallets
, cmdNewAcc
, cmdNewMS
, cmdNewRead
, cmdNewReadMS
, cmdAddKeys
, cmdGetAcc
, cmdAccList
, cmdList
, cmdPage
, cmdNew
, cmdLabel
, cmdTxList
, cmdTxPage
, cmdSend
, cmdSendMany
, cmdSignTx
, cmdImportTx
, cmdGetOffline
, cmdSignOffline
, cmdBalance
, cmdSpendable
, cmdGetProp
, cmdGetTx
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
import qualified Control.Monad.State as S (StateT, gets)

import Data.Maybe (listToMaybe, isNothing, fromJust, fromMaybe)
import Data.List (intersperse)
import qualified Data.Yaml as YAML (encode)
import qualified Data.Text as T (pack, unpack, splitOn)
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

type Handler = S.StateT ClientConfig IO

-- hw start [config] [--detach]
cmdStart :: [String] -> Handler ()
cmdStart configLs = do
    detach <- S.gets clientDetach
    liftIO $ runSPVServer (listToMaybe configLs) detach
    liftIO $ putStrLn "Process started"

-- hw stop [config] 
cmdStop :: [String] -> Handler ()
cmdStop configLs = liftIO $ do
    stopSPVServer $ listToMaybe configLs
    putStrLn "Process stopped"

cmdNewWallet :: [String] -> Handler ()
cmdNewWallet mnemonicLs = do
    newWalletWalletName <- S.gets clientWallet
    newWalletPassphrase <- S.gets clientPass
    sendZmq (PostWalletsR NewWallet{..}) $ \(MnemonicRes m) -> do
        putStrLn "Write down your seed:"
        putStrLn m
  where
    newWalletMnemonic = T.pack <$> (listToMaybe mnemonicLs)

cmdGetWallet :: Handler ()
cmdGetWallet = do
    w <- S.gets clientWallet
    sendZmq (GetWalletR w) $ putStr . printWallet

cmdGetWallets :: Handler ()
cmdGetWallets = sendZmq GetWalletsR $ \ws -> do
    let xs = map (putStr . printWallet) ws
    sequence_ $ intersperse (putStrLn "-") xs

cmdNewAcc :: String -> Handler ()
cmdNewAcc name = do
    w <- S.gets clientWallet
    sendZmq (PostAccountsR w newAcc) $ putStr . printAccount
  where
    newAcc = NewAccountRegular $ T.pack name

cmdNewMS :: String -> String -> String -> [String] -> Handler ()
cmdNewMS name mStr nStr ks = do
    when (isNothing keysM) $ error "Could not parse key(s)"
    w <- S.gets clientWallet
    sendZmq (PostAccountsR w newAcc) $ putStr . printAccount
  where
    m      = read mStr
    n      = read nStr
    newAcc = NewAccountMultisig (T.pack name) m n $ fromJust keysM
    keysM  = mapM xPubImport ks

cmdNewRead :: String -> String -> Handler ()
cmdNewRead name key = do
    when (isNothing keyM) $ error "Could not parse key"
    w <- S.gets clientWallet
    sendZmq (PostAccountsR w newAcc) $ putStr . printAccount
  where
    newAcc = NewAccountRead (T.pack name) $ fromJust keyM
    keyM = xPubImport key

cmdNewReadMS :: String -> String -> String -> [String] -> Handler ()
cmdNewReadMS name mStr nStr ks = do
    when (isNothing keysM) $ error "Could not parse key(s)"
    w <- S.gets clientWallet
    sendZmq (PostAccountsR w newAcc) $ putStr . printAccount
  where
    m      = read mStr
    n      = read nStr
    newAcc = NewAccountReadMultisig (T.pack name) m n $ fromJust keysM
    keysM  = mapM xPubImport ks

cmdAddKeys :: String -> [String] -> Handler ()
cmdAddKeys name ks = do
    when (isNothing keysM) $ error "Could not parse key(s)"
    w <- S.gets clientWallet
    sendZmq (PostAccountKeysR w (T.pack name) keys) $ putStr . printAccount
  where
    keysM  = mapM xPubImport ks
    keys   = fromJust keysM

cmdGetAcc :: String -> Handler ()
cmdGetAcc name = do
    w <- S.gets clientWallet
    sendZmq (GetAccountR w $ T.pack name) $ putStr . printAccount

cmdAccList :: Handler ()
cmdAccList = do
    w <- S.gets clientWallet
    sendZmq (GetAccountsR w) $ \as -> do
        let xs = map (putStr . printAccount) as
        sequence_ $ intersperse (putStrLn "-") xs

cmdList :: String -> Handler ()
cmdList name = do
    w <- S.gets clientWallet
    m <- S.gets clientMinConf
    i <- S.gets clientInternal
    sendZmq (GetAddressesR w (T.pack name) Nothing m i False False) $
        mapM_ (putStrLn . printBalanceAddress)

cmdPage :: String -> [String] -> Handler ()
cmdPage name pageLs = do
    w <- S.gets clientWallet
    m <- S.gets clientMinConf
    i <- S.gets clientInternal
    c <- S.gets clientCount
    let pagedRes = Just $ PagedResult page c
    sendZmq (GetAddressesR w (T.pack name) pagedRes m i False False) $
        \(AddressPageRes as maxPage) -> do
            -- page 0 is the last page
            let currPage = if page == 0 then maxPage else page
            putStrLn $ unwords [ "Page", show currPage, "of", show maxPage ]
            forM_ as $ putStrLn . printBalanceAddress
  where
    page = fromMaybe 0 (read <$> listToMaybe pageLs)

cmdNew :: String -> String -> Handler ()
cmdNew name label = do
    w <- S.gets clientWallet
    sendZmq (PostAddressesR w (T.pack name) addrData) $
        putStrLn . printLabeledAddress
  where
    addrData = AddressData $ T.pack label

cmdLabel :: String -> String -> String -> Handler ()
cmdLabel name iStr label = do
    w <- S.gets clientWallet
    sendZmq (PutAddressR w (T.pack name) i addrData) $
        putStrLn . printLabeledAddress
  where
    i        = read iStr
    addrData = AddressData $ T.pack label

cmdTxList :: String -> Handler ()
cmdTxList name = do
    w <- S.gets clientWallet
    sendZmq (GetTxsR w (T.pack name) Nothing) $ \ts -> do
        let xs = map (putStr . printAccTx) ts
        sequence_ $ intersperse (putStrLn "-") xs

cmdTxPage :: String -> [String] -> Handler ()
cmdTxPage name pageLs = do
    w <- S.gets clientWallet
    c <- S.gets clientCount
    let pagedRes = Just $ PagedResult page c
    sendZmq (GetTxsR w (T.pack name) pagedRes) $ \(TxPageRes ts maxPage) -> do
        -- page 0 is the last page
        let currPage = if page == 0 then maxPage else page
        putStrLn $ unwords [ "Page", show currPage, "of", show maxPage ]
        let xs = map (putStr . printAccTx) ts
        sequence_ $ intersperse (putStrLn "-") xs
  where
    page = fromMaybe 0 (read <$> listToMaybe pageLs)

cmdSend :: String -> String -> String -> Handler ()
cmdSend name addrStr amntStr = do
    when (isNothing addrM) $ error "Could not parse address"
    w <- S.gets clientWallet
    sendZmq (PostTxsR w (T.pack name) action) $ \(TxHashStatusRes h c) -> do
        putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
        putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
  where
    action = CreateTx [(fromJust addrM, amnt)]
    addrM = base58ToAddr addrStr
    amnt  = read amntStr

cmdSendMany :: String -> [String] -> Handler ()
cmdSendMany name xs = do
    when (isNothing rcpsM) $ error "Could not parse recipient list"
    w <- S.gets clientWallet
    sendZmq (PostTxsR w (T.pack name) action) $ \(TxHashStatusRes h c) -> do
        putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
        putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
  where
    action  = CreateTx (fromJust rcpsM)
    g str   = map T.unpack $ T.splitOn ":" (T.pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (return $ read v)
    f _     = Nothing
    rcpsM   = mapM (f . g) xs

cmdSignTx :: String -> String -> Handler ()
cmdSignTx name txStr = do
    when (isNothing txM) $ error "Could not parse transaction"
    w <- S.gets clientWallet
    sendZmq (PostTxsR w (T.pack name) action) $ \(TxHashStatusRes h c) -> do
        putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
        putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
  where
    action = SignTx $ fromJust txM
    txM = decodeToMaybe =<< hexToBS txStr

cmdImportTx :: String -> String -> Handler ()
cmdImportTx name txStr = do
    when (isNothing txM) $ error "Could not parse transaction"
    w <- S.gets clientWallet
    sendZmq (PostTxsR w (T.pack name) action) $ \(TxHashStatusRes h c) -> do
        putStrLn $ unwords [ "TxHash  :", encodeTxHashLE h]
        putStrLn $ unwords [ "Complete:", if c then "Yes" else "No"]
  where
    action = ImportTx $ fromJust txM
    txM = decodeToMaybe =<< hexToBS txStr

cmdGetOffline :: String -> String -> Handler ()
cmdGetOffline name tidStr = do
    when (isNothing tidM) $ error "Could not parse txid"
    w <- S.gets clientWallet
    sendZmq (GetOfflineTxDataR w (T.pack name) $ fromJust tidM) $ 
        \otd -> putStrLn $ bsToHex $ toStrictBS $ encode (otd :: OfflineTxData)
  where
    tidM = decodeTxHashLE tidStr

cmdSignOffline :: String -> String -> Handler ()
cmdSignOffline name otdStr = do
    when (isNothing otdM) $ error "Could not decode offline tx data"
    w <- S.gets clientWallet
    sendZmq (PostTxsR w (T.pack name) action) $ \(TxStatusRes tx c) -> do
        putStrLn $ unwords [ "Tx      :", bsToHex $ encode' tx ]
        putStrLn $ unwords [ "Complete:", if c then "Yes" else "No" ]
  where
    action = SignOfflineTxData $ fromJust otdM
    otdM = decode . toLazyBS =<< hexToBS otdStr 

cmdBalance :: String -> Handler ()
cmdBalance name = do
    w <- S.gets clientWallet
    m <- S.gets clientMinConf
    sendZmq (GetBalanceR w (T.pack name) m) $ \(BalanceRes b cs) -> do
        putStrLn $ unwords [ "Balance:", printBalance b ]
        unless (null cs) $ do
            putStrLn "Conflicts:"
            forM_ cs $ putStrLn . encodeTxHashLE

cmdSpendable :: String -> Handler ()
cmdSpendable name = do
    w <- S.gets clientWallet
    m <- S.gets clientMinConf
    sendZmq (GetSpendableR w (T.pack name) m) $ \(SpendableRes b) -> do
        putStrLn $ unwords [ "Spendable balance:", show b ]

cmdGetProp :: String -> String -> Handler ()
cmdGetProp name tidStr = do
    when (isNothing tidM) $ error "Could not parse txid"
    w <- S.gets clientWallet
    sendZmq (GetTxR w (T.pack name) (fromJust tidM) True) $ \AccTx{..} ->
        putStrLn $ bsToHex $ encode' accTxTx
  where
    tidM = decodeTxHashLE tidStr

cmdGetTx :: String -> String -> Handler ()
cmdGetTx name tidStr = do
    when (isNothing tidM) $ error "Could not parse txid"
    w <- S.gets clientWallet
    sendZmq (GetTxR w (T.pack name) (fromJust tidM) False) $ \AccTx{..} ->
        putStrLn $ bsToHex $ encode' accTxTx
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
    format <- S.gets clientFormat
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
    sockName <- S.gets clientSocket
    resE <- liftIO $ runZMQ $ do
        sock <- socket Req
        connect sock sockName
        send sock [] (toStrictBS $ encode req)
        eitherDecode . toLazyBS <$> receive sock
    case resE of
        Right (ResponseError err) -> error $ T.unpack err
        Right (ResponseValid a)   -> formatOutput a =<< S.gets clientFormat
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

