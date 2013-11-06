module Haskoin.Wallet.Commands where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either

import Data.Word
import Data.Yaml
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.Store
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

type Command = WalletDB (ResourceT IO) Value
type Args = [String]

{- YAML templates -}

instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object
        [ (T.pack "TxID") .= (bsToHex $ BS.reverse $ encode' h)
        , (T.pack "index") .= toJSON i
        ]

instance ToJSON TxOut where
    toJSON (TxOut v s) = object $
        [ (T.pack "Value") .= toJSON v
        , (T.pack "Raw Script") .= (bsToHex $ encodeScriptOps s)
        , (T.pack "Script") .= toJSON s
        ] ++ scptPair 
        where scptPair = 
                either (const [])
                       (\out -> [(T.pack "Decoded Script") .= toJSON out]) 
                       (decodeOutput s)

instance ToJSON TxIn where
    toJSON (TxIn o s i) = object $
        [ (T.pack "OutPoint") .= toJSON o
        , (T.pack "Sequence") .= toJSON i
        , (T.pack "Raw Script") .= (bsToHex $ encodeScriptOps s)
        , (T.pack "Script") .= toJSON s
        ] ++ decoded 
        where decoded = either (const $ either (const []) f $ decodeInput s) 
                               f $ decodeScriptHash s
              f inp = [(T.pack "Decoded Script") .= toJSON inp]
              
instance ToJSON Tx where
    toJSON tx@(Tx v is os i) = object 
        [ (T.pack "Transaction") .= object
            [ (T.pack "TxID") .= (bsToHex $ BS.reverse $ encode' $ txid tx)
            , (T.pack "Version") .= toJSON v
            , (T.pack "Inputs") .= (toJSON $ map input $ zip is [0..])
            , (T.pack "Outputs") .= (toJSON $ map output $ zip os [0..])
            , (T.pack "LockTime") .= toJSON i
            ]
        ]
        where input (x,j) = object 
                [(T.pack $ unwords ["Input", show j]) .= toJSON x]
              output (x,j) = object 
                [(T.pack $ unwords ["Output", show j]) .= toJSON x]

instance ToJSON Script where
    toJSON (Script ops) = toJSON $ map show ops

instance ToJSON ScriptOutput where
    toJSON (PayPK p) = object 
        [ (T.pack "PayToPublicKey") .= object
            [ (T.pack "Public Key") .= (bsToHex $ encode' p)
            ]
        ]
    toJSON (PayPKHash a) = object 
        [ (T.pack "PayToPublicKeyHash") .= object
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ runAddress a)
            , (T.pack "Address Base58") .= addrToBase58 a
            ]
        ]
    toJSON (PayMulSig ks r) = object 
        [ (T.pack "PayToMultiSig") .= object
            [ (T.pack "Required Keys (M)") .= toJSON r
            , (T.pack "Public Keys") .= (toJSON $ map (bsToHex . encode') ks)
            ]
        ]
    toJSON (PayScriptHash a) = object 
        [ (T.pack "PayToScriptHash") .= object
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ runAddress a)
            , (T.pack "Address Base58") .= addrToBase58 a
            ]
        ]

instance ToJSON ScriptInput where
    toJSON (SpendPK s) = object 
        [ (T.pack "SpendPublicKey") .= object
            [ (T.pack "Signature") .= (bsToHex $ encodeSig s)
            ]
        ]
    toJSON (SpendPKHash s p) = object 
        [ (T.pack "SpendPublicKeyHash") .= object
            [ (T.pack "Signature") .= (bsToHex $ encodeSig s)
            , (T.pack "Public Key") .= (bsToHex $ encode' p)
            ]
        ]
    toJSON (SpendMulSig sigs r) = object 
        [ (T.pack "SpendMultiSig") .= object
            [ (T.pack "Required Keys (M)") .= toJSON r
            , (T.pack "Signatures") .= (toJSON $ map (bsToHex . encodeSig) sigs)
            ]
        ]

instance ToJSON ScriptHashInput where
    toJSON (ScriptHashInput s r) = object
        [ (T.pack "SpendScriptHash") .= object
            [ (T.pack "ScriptInput") .= toJSON s
            , (T.pack "RedeemScript") .= toJSON r
            ]
        ]

yamlAcc :: DBAccount -> Value
yamlAcc acc 
    | isMSAcc acc = object 
        [ (T.pack "Name") .= accName aData
        , (T.pack "Type") .= unwords [ "Multisig", ms ]
        , (T.pack "Tree") .= dbAccTree acc
        , (T.pack "Addr count") .= (toJSON $ accExtCount aData)
        ]
    | otherwise   = object 
        [ (T.pack "Name") .= accName aData
        , (T.pack "Type") .= "Regular"
        , (T.pack "Tree") .= dbAccTree acc
        , (T.pack "Addr count") .= (toJSON $ accExtCount aData)
        ]
    where aData = runAccData acc
          ms    = unwords [ show $ msReq acc
                          , "of"
                          , show $length (msKeys acc) + 1
                          ]
    
yamlAddr :: DBAddress -> Value
yamlAddr a
    | null $ addrLabel a = object base
    | otherwise = object $ label:base
    where base  = [ (T.pack "Addr") .= addrBase58 a
                  , (T.pack "ID") .= (toJSON $ addrPos a) 
                  , (T.pack "Tree") .= dbAddrTree a
                  ]
          label = (T.pack "Label") .= addrLabel a

yamlAddrList :: [DBAddress] -> DBAccount -> Value
yamlAddrList addrs acc = toJSON 
    [ object [ (T.pack "Account") .= yamlAcc acc ]
    , object [ (T.pack "Addresses") .= (toJSON $ map yamlAddr addrs) ]
    ]

yamlCoin :: DBCoin -> DBAccount -> Value
yamlCoin (DBCoin (OutPoint tid i) (TxOut v s) _ _ p) acc = object $
    [ (T.pack "TxID") .= (bsToHex $ BS.reverse $ encode' tid)
    , (T.pack "Index") .= toJSON i
    , (T.pack "Value") .= toJSON v
    , (T.pack "Script") .= (bsToHex $ encodeScriptOps s)
    , (T.pack "Acc") .= (accName $ runAccData acc)
    ] ++ addrPair
    where addrPair = either (const []) 
                            (\a -> [(T.pack "Addr") .= addrToBase58 a])
                            (scriptRecipient s)

{- Commands -}

type AccountName = String

cmdInit :: String -> Command
cmdInit seed = do
    acc <- dbInit seed
    return $ yamlAcc acc

cmdList :: Int -> AccountName -> Command
cmdList count name = do
    acc <- dbGetAcc $ AccName name
    let total = accExtCount $ runAccData acc
        c     = min count total
        from  = total - c + 1
    addrs <- dbAddrList (accPos $ runAccData acc) from c False
    return $ yamlAddrList addrs acc

cmdListFrom :: Int -> Int -> AccountName -> Command
cmdListFrom from count name = do
    acc   <- dbGetAcc $ AccName name
    when (from > (accExtCount $ runAccData acc)) $ 
        left $ unwords ["cmdListFrom: From index not in wallet:", show from]
    addrs <- dbAddrList (accPos $ runAccData acc) from count False
    return $ yamlAddrList addrs acc

cmdListAll :: AccountName -> Command
cmdListAll name = do
    acc <- dbGetAcc $ AccName name
    let aData = runAccData acc
    addrs <- dbAddrList (accPos aData) 1 (accExtCount aData) False
    return $ yamlAddrList addrs acc

cmdNew :: String -> AccountName -> Command
cmdNew label name = do
    acc  <- dbGetAcc $ AccName name
    addr <- head <$> (dbGenAddr (accPos $ runAccData acc) 1 False)
    let newAddr = addr{ addrLabel = label }
    dbPutAddr newAddr
    newAcc <- dbGetAcc $ AccPos $ addrAccPos newAddr
    return $ yamlAddrList [newAddr] newAcc

cmdGenAddr :: Int -> AccountName -> Command
cmdGenAddr count name = do
    acc    <- dbGetAcc $ AccName name
    addrs  <- dbGenAddr (accPos $ runAccData acc) count False
    newAcc <- dbGetAcc $ AccPos $ accPos $ runAccData acc
    return $ yamlAddrList addrs newAcc

cmdFocus :: AccountName -> Command
cmdFocus name = do
    acc <- dbGetAcc $ AccName name
    dbPutConfig $ \cfg -> cfg{ cfgFocus = accName $ runAccData acc }
    return $ yamlAcc acc

cmdNewAcc :: AccountName -> Command
cmdNewAcc name = do
    acc <- dbNewAcc name
    return $ yamlAcc acc

cmdNewMS :: AccountName -> Int -> [String] -> Command
cmdNewMS name r xs = do
    keys <- mapM ((liftMaybe errKey) . xPubImport) xs
    acc <- dbNewMSAcc name r keys
    return $ yamlAcc acc
    where errKey = "cmdNewMS: Error importing extended public key"

cmdListAcc :: Command
cmdListAcc = do
    accs <- dbAccList
    return $ toJSON $ map yamlAcc accs

cmdLabel :: Int -> String -> AccountName -> Command
cmdLabel pos label name = do
    acc  <- dbGetAcc $ AccName name
    when (pos > (accExtCount $ runAccData acc)) $ 
        left $ unwords ["cmdLabel: Address index not in wallet:", show pos]
    addr <- dbGetAddr $ AddrExt (accPos $ runAccData acc) pos
    let newAddr = addr{ addrLabel = label }
    dbPutAddr newAddr
    return $ yamlAddrList [newAddr] acc

cmdBalance :: AccountName -> Command
cmdBalance name = do
    acc   <- dbGetAcc $ AccName name
    coins <- dbCoinList $ accPos $ runAccData acc
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins 
    return $ toJSON 
        [ object [ (T.pack "Account") .= yamlAcc acc ]
        , object [ (T.pack "Balance") .= toJSON (balance :: Word64) ]
        ]

cmdTotalBalance :: Command
cmdTotalBalance = do
    coins <- dbCoinListAll 
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins
    return $ object [ (T.pack "Total balance") .= toJSON (balance :: Word64) ] 

cmdDumpKey :: AccountName -> Command
cmdDumpKey name = do
    acc <- dbGetAcc $ AccName name
    mst <- dbGetConfig cfgMaster
    prv <- liftMaybe prvErr $ accPrvKey mst (accIndex $ runAccData acc)
    let prvKey = runAccPrvKey prv
        pubKey = deriveXPubKey prvKey
        msJson = map (toJSON . xPubExport) $ msKeys acc
        ms | isMSAcc acc = [object [(T.pack "MSKeys") .= toJSON msJson]]
           | otherwise   = []
    return $ toJSON $
        [ object [ (T.pack "Account") .= yamlAcc acc ]
        , object [ (T.pack "PubKey") .= xPubExport pubKey ]
        , object [ (T.pack "PrvKey") .= xPrvExport prvKey ]
        ] ++ ms
    where prvErr = "cmdDumpKey: Invalid private key derivation index"

cmdImportTx :: String -> Command
cmdImportTx str = do
    tx    <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    coins <- dbImportTx tx
    accs  <- mapM (dbGetAcc . AccPos . coinAccPos) coins
    let json = map (\(c,a) -> yamlCoin c a) $ zip coins accs
    return $ object
        [ (T.pack "Import count") .= (toJSON $ length coins) 
        , (T.pack "Imported coins") .= toJSON json
        ]
    where txErr = "cmdImportTx: Could not decode transaction"

cmdCoins :: AccountName -> Command
cmdCoins name = do
    acc   <- dbGetAcc $ AccName name
    coins <- dbCoinList $ accPos $ runAccData acc
    return $ toJSON
        [ object [(T.pack "Account") .= yamlAcc acc]
        , object [(T.pack "Coins") .= (toJSON $ map (flip yamlCoin acc) coins)]
        ]

cmdAllCoins :: Command
cmdAllCoins = do
    coins <- dbCoinListAll
    accs  <- mapM (dbGetAcc . AccPos . coinAccPos) coins
    return $ toJSON $ map (\(c,a) -> yamlCoin c a) $ zip coins accs

cmdDecodeTx :: String -> Command
cmdDecodeTx str = do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    return $ toJSON (tx :: Tx)
    where txErr = "cmdDecodeTx: Could not decode transaction"

cmdBuildTx :: [(String,Int)] -> [(String,Int)] -> Command
cmdBuildTx os as = do
    ops <- mapM f os
    tx  <- liftEither $ buildAddrTx ops $ map (\(a,v) -> (a,fromIntegral v)) as
    return $ object [ (T.pack "Hex Tx") .= (bsToHex $ encode' tx) ]
    where f (t,i) = do
            tid <- liftMaybe tidErr $ (decodeToMaybe . BS.reverse) =<< hexToBS t
            return $ OutPoint tid $ fromIntegral i
          tidErr  = "cmdBuildTx: Could not decode outpoint txid"

cmdSignTx :: String -> [(String,Int,String)] -> SigHash -> Command
cmdSignTx strTx xs sh = do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS strTx)
    ys <- mapRights f xs
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
        bsTx  = (bsToHex . encode') <$> sigTx
    return $ object [ (T.pack "Hex Tx") .= (toJSON $ runBuild bsTx)
                    , (T.pack "Complete") .= isComplete bsTx
                    ]
    where f (t,i,s) = do
            sBS <- liftMaybe "Invalid script HEX encoding" $ hexToBS s
            tBS <- liftMaybe "Invalid txid HEX encoding" $ hexToBS t
            scp <- liftEither $ decodeScriptOps sBS
            tid <- liftEither $ decodeToEither $ BS.reverse tBS
            dbGetSigData scp (OutPoint tid $ fromIntegral i) sh
          txErr = "cmdSignTx: Could not decode transaction"

