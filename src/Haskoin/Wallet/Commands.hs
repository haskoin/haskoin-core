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

type Command = WalletDB (ResourceT IO) Value
type Args = [String]

{- YAML templates -}

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

{- Helpers -}

whenArgs :: Args -> (Int -> Bool) -> Command -> Command
whenArgs args f cmd = if f $ length args then cmd else argErr
    where argErr = left "Invalid number of arguments"

-- Return the account from the arguments, or get the current focused account
focusedAcc :: Args -> WalletDB (ResourceT IO) DBAccount
focusedAcc args = case args of
    []      -> (dbGetAcc . AccPos) =<< dbGetConfig cfgFocus
    name:[] -> dbGetAcc $ AccName name

{- Commands -}

cmdInit :: Options -> Args -> Command
cmdInit opts args = whenArgs args (== 1) $ do
    acc   <- dbInit $ head args
    addrs <- dbGenAddr (accPos $ runAccData acc) (optCount opts) False
    return $ yamlAddrList addrs acc

cmdList :: Options -> Args -> Command
cmdList opts args = whenArgs args (<= 1) $ do
    acc <- focusedAcc args
    let total = accExtCount $ runAccData acc
        count = min (optCount opts) total
        from  = total - count + 1
    addrs <- dbAddrList (accPos $ runAccData acc) from count False
    return $ yamlAddrList addrs acc

cmdListFrom :: Options -> Args -> Command
cmdListFrom opts args = whenArgs args (<= 2) $ do
    acc <- focusedAcc $ drop 1 args
    let from = read $ args !! 0
    addrs <- dbAddrList (accPos $ runAccData acc) from (optCount opts) False
    return $ yamlAddrList addrs acc

cmdListAll :: Options -> Args -> Command
cmdListAll opts args = whenArgs args (<= 1) $ do
    acc <- focusedAcc args
    let aData = runAccData acc
    addrs <- dbAddrList (accPos aData) 1 (accExtCount aData) False
    return $ yamlAddrList addrs acc

cmdNew :: Options -> Args -> Command
cmdNew opts args = whenArgs args (<= 2) $ do
    acc  <- focusedAcc $ drop 1 args
    addr <- head <$> (dbGenAddr (accPos $ runAccData acc) 1 False)
    let newAddr = addr{ addrLabel = args !! 0 }
    dbPutAddr newAddr
    newAcc <- dbGetAcc $ AccPos $ addrAccPos newAddr
    return $ yamlAddrList [newAddr] newAcc

cmdGenAddr :: Options -> Args -> Command
cmdGenAddr opts args = whenArgs args (<= 1) $ do
    acc   <- focusedAcc args
    addrs <- dbGenAddr (accPos $ runAccData acc) (optCount opts) False
    newAcc <- dbGetAcc $ AccPos $ accPos $ runAccData acc
    return $ yamlAddrList addrs newAcc

cmdFocus :: Options -> Args -> Command
cmdFocus opts args = whenArgs args (== 1) $ do
    acc <- dbGetAcc $ AccName $ head args
    dbPutConfig $ \cfg -> cfg{ cfgFocus = accPos $ runAccData acc }
    cmdList opts args

cmdNewAcc :: Options -> Args -> Command
cmdNewAcc opts args = whenArgs args (== 1) $ do
    dbNewAcc $ head args
    cmdGenAddr opts args

cmdNewMS :: Options -> Args -> Command
cmdNewMS opts args = whenArgs args (>= 3) $ do
    keys <- mapM ((liftMaybe errKey) . xPubImport) $ drop 2 args
    dbNewMSAcc (head args) (read $ args !! 1) keys
    cmdGenAddr opts [head args]
    where errKey = "cmdNewMS: Error importing extended public key"

cmdListAcc :: Options -> Args -> Command
cmdListAcc opts args = whenArgs args (== 0) $ do
    accs <- dbAccList
    return $ toJSON $ map yamlAcc accs

cmdLabel :: Options -> Args -> Command
cmdLabel opts args = whenArgs args (<= 3) $ do
    acc  <- focusedAcc $ drop 2 args
    addr <- dbGetAddr $ AddrExt (accPos $ runAccData acc) (read $ head args)
    let newAddr = addr{ addrLabel = args !! 1 }
    dbPutAddr newAddr
    return $ yamlAddrList [newAddr] acc

cmdBalance :: Options -> Args -> Command
cmdBalance opts args = whenArgs args (<= 1) $ do
    acc   <- focusedAcc args
    coins <- dbCoinList $ accPos $ runAccData acc
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins 
    return $ toJSON 
        [ object [ (T.pack "Account") .= yamlAcc acc ]
        , object [ (T.pack "Balance") .= toJSON (balance :: Word64) ]
        ]

cmdTotalBalance :: Options -> Args -> Command
cmdTotalBalance opts args = whenArgs args (== 0) $ do
    coins <- dbCoinListAll 
    let balance = sum $ map (fromIntegral . outValue . coinTxOut) coins
    return $ object [ (T.pack "Total balance") .= toJSON (balance :: Word64) ] 

cmdDumpKey :: Options -> Args -> Command
cmdDumpKey opts args = whenArgs args (<= 1) $ do
    acc <- focusedAcc args
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

cmdImportTx :: Options -> Args -> Command
cmdImportTx opts args = whenArgs args (== 1) $ do
    tx    <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args)
    coins <- dbImportTx tx
    accs  <- mapM (dbGetAcc . AccPos . coinAccPos) coins
    let json = map (\(c,a) -> yamlCoin c a) $ zip coins accs
    return $ object
        [ (T.pack "Import count") .= (toJSON $ length coins) 
        , (T.pack "Imported coins") .= toJSON json
        ]
    where txErr = "cmdImportTx: Could not decode transaction"

cmdCoins :: Options -> Args -> Command
cmdCoins opts args = whenArgs args (<= 1) $ do
    acc   <- focusedAcc args
    coins <- dbCoinList $ accPos $ runAccData acc
    return $ toJSON
        [ object [(T.pack "Account") .= yamlAcc acc]
        , object [(T.pack "Coins") .= (toJSON $ map (flip yamlCoin acc) coins)]
        ]

cmdAllCoins :: Options -> Args -> Command
cmdAllCoins opts args = whenArgs args (== 0) $ do
    coins <- dbCoinListAll
    accs  <- mapM (dbGetAcc . AccPos . coinAccPos) coins
    let json = map (\(c,a) -> yamlCoin c a) $ zip coins accs
    return $ toJSON json

cmdDecodeTx :: Options -> Args -> Command
cmdDecodeTx opts args = whenArgs args (== 1) $ do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args) 
    liftIO $ forM_ (lines $ ppShow (tx :: Tx)) putStrLn
    return Null
    where txErr = "cmdDecodeTx: Could not decode transaction"

cmdBuildTx :: Options -> Args -> Command
cmdBuildTx opts args = whenArgs args (>= 2) $ do
    ops <- mapM f os
    ads <- mapM g as
    tx  <- liftEither $ buildAddrTx ops ads
    return $ object [ (T.pack "Hex Tx") .= (bsToHex $ encode' tx) ]
    where xs      = map (splitOn ":") args
          (os,as) = span ((== 64) . length . head) xs
          f [t,i] = do
            tid <- liftMaybe tidErr $ (decodeToMaybe . BS.reverse) =<< hexToBS t
            return $ OutPoint tid $ read i
          f _     = left "Invalid syntax for txid:index"
          g [a,v] = return (a,read v)
          g _     = left "Invalid syntax for address:amount"
          tidErr  = "cmdBuildTx: Could not decode outpoint txid"

cmdSignTx :: Options -> Args -> Command
cmdSignTx opts args = whenArgs args (>= 2) $ do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS $ head args)
    ys <- mapRights f (map (splitOn ":") $ tail args)
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
        bsTx  = (bsToHex . encode') <$> sigTx
    return $ object [ (T.pack "Hex Tx") .= runBuild bsTx
                    , (T.pack "Complete" .= isComplete bsTx)
                    ]
    where f [t,i,s] = do
            sBS <- liftMaybe "Invalid script HEX encoding" $ hexToBS s
            tBS <- liftMaybe "Invalid txid HEX encoding" $ hexToBS t
            scp <- liftEither $ decodeScriptOps sBS
            tid <- liftEither $ decodeToEither $ BS.reverse tBS
            dbGetSigData scp (OutPoint tid $ read i) $ optSigHash opts
          f _ = left "Invalid syntax for txid:index:script"
          txErr = "cmdSignTx: Could not decode transaction"

