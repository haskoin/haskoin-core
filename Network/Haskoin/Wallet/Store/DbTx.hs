{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Store.DbTx
( dbImportTx
, dbRemoveTx
, dbSendTx
, dbSendSolution
, dbSendCoins
, dbSignTx
, yamlTx
, RawTxOutPoints(..)
, RawTxDests(..)
, RawSigInput(..)
, RawPrvKey(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, unless, when, mzero)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT, left)

import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Data.List ((\\), nub)
import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import Data.Either (rights)
import Data.Yaml 
    ( Value
    , object 
    , (.=), (.:), (.:?)
    , parseJSON
    , Parser
    )
import qualified Data.Aeson as Json 
    ( FromJSON
    , withObject
    , withArray
    )
import qualified Data.Vector as V (toList)
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Map.Strict as M 
    ( toList
    , empty
    , lookup
    , insert
    )
import qualified Data.Text as T (pack, unpack)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , entityKey
    , get
    , getBy
    , selectList
    , deleteWhere
    , updateWhere
    , update
    , insert_
    , insertUnique
    , replace
    , (=.), (==.), (<-.)
    )
import Database.Persist.Sql (SqlBackend)

import Network.Haskoin.Wallet.Keys
import Network.Haskoin.Wallet.Manager
import Network.Haskoin.Wallet.TxBuilder
import Network.Haskoin.Wallet.Store.DbAccount
import Network.Haskoin.Wallet.Store.DbAddress
import Network.Haskoin.Wallet.Store.DbCoin
import Network.Haskoin.Wallet.Store.Util
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Util.BuildMonad

yamlTx :: DbTxGeneric b -> Value
yamlTx tx = object $ concat
    [ [ "Recipients" .= dbTxRecipients tx
      , "Value" .= dbTxValue tx
      ]
    , if dbTxOrphan tx then ["Orphan" .= True] else []
    , if dbTxPartial tx then ["Partial" .= True] else []
    ]

-- |Remove a transaction from the database and any parent transaction
dbRemoveTx :: PersistQuery m => String -> EitherT String m [String]
dbRemoveTx tid = do
    -- Find all parents of this transaction
    -- Partial transactions should not have any coins. Won't check for it
    coins <- selectList [ DbCoinTxid ==. tid ] []
    let parents = nub $ catStatus $ map (dbCoinStatus . entityVal) coins
    -- Recursively remove parents
    pids <- forM parents dbRemoveTx
    -- Delete output coins generated from this transaction
    deleteWhere [ DbCoinTxid ==. tid ]
    -- Delete account transactions
    deleteWhere [ DbTxTxid ==. tid ]
    -- Delete transaction blob
    deleteWhere [ DbTxBlobTxid ==. tid ]
    -- Delete orphaned input coins spent by this transaction
    deleteWhere [ DbCoinOrphan ==. True
                , DbCoinStatus <-. [Spent tid, Reserved tid]
                ]
    -- Unspend input coins that were previously spent by this transaction
    updateWhere [ DbCoinStatus <-. [Spent tid, Reserved tid] ]
                [ DbCoinStatus =. Unspent ]
    return $ tid:(concat pids)
          
-- |Import a transaction into the database
dbImportTx :: ( PersistQuery m, PersistUnique m
              , PersistMonadBackend m ~ SqlBackend
              ) 
           => Tx -> EitherT String m [DbTxGeneric SqlBackend]
dbImportTx tx = do
    coinsM <- mapM (getBy . f) $ map prevOutput $ txIn tx
    let inCoins = catMaybes coinsM
        -- Unspent coins in the wallet related to this transaction
        unspent = filter ((== Unspent) . dbCoinStatus . entityVal) inCoins
        -- OutPoints from this transaction with no associated coins
        unknown = map snd $ filter (isNothing . fst) $ zip coinsM $ txIn tx
    -- Fail if an input is spent by a transaction which is not this one
    unless (isImportValid tid $ map entityVal inCoins) $ left
        "dbImportTx: Double spend detected. Import failed"
    let toRemove = txToRemove $ map entityVal inCoins
    if length toRemove > 0
        then do
            -- Partial transactions need to be removed 
            -- before trying to re-import
            _ <- mapM dbRemoveTx toRemove
            dbImportTx tx
        else do
            time <- liftIO getCurrentTime
            -- Change status of all the unspent coins
            _ <- forM unspent $ \(Entity ci _) -> 
                update ci [DbCoinStatus =. status]
            -- Insert orphaned coins
            orphans <- catMaybes <$> mapM (dbImportOrphan status) unknown
            -- Import new coins and update existing coins
            outCoins <- catMaybes <$> 
                (mapM (dbImportCoin tid complete) $ zip (txOut tx) [0..])
            let accTxs = buildAccTx tx ((map entityVal inCoins) ++ orphans)
                            outCoins (not complete) time
            -- Insert transaction blob. Ignore if already exists
            _ <- insertUnique $ DbTxBlob tid (encode' tx) time
            -- insert account transactions into database, 
            -- rewriting if they exist
            _ <- forM accTxs $ \accTx -> do
                prev <- getBy $ UniqueTx (dbTxTxid accTx) (dbTxAccount accTx)
                if isNothing prev 
                    then insert_ accTx  
                    else replace (entityKey $ fromJust prev) accTx
            -- Re-import parent transactions (transactions spending this one)
            if complete
                then do
                    let ids = nub $ catStatus $ map dbCoinStatus outCoins
                    blobs <- mapM dbGetTxBlob ids
                    reImported <- forM blobs $ (dbImportTx =<<) . dec
                    return $ accTxs ++ (concat reImported)
                else return accTxs
  where
    tid              = encodeTxid $ txid tx
    f (OutPoint h i) = CoinOutPoint (encodeTxid h) (fromIntegral i)
    complete         = isTxComplete tx
    status           = if complete then Spent tid else Reserved tid
    dec              = liftEither . decodeToEither . dbTxBlobValue . entityVal

-- |A transaction can not be imported if it double spends coins in the wallet.
-- Upstream code needs to remove the conflicting transaction first using
-- dbTxRemove function
isImportValid :: String -> [DbCoinGeneric b] -> Bool
isImportValid tid coins = all (f . dbCoinStatus) coins
  where
    f (Spent parent) = parent == tid
    f _              = True

-- When a transaction spends coins previously spent by a partial transaction,
-- we need to remove the partial transactions from the database and try to
-- re-import the transaction. Coins with Reserved status are spent by a partial
-- transaction.
txToRemove :: [DbCoinGeneric b] -> [String]
txToRemove coins = catMaybes $ map (f . dbCoinStatus) coins
  where
    f (Reserved parent) = Just parent
    f _                 = Nothing

-- |Group input and output coins by accounts and create 
-- account-level transaction
buildAccTx :: Tx -> [DbCoinGeneric b] -> [DbCoinGeneric b]
           -> Bool -> UTCTime -> [DbTxGeneric b]
buildAccTx tx inCoins outCoins partial time = map build $ M.toList oMap
  where
    iMap = foldr (f (\(i,o) x -> (x:i,o))) M.empty inCoins
    oMap = foldr (f (\(i,o) x -> (i,x:o))) iMap outCoins
    f g coin accMap = case M.lookup (dbCoinAccount coin) accMap of
        Just tuple -> M.insert (dbCoinAccount coin) (g tuple coin) accMap
        Nothing    -> M.insert (dbCoinAccount coin) (g ([],[]) coin) accMap
    allRecip = rights $ map toAddr $ txOut tx
    toAddr   = (addrToBase58 <$>) . scriptRecipient . scriptOutput
    sumVal   = sum . (map dbCoinValue)
    build (ai,(i,o)) = 
        DbTx (encodeTxid $ txid tx) recips total ai orphan partial time
      where
        orphan = or $ map dbCoinOrphan i
        total | orphan    = 0
              | otherwise = sumVal o - sumVal i
        addrs = map dbCoinAddress o
        recips | null addrs = allRecip
               | orphan     = allRecip \\ addrs -- assume this is an outgoing tx
               | total < 0  = allRecip \\ addrs -- remove the change
               | otherwise  = addrs

-- |Create an orphaned coin if the input spends from an address in the wallet
-- but the coin doesn't exist in the wallet. This allows out-of-order tx import
dbImportOrphan :: (PersistUnique m, PersistMonadBackend m ~ SqlBackend) 
               => CoinStatus -> TxIn 
               -> EitherT String m (Maybe (DbCoinGeneric SqlBackend))
dbImportOrphan status (TxIn (OutPoint h i) s _)
    | isLeft a  = return Nothing
    | otherwise = getBy (UniqueAddress b58) >>= \addrM -> case addrM of
        Nothing              -> return Nothing
        Just (Entity _ add) -> do
            time <- liftIO $ getCurrentTime
            rdm  <- dbGetRedeem add
            let coin = build rdm add time
            insert_ coin >> return (Just coin)
  where
    a   = scriptSender s
    b58 = addrToBase58 $ fromRight a
    build rdm add time = 
        DbCoin (encodeTxid h) (fromIntegral i) 0 "" rdm b58 status
               (dbAddressAccount add) True time

-- |Create a new coin for an output if it sends coins to an 
-- address in the wallet. Does not actually write anything to the database
-- if commit is False. This is for correctly reporting on partial transactions
-- without creating coins in the database from a partial transaction
dbImportCoin :: ( PersistQuery m, PersistUnique m
                , PersistMonadBackend m ~ SqlBackend
                )
             => String -> Bool -> (TxOut,Int) 
             -> EitherT String m (Maybe (DbCoinGeneric SqlBackend))
dbImportCoin tid commit ((TxOut v s), index) 
    | isLeft a  = return Nothing
    | otherwise = getBy (UniqueAddress b58) >>= \addrM -> case addrM of
        Nothing              -> return Nothing
        Just (Entity _ add) -> do
            time  <- liftIO getCurrentTime
            coinM <- getBy $ CoinOutPoint tid index
            case coinM of
                Just (Entity ci coin) -> do
                    -- Update existing coin with up to date information
                    let newCoin = coin{ dbCoinOrphan  = False
                                      , dbCoinValue   = (fromIntegral v)
                                      , dbCoinScript  = scp
                                      , dbCoinCreated = time
                                      }
                    when commit $ replace ci newCoin
                    return $ Just newCoin
                Nothing -> do
                    rdm <- dbGetRedeem add
                    let coin = build rdm add time
                    when commit $ insert_ coin
                    dbAdjustGap add -- Adjust address gap
                    return $ Just coin
  where
    a   = scriptRecipient s
    b58 = addrToBase58 $ fromRight a
    scp = bsToHex $ encodeScriptOps s
    build rdm add time = 
        DbCoin tid index (fromIntegral v) scp rdm b58 Unspent
               (dbAddressAccount add) False time
    
-- |Builds a redeem script given an address. Only relevant for addresses
-- linked to multisig accounts. Otherwise it returns Nothing
dbGetRedeem :: (PersistStore m, PersistMonadBackend m ~ SqlBackend) 
            => DbAddressGeneric SqlBackend -> EitherT String m (Maybe String)
dbGetRedeem add = do
    acc <- liftMaybe accErr =<< (get $ dbAddressAccount add)
    rdm <- if isMSAcc acc 
        then Just <$> liftMaybe rdmErr (getRdm acc)
        else return Nothing
    return $ bsToHex . encodeScriptOps . encodeOutput <$> rdm
  where
    getRdm acc = do
        key      <- loadPubAcc =<< (xPubImport $ dbAccountKey acc)
        msKeys   <- mapM xPubImport $ dbAccountMsKeys acc
        addrKeys <- f key msKeys $ fromIntegral $ dbAddressIndex add
        let pks = map (xPubKey . getAddrPubKey) addrKeys
        sortMulSig . (PayMulSig pks) <$> dbAccountMsRequired acc
      where
        f = if dbAddressInternal add then intMulSigKey else extMulSigKey
    accErr = "dbImportOut: Invalid address account"
    rdmErr = "dbGetRedeem: Could not generate redeem script"

-- |Build and sign a transactoin given a list of recipients
dbSendTx :: ( PersistUnique m, PersistQuery m
            , PersistMonadBackend m ~ SqlBackend
            )
         => AccountName -> [(String,Word64)] -> Word64
         -> EitherT String m (Tx, Bool)
dbSendTx name dests fee = do
    (coins,recips) <- dbSendSolution name dests fee
    dbSendCoins coins recips (SigAll False)

-- |Given a list of recipients and a fee, finds a valid combination of coins
dbSendSolution :: ( PersistUnique m, PersistQuery m
                  , PersistMonadBackend m ~ SqlBackend
                  )
               => AccountName -> [(String,Word64)] -> Word64
               -> EitherT String m ([Coin],[(String,Word64)])
dbSendSolution name dests fee = do
    (Entity ai acc) <- dbGetAcc name
    unspent <- liftEither . (mapM toCoin) =<< dbCoins ai
    (coins,change) <- liftEither $ if isMSAcc acc
        then let msParam = ( fromJust $ dbAccountMsRequired acc
                           , fromJust $ dbAccountMsTotal acc
                           )
             in chooseMSCoins tot fee msParam unspent
        else chooseCoins tot fee unspent
    recips <- if change < 5000 then return dests else do
        cAddr <- dbGenIntAddrs name 1
        return $ dests ++ [(dbAddressBase58 $ head cAddr,change)]
    return (coins,recips)
  where
    tot = sum $ map snd dests
    
-- | Build and sign a transaction by providing coins and recipients
dbSendCoins :: PersistUnique m
            => [Coin] -> [(String,Word64)] -> SigHash
            -> EitherT String m (Tx, Bool)
dbSendCoins coins recipients sh = do
    tx <- liftEither $ buildAddrTx (map coinOutPoint coins) recipients
    ys <- mapM (dbGetSigData sh) coins
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return (bsTx, isComplete sigTx)

dbSignTx :: PersistUnique m
         => AccountName -> Tx -> SigHash -> EitherT String m (Tx, Bool)
dbSignTx name tx sh = do
    (Entity ai _) <- dbGetAcc name
    coins <- catMaybes <$> (mapM (getBy . f) $ map prevOutput $ txIn tx)
    -- Filter coins for this account only
    let accCoins = filter ((== ai) . dbCoinAccount . entityVal) coins
    ys <- forM accCoins g
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return (bsTx, isComplete sigTx)
  where
    f (OutPoint h i) = CoinOutPoint (encodeTxid h) (fromIntegral i)
    g = ((dbGetSigData sh) =<<) . liftEither . toCoin . entityVal

-- |Given a coin, retrieves the necessary data to sign a transaction
dbGetSigData :: PersistUnique m
             => SigHash -> Coin -> EitherT String m (SigInput,PrvKey)
dbGetSigData sh coin = do
    (Entity _ w) <- dbGetWallet "main"
    a   <- liftEither $ scriptRecipient out
    (Entity _ add) <- dbGetAddr $ addrToBase58 a
    acc  <- liftMaybe accErr =<< get (dbAddressAccount add)
    mst <- liftMaybe mstErr $ loadMasterKey =<< xPrvImport (dbWalletMaster w)
    aKey <- liftMaybe prvErr $ accPrvKey mst $ fromIntegral $ dbAccountIndex acc
    let g = if dbAddressInternal add then intPrvKey else extPrvKey
    sigKey <- liftMaybe addErr $ g aKey $ fromIntegral $ dbAddressIndex add
    return (sigi, xPrvKey $ getAddrPrvKey sigKey)
  where
    out    = scriptOutput $ coinTxOut coin
    rdm    = coinRedeem coin
    sigi | isJust rdm = SigInputSH out (coinOutPoint coin) (fromJust rdm) sh
         | otherwise  = SigInput out (coinOutPoint coin) sh
    mstErr = "dbGetSigData: Could not load master key"
    accErr = "dbGetSigData: Could not load address account"
    prvErr = "dbGetSigData: Invalid account derivation index"
    addErr = "dbGetSigData: Invalid address derivation index"

data RawTxOutPoints = RawTxOutPoints [OutPoint] 
    deriving (Eq, Show)

data RawTxDests = RawTxDests [(String,Word64)]
    deriving (Eq, Show)

instance Json.FromJSON RawTxOutPoints where
    parseJSON = Json.withArray "Expected: Array" $ \arr -> do
        RawTxOutPoints <$> (mapM f $ V.toList arr)
      where
        f = Json.withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: T.pack "txid" :: Parser String
            vout <- obj .: T.pack "vout" :: Parser Word32
            let i = maybeToEither ("Failed to decode txid" :: String)
                                  (decodeTxid tid)
                o = OutPoint <$> i <*> (return vout)
            either (const mzero) return o

instance Json.FromJSON RawTxDests where
    parseJSON = Json.withObject "Expected: Object" $ \obj ->
        RawTxDests <$> (mapM f $ H.toList obj)
      where
        f (add,v) = do
            amnt <- parseJSON v :: Parser Word64
            return (T.unpack add, amnt)

data RawSigInput = RawSigInput [(SigHash -> SigInput)]

data RawPrvKey = RawPrvKey [PrvKey]
    deriving (Eq, Show)

instance Json.FromJSON RawSigInput where
    parseJSON = Json.withArray "Expected: Array" $ \arr -> do
        RawSigInput <$> (mapM f $ V.toList arr)
      where
        f = Json.withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: T.pack "txid" :: Parser String
            vout <- obj .: T.pack "vout" :: Parser Word32
            scp  <- obj .: T.pack "scriptPubKey" :: Parser String
            rdm  <- obj .:? T.pack "scriptRedeem" :: Parser (Maybe String)
            let s = decodeScriptOps =<< maybeToEither "Hex parsing failed" 
                        (hexToBS scp)
                i = maybeToEither "Failed to decode txid" (decodeTxid tid)
                o = OutPoint <$> i <*> (return vout)
                r = decodeScriptOps =<< maybeToEither "Hex parsing failed" 
                        (hexToBS $ fromJust rdm)
                res | isJust rdm = SigInputSH <$> s <*> o <*> r
                    | otherwise  = SigInput <$> s <*> o
            either (const mzero) return res

instance Json.FromJSON RawPrvKey where
    parseJSON = Json.withArray "Expected: Array" $ \arr ->
        RawPrvKey <$> (mapM f $ V.toList arr)
      where
        f v = do
            str <- parseJSON v :: Parser String  
            maybe mzero return $ fromWIF str

